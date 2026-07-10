(ns intel.graph
  "Pure functions over the var graph: globs, traversal, diff hunks, dead vars."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

;; paths

(defn rel-file
  "Normalize a clj-kondo filename to a base-relative path. clj-kondo echoes the
  lint path spelling (./src/x, /abs/src/x, src/x); git diff --relative reports
  cwd-relative. Both must agree or diff hunks never match stored :file values."
  [base filename]
  (str (fs/relativize base (fs/path base filename))))

;; globs

(defn glob->re [glob]
  (-> glob
      (str/replace #"[.+^$()\[\]{}|\\]" "\\\\$0")
      (str/replace "*" ".*")
      (str/replace "?" ".")
      (->> (format "^%s$"))
      re-pattern))

(defn matcher
  "Predicate over sym strings for one glob. A bare namespace (no / * ?)
  also matches every var in that namespace."
  [glob]
  (let [re      (glob->re glob)
        bare-ns (not (re-find #"[/*?]" glob))]
    (fn [s]
      (let [s (str s)]
        (boolean (or (re-matches re s)
                     (and bare-ns (str/starts-with? s (str glob "/")))))))))

(defn any-matcher [globs]
  (let [ms (mapv matcher globs)]
    (fn [s] (boolean (some #(% s) ms)))))

(defn pattern? [s]
  (boolean (re-find #"[*?]" (str s))))

;; traversal

(defn bfs
  "Syms reachable from seeds via adj, up to depth hops. 0 = unbounded.
  Excludes the seeds themselves."
  [adj seeds depth]
  (loop [frontier (set seeds)
         seen     (set seeds)
         acc      #{}
         hops     0]
    (if (or (empty? frontier)
            (and (pos? depth) (= hops depth)))
      acc
      (let [next (set (remove seen (mapcat #(adj % #{}) frontier)))]
        (recur next (into seen next) (into acc next) (inc hops))))))

(defn shortest-path
  "Shortest path from a to b via adj as a vector of syms, nil if none."
  [adj a b]
  (if (= a b)
    [a]
    (loop [frontier [a]
           parent   {a nil}]
      (when (seq frontier)
        (let [step (for [n frontier
                         m (adj n #{})
                         :when (not (contains? parent m))]
                     [m n])
              parent (into parent step)]
          (if (contains? parent b)
            (->> (iterate parent b) (take-while some?) reverse vec)
            (recur (distinct (map first step)) parent)))))))

;; git diff hunks

(defn parse-hunks
  "Parse `git diff -U0` output into {file [[start end] ...]} of new-side
  line ranges. Pure deletions keep a 1-line range at the deletion point."
  [diff-out]
  (first
   (reduce
    (fn [[m file header?] line]
      (cond
        ;; each file's header block opens with a diff --git line; the real
        ;; new-side path comes from the +++ line inside it. Gating on the block
        ;; stops an added line like "++ x" (rendered "+++ x" under -U0) from
        ;; hijacking the current file.
        (str/starts-with? line "diff --git ")
        [m nil true]

        (and header? (str/starts-with? line "+++ "))
        [m (let [p (subs line 4)]
             (when-not (= p "/dev/null")
               ;; git appends a tab after a header path that contains spaces
               (-> p (str/replace #"^b/" "") (str/replace #"\t$" ""))))
         false]

        (and file (str/starts-with? line "@@"))
        (if-let [[_ start cnt] (re-find #"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@" line)]
          (let [start (parse-long start)
                cnt   (if cnt (parse-long cnt) 1)
                range (if (zero? cnt)
                        [(max 1 start) (max 1 start)]
                        [start (+ start cnt -1)])]
            [(update m file (fnil conj []) range) file header?])
          [m file header?])

        :else [m file header?]))
    [{} nil false]
    (str/split-lines diff-out))))

(defn changed-syms
  "Syms whose row..end-row span overlaps a changed range in their file.
  vars: seq of {:sym :file :row :end-row}."
  [vars ranges-by-file]
  (for [{:keys [sym file row end-row]} vars
        :when (and file row)
        :let [ranges (ranges-by-file file)]
        :when (some (fn [[s e]] (and (<= s (or end-row row)) (<= row e)))
                    ranges)]
    sym))

;; method bodies

(def ^:private marker-names
  '#{defmethod extend-protocol extend-type extend defrecord deftype definterface})

(def ^:private marker-nses '#{clojure.core cljs.core})

(defn fq [ns name] (symbol (str ns) (str name)))

(defn- pos [u] [(:name-row u) (:name-col u)])

(defn rescue-method-usages
  "clj-kondo attaches usages inside defmethod, extend-*, defrecord and
  deftype bodies to the namespace, with no from-var. Re-attribute each such
  usage to an anchor var: the multimethod, the protocol, or the type.
  Returns {:usages usages-with-:intel/from-sym-added
           :spans [{:sym anchor :file f :row r :end-row er}]}."
  [usages var-defs]
  (let [ns-level  (fn [u] (and (nil? (:from-var u)) (:name-row u)))
        marker?   (fn [u] (and (ns-level u)
                               (contains? marker-names (:name u))
                               (contains? marker-nses (:to u))))
        by-file   (group-by :filename usages)
        defs-by-f (group-by :filename var-defs)
        rescued
        (for [[file us] by-file
              :let [markers  (filter marker? us)
                    defs     (defs-by-f file)
                    type-def (fn [row]
                               (first (for [d defs
                                            :when (and (= row (:row d))
                                                       (not (re-find #"^(->|map->)" (str (:name d)))))]
                                          (fq (:ns d) (:name d)))))]
              m markers
              ;; clj-kondo gives the marker usage the whole form's :row/:end-row,
              ;; so the body is exactly what falls inside it. Nothing after the
              ;; form (a following def or a bare top-level call) can leak in.
              :let [start (:row m)
                    end   (:end-row m)
                    body  (filter (fn [u]
                                    (and (ns-level u)
                                         (not (marker? u))
                                         (symbol? (:to u))
                                         (pos? (compare (pos u) (pos m)))
                                         (<= (:name-row u) end)))
                                  us)
                    anchor (if ('#{defrecord deftype definterface} (:name m))
                             (type-def start)
                             (some->> (first (sort-by pos body))
                                      ((fn [u] (fq (:to u) (:name u))))))]
              :when anchor]
          {:anchor anchor :file file :row start :end-row end
           :body-keys (set (map pos body))})
        anchor-at (into {} (for [r rescued, k (:body-keys r)]
                             [[(:file r) k] (:anchor r)]))]
    {:usages (map (fn [u]
                    (if-let [a (and (nil? (:from-var u))
                                    (anchor-at [(:filename u) (pos u)]))]
                      (assoc u :intel/from-sym a)
                      u))
                  usages)
     :spans  (distinct
              (map (fn [{:keys [anchor file row end-row]}]
                     {:sym anchor :file file :row row :end-row end-row})
                   rescued))}))

;; dead and untested vars

(defn generated?
  "Vars synthesized by defrecord/deftype (the type var, ->Type, map->Type)."
  [a]
  (contains? #{"defrecord" "deftype"} (some-> (:defined-by a) name)))

(defn- candidate?
  "A public, non-test, project-defined var eligible for dead/untested analysis.
  Excludes -main and, unless generated, defrecord/deftype synthesized vars."
  [sym a generated]
  (and (:file a)
       (namespace sym)
       (not (:private a))
       (not (:test a))
       (not= "-main" (name sym))
       (or generated (not (generated? a)))))

(defn dead-vars
  "Public project vars nobody uses (:unused) or only test code uses
  (:test-only). Exempts test vars, -main, and generated record/type vars
  unless generated? is truthy in opts.
  attrs: {sym {:file :private :test ...}}, in-adj: {sym #{caller-sym}}."
  [attrs in-adj {:keys [generated]}]
  (for [[sym a] attrs
        :when (candidate? sym a generated)
        :let [callers (seq (in-adj sym))]
        :when (or (nil? callers)
                  (every? #(:test (attrs %)) callers))]
    {:sym sym :class (if callers :test-only :unused)}))

(defn untested-vars
  "Public non-test project vars that no test var reaches through the graph.
  Computed as a forward BFS from every test var, complemented."
  [attrs out-adj {:keys [generated]}]
  (let [test-syms (for [[sym a] attrs :when (:test a)] sym)
        reached   (into (set test-syms) (bfs out-adj test-syms 0))]
    (for [[sym a] attrs
          :when (and (candidate? sym a generated)
                     (not (contains? reached sym)))]
      sym)))
