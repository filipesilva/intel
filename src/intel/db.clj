(ns intel.db
  "Datalevin storage and clj-kondo analysis."
  (:require [babashka.fs :as fs]
            [babashka.pods :as pods]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [intel.graph :as graph]))

(pods/load-pod 'huahaiy/datalevin "0.10.7")
(require '[pod.huahaiy.datalevin :as d])

(def dir ".intel/db")

(def schema
  {:sym         {:db/unique      :db.unique/identity
                 :db/valueType   :db.type/symbol
                 :db/cardinality :db.cardinality/one}
   :ns          {:db/valueType :db.type/symbol :db/cardinality :db.cardinality/one}
   :file        {:db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   :row         {:db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   :end-row     {:db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   :private     {:db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
   :test        {:db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
   :defined-by  {:db/valueType :db.type/symbol :db/cardinality :db.cardinality/one}
   :uses        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :method-of   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   :intel/sha   {:db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   :intel/paths {:db/valueType :db.type/string :db/cardinality :db.cardinality/one}})

(def rules
  '[[(uses ?a ?b) [?a :uses ?b]]
    [(uses+ ?a ?b) [?a :uses ?b]]
    [(uses+ ?a ?b) [?a :uses ?x] (uses+ ?x ?b)]
    [(used+ ?a ?b) (uses+ ?b ?a)]
    [(internal ?v) [?v :sym _] [?v :file _]]])

(def ^:private lock-file ".intel/lock")

(defn- with-lock
  "Serialize db access across intel processes with an exclusive file lock.
  Datalevin writes to its txn-log on every open, so concurrent opens (e.g.
  intel diff | intel _deps -) corrupt the db without this. The lock releases
  when the file closes, including on process death."
  [f]
  (fs/create-dirs (fs/parent lock-file))
  (let [raf (java.io.RandomAccessFile. (str lock-file) "rw")]
    (.lock (.getChannel raf))
    (try
      (f)
      (finally (.close raf)))))

(defn- complete?
  "The :intel/paths meta datom is transacted last, so its presence proves the
  rebuild finished. A db interrupted mid-analyse opens but lacks it."
  [db]
  (some? (ffirst (d/q '[:find ?p :where [_ :intel/paths ?p]] db))))

(defn with-db
  "Open the db under the lock, pass it to f, and return f's result. Callers
  must not touch stdin or stdout inside f: a process blocked on a pipe while
  holding the lock deadlocks the other end of the pipe."
  [f]
  (binding [*out* *err*]
    (println (str "db: " (fs/absolutize dir))))
  (with-lock
    (fn []
      ;; checked under the lock so a concurrent analyse's delete/create
      ;; window cannot make a valid db look absent
      (when-not (fs/exists? dir)
        (throw (ex-info (str "no db at " (fs/absolutize dir) "; run: intel analyse")
                        {:intel/exit 2})))
      (let [conn (d/get-conn dir)
            db   (d/db conn)]
        (try
          (when-not (complete? db)
            (throw (ex-info "db incomplete (analyse was interrupted); run: intel analyse"
                            {:intel/exit 2})))
          (f db)
          (finally (d/close conn)))))))

(defn q [query & inputs]
  (apply d/q query inputs))

;; analyse

(defn git-sha []
  ;; :continue tolerates a non-zero exit but not a missing git binary, which
  ;; throws; the sha is optional, so absorb both.
  (try
    (let [{:keys [exit out]} (p/sh {:continue true} "git" "rev-parse" "HEAD")]
      (when (zero? exit) (str/trim out)))
    (catch Exception _ nil)))

(defn- test-var? [{:keys [test ns filename]}]
  (boolean (or test
               (some-> ns str (str/ends-with? "-test"))
               (some->> filename (re-find #"(^|/)test/")))))

(defn- def->ent [d]
  (cond-> {:sym     (graph/fq (:ns d) (:name d))
           :ns      (:ns d)
           :file    (:filename d)
           :private (boolean (:private d))
           :test    (test-var? d)}
    (:row d)        (assoc :row (:row d))
    (:end-row d)    (assoc :end-row (:end-row d))
    (:defined-by d) (assoc :defined-by (:defined-by d))))

(defn- ns-def->ent [d]
  (cond-> {:sym     (:name d)
           :ns      (:name d)
           :file    (:filename d)
           :private false
           :test    (test-var? {:ns (:name d) :filename (:filename d)})}
    (:row d)     (assoc :row (:row d))
    (:end-row d) (assoc :end-row (:end-row d))))

(defn- pick-def
  "A var can have several definition records (declare + defn, clj + cljs
  branches of cljc). Prefer a real definition over declare."
  [defs]
  (or (first (remove #(str/ends-with? (str (:defined-by %)) "declare") defs))
    (first defs)))

(defn- usage->edge [{:keys [name from from-var to] :as u}]
  [(or (:intel/from-sym u)
       (if from-var (graph/fq from from-var) from))
   (graph/fq to name)])

(defn- sym->ns-ent [s]
  {:sym s :ns (if-let [n (namespace s)] (symbol n) s)})

(def ^:private kondo-config
  {:analysis      true
   :skip-comments true
   :output        {:format :json}
   :lint-as       {'clojure.test.check.clojure-test/defspec
                   'clojure.test/deftest}})

(def ^:private symbol-keys [:ns :name :to :from :from-var :defined-by])

(defn- symbolize [m]
  (reduce (fn [m k] (if-let [v (m k)] (assoc m k (symbol v)) m)) m symbol-keys))

(defn- kondo-analysis
  "clj-kondo :analysis for paths, via the binary's JSON output. The pod
  speaks EDN, and a var named like /-clause (metabase has one) prints as a
  token no EDN reader accepts; JSON carries symbols as strings."
  [paths]
  (let [bin  (or (fs/which "clj-kondo")
                 (throw (ex-info "clj-kondo not found on PATH; install it: https://github.com/clj-kondo/clj-kondo/blob/master/doc/install.md"
                                 {:intel/exit 2})))
        proc (p/process (vec (concat [(str bin) "--lint"] paths
                                     ["--parallel" "--config" (pr-str kondo-config)]))
                        {:err :string})
        out  (with-open [r (io/reader (:out proc))]
               (json/parse-stream r true))
        {:keys [exit err]} @proc]
    ;; 2 and 3 mean findings, which analysis does not care about
    (when-not (contains? #{0 2 3} exit)
      (throw (ex-info (str "clj-kondo failed: " (str/trim (str err))) {:intel/exit 2})))
    (-> (:analysis out)
        (update :var-definitions #(map symbolize %))
        (update :var-usages #(map symbolize %))
        (update :namespace-definitions #(map symbolize %)))))

(defn file-usages
  "clj-kondo var-usage positions for one file: {[row col] -> fq-sym}.
  Every usage is included, clojure.core macros too, so callers can both
  resolve symbols and recognize control forms by position."
  [file]
  (let [usages (:var-usages (kondo-analysis [(str file)]))]
    (reduce (fn [m u]
              (let [pos [(:name-row u) (:name-col u)]
                    fq  (graph/fq (:to u) (:name u))
                    cur (m pos)]
                ;; cljc twins share positions; prefer the clj resolution
                (if (or (nil? cur)
                        (and (str/starts-with? (str cur) "cljs.")
                             (not (str/starts-with? (str fq) "cljs."))))
                  (assoc m pos fq)
                  m)))
            {}
            (filter #(and (symbol? (:to %)) (:name-row %)) usages))))

(defn analyse!
  "Rebuild the db from clj-kondo analysis of paths. Returns summary counts."
  [paths]
  (let [{:keys [var-definitions var-usages namespace-definitions]} (kondo-analysis paths)
        ;; clj-kondo echoes the lint-path spelling in :filename; git diff
        ;; --relative reports cwd-relative, so normalize both to match.
        base     (fs/cwd)
        norm     (fn [xs] (map #(cond-> % (:filename %)
                                        (update :filename (partial graph/rel-file base)))
                               xs))
        var-definitions       (norm var-definitions)
        var-usages            (norm var-usages)
        namespace-definitions (norm namespace-definitions)
        usable   (filter #(and (symbol? (:to %)) (symbol? (:from %))) var-usages)
        {:keys [usages spans]} (graph/rescue-method-usages usable var-definitions)
        edges    (->> usages
                      (map usage->edge)
                      (remove (fn [[a b]] (= a b)))
                      distinct)
        defs     (->> (group-by (juxt :ns :name) var-definitions)
                      vals
                      (map pick-def))
        sha      (git-sha)
        ents     (concat
                  (for [[a b] edges] {:sym a :uses [{:sym b}]})
                  (map sym->ns-ent (distinct (mapcat identity edges)))
                  (map def->ent defs)
                  (map ns-def->ent namespace-definitions)
                  (for [{:keys [sym file row end-row]} spans]
                    {:method-of {:sym sym} :file file :row row :end-row end-row})
                  [(cond-> {:intel/paths (pr-str (vec paths))}
                     sha (assoc :intel/sha sha))])]
    (with-lock
      (fn []
        (when (fs/exists? dir)
          (fs/delete-tree dir))
        (fs/create-dirs dir)
        (let [conn (d/get-conn dir schema)]
          (try
            (doseq [batch (partition-all 10000 ents)]
              (d/transact! conn (vec batch)))
            (finally (d/close conn))))))
    {:namespaces (count (distinct (map :name namespace-definitions)))
     :vars       (count defs)
     :edges      (count edges)}))

;; bulk reads

(defn method-spans
  "Row spans of defmethod/extend-*/defrecord bodies, attributed to their
  anchor var."
  [db]
  (map (fn [[s f r er]] {:sym s :file f :row r :end-row er})
       (q '[:find ?s ?f ?r ?er
            :where
            [?m :method-of ?v] [?v :sym ?s]
            [?m :file ?f] [?m :row ?r] [?m :end-row ?er]]
          db)))

(defn meta-ent [db]
  (let [paths (ffirst (q '[:find ?p :where [?e :intel/paths ?p]] db))]
    {:paths (when paths (edn/read-string paths))
     :sha   (ffirst (q '[:find ?s :where [?e :intel/sha ?s]] db))}))

(defn load-graph
  "Whole graph in memory:
  {:out {sym #{sym}} :in {sym #{sym}} :attrs {sym {:file :row ...}}}."
  [db]
  (let [edges (q '[:find ?sa ?sb
                   :where [?a :uses ?b] [?a :sym ?sa] [?b :sym ?sb]]
                 db)
        ents  (map first
                   (q '[:find (pull ?e [:sym :file :row :end-row :private :test :defined-by])
                        :where [?e :sym _]]
                      db))]
    {:out   (reduce (fn [m [a b]] (update m a (fnil conj #{}) b)) {} edges)
     :in    (reduce (fn [m [a b]] (update m b (fnil conj #{}) a)) {} edges)
     :attrs (into {} (for [e ents] [(:sym e) (dissoc e :sym)]))}))
