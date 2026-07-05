(ns filipesilva.intel
  "Analyse your Clojure code with Datalog."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [filipesilva.intel.db :as db]
            [filipesilva.intel.graph :as graph]))

(def help
  "intel: analyse your Clojure code with Datalog

usage:
  intel analyse [PATH...]      rebuild .intel/db (default paths: src test)
  intel ls [PATTERN...]        list project vars
  intel deps SEED... [opts]    what these vars use
  intel _deps SEED... [opts]   what uses these vars
  intel path FROM TO           shortest dependency path between two vars
  intel diff [REF]             vars changed vs git REF (default HEAD)
  intel affected [REF]         vars whose behaviour the changes vs REF reach;
                               add --test for the tests to run
  intel dead [PATTERN...]      public vars unused, or used only by tests
  intel untested [PATTERN...]  public vars no test reaches transitively
  intel q [QUERY|-] [ARG...]   raw datalog query, - reads it from stdin
                               (also: -f FILE, --rules FILE)
  intel schema                 schema, rules, and example queries

SEED is a var sym, a glob pattern, a bare ns (the ns and its vars), or -
to read syms from stdin. Quote patterns; ? and * are shell globs too.

options (deps, _deps, affected; filters also on ls, dead, untested):
  -d, --depth N     traversal depth, 0 = unbounded
                    (default: 1 for deps/_deps, 0 for affected)
      --count       per seed, print seed and closure size, sorted desc
      --filter G    keep only syms matching glob (repeatable)
      --remove G    drop syms matching glob (repeatable)
      --test        keep only test vars
      --no-test     drop test vars
      --seeds       include seeds themselves in output
      --ext         include external vars (clojure.core, libs; cljc repos
                    list clj and cljs twins separately)
      --generated   include defrecord/deftype generated vars (dead, untested)
  -l, --long        append file:row and private/test flags
      --edn         EDN output

globs: * matches anything, ? one char; a bare namespace matches its vars.
exit codes: 0 results, 1 no results, 2 error.
notes: analysis is static; deleted vars are invisible to diff, and vars only
referenced inside syntax-quoted macro bodies can appear unused.

recipes:
  intel affected origin/master --test                    tests a PR can change
  intel affected origin/master --test | sed 's|/.*||'|sort -u   ...as namespaces
  intel affected origin/master                            PR blast radius
  intel _deps 'myapp.*' --count | head                   most-used vars
  intel deps 'myapp.*' -d 0 --count --no-test | head     biggest closures
  intel deps 'myapp.domain.*' -d 0 --filter 'myapp.web.*'  layering violations")

(def opt-spec
  {:depth     {:alias :d :coerce :long}
   :count     {:coerce :boolean}
   :filter    {:coerce []}
   :remove    {:coerce []}
   :test      {:coerce :boolean}
   :no-test   {:coerce :boolean}
   :ext       {:coerce :boolean}
   :seeds     {:coerce :boolean}
   :generated {:coerce :boolean}
   :long      {:alias :l :coerce :boolean}
   :edn       {:coerce :boolean}
   :file      {:alias :f}
   :rules     {}
   :help      {:alias :h :coerce :boolean}})

(defn- fail! [msg]
  (throw (ex-info msg {:intel/exit 2})))

;; output

(defn- flags [attrs sym]
  (let [a (attrs sym)]
    (cond-> []
      (:private a) (conj "private")
      (:test a)    (conj "test"))))

(defn- loc [attrs sym]
  (let [{:keys [file row]} (attrs sym)]
    (when file (str file ":" row))))

(defn- long-cols
  "The --long trailer: file:row (or empty) plus private/test flags."
  [attrs sym]
  (into [(or (loc attrs sym) "")] (flags attrs sym)))

(defn- sym-row [attrs sym {:keys [long]}]
  (into [(str sym)]
        (when long (long-cols attrs sym))))

(defn- sym-map [attrs sym]
  (into {:sym sym} (select-keys (attrs sym) [:file :row :private :test])))

(defn- print-rows!
  "rows: seq of vectors of strings. Returns exit code."
  [rows]
  (doseq [r rows]
    (println (str/join "\t" r)))
  (if (seq rows) 0 1))

(defn- print-edn! [xs]
  (prn (vec xs))
  (if (seq xs) 0 1))

;; seeds and filters

(def ^:private stdin-seed
  "Stands in for a - argument, which babashka.cli would treat as an
  options terminator."
  " stdin")

(defn- stdin-syms []
  (->> (line-seq (java.io.BufferedReader. *in*))
       (map #(first (str/split % #"\t")))
       (map str/trim)
       (remove str/blank?)
       (map symbol)))

(defn- resolve-seeds [graph args stdin]
  (let [known?   (:attrs graph)
        resolve1 (fn [arg]
                   (cond
                     (= stdin-seed arg)
                     (mapv (fn [s]
                             (when-not (known? s) (fail! (str "unknown sym: " s)))
                             s)
                           stdin)

                     ;; an exact qualified sym wins over glob reading, so a
                     ;; predicate name like my.ns/valid? is itself, not a ? glob
                     (and (str/includes? arg "/") (known? (symbol arg)))
                     [(symbol arg)]

                     (graph/pattern? arg)
                     (let [ms (filter (graph/matcher arg) (keys known?))]
                       (when (empty? ms) (fail! (str "no match: " arg)))
                       ms)

                     ;; a bare namespace means the ns entity and its vars
                     (not (str/includes? arg "/"))
                     (let [all (filter (graph/matcher arg) (keys known?))]
                       (when (empty? all) (fail! (str "unknown sym: " arg)))
                       all)

                     :else
                     (let [sym (symbol arg)]
                       (when-not (known? sym) (fail! (str "unknown sym: " arg)))
                       [sym])))]
    (distinct (mapcat resolve1 args))))

(defn- arg-filter
  "Keep syms matching any PATTERN arg, or everything when no args are given."
  [args]
  (if (seq args) (graph/any-matcher args) (constantly true)))

(defn- result-filter
  "Post-traversal filter from --filter/--remove/--test/--no-test/--ext.
  With :skip-ext, externals pass regardless (for seed rows in count mode)."
  [attrs {:keys [filter remove test no-test ext skip-ext]}]
  (let [keep?   (if (seq filter) (graph/any-matcher filter) (constantly true))
        drop?   (if (seq remove) (graph/any-matcher remove) (constantly false))
        select? (fn [sym]
                  (let [a (attrs sym)]
                    (and (or ext skip-ext (:file a))
                         (or (not test) (:test a))
                         (or (not no-test) (not (:test a)))
                         (keep? sym)
                         (not (drop? sym)))))]
    select?))

(defn- emit-syms
  "Print a plain sym list as TSV rows or EDN. The shared output shape of every
  command whose result is just vars."
  [attrs syms opts]
  (if (:edn opts)
    (print-edn! (map #(sym-map attrs %) syms))
    (print-rows! (map #(sym-row attrs % opts) syms))))

(defn- emit-closure
  "Traverse adj from seeds and print the result, honouring --count/--seeds and
  the shared filters. opts must carry a resolved :depth."
  [attrs adj seeds opts]
  (let [select? (result-filter attrs opts)
        closure (fn [from] (filter select? (graph/bfs adj from (:depth opts))))]
    (if (:count opts)
      (let [seed? (result-filter attrs (assoc opts :skip-ext true))
            rows  (->> (filter seed? seeds)
                       (map (fn [s] {:sym s :count (count (closure [s]))}))
                       (sort-by (juxt (comp - :count) (comp str :sym))))]
        (if (:edn opts)
          (print-edn! rows)
          (print-rows! (map (fn [{:keys [sym count]}] [(str sym) (str count)]) rows))))
      (emit-syms attrs
                 (-> (set (closure seeds))
                     (into (when (:seeds opts) (filter select? seeds)))
                     (->> (sort-by str)))
                 opts))))

;; commands

(defn- cmd-analyse [args _opts]
  (let [paths (or (seq args) (seq (filter fs/exists? ["src" "test"])))]
    (when-not paths
      (fail! "no paths to analyse"))
    (let [start (System/currentTimeMillis)
          {:keys [namespaces vars edges]} (db/analyse! paths)
          secs  (/ (- (System/currentTimeMillis) start) 1000.0)]
      (binding [*out* *err*]
        (println (format "analysed %s: %d namespaces, %d vars, %d edges (%.1fs)"
                         (str/join " " paths) namespaces vars edges secs))))
    0))

(defn- cmd-ls [args opts]
  (let [{:keys [attrs]} (db/with-db db/load-graph)
        keep? (arg-filter args)
        syms  (->> (keys attrs)
                   (filter #(and (namespace %) (:file (attrs %))))
                   (filter keep?)
                   (filter (result-filter attrs (assoc opts :ext true)))
                   (sort-by str))]
    (emit-syms attrs syms opts)))

(defn- cmd-deps [direction args opts]
  (when-not (seq args)
    (fail! "deps needs at least one SEED"))
  (let [stdin (when (some #{stdin-seed} args) (doall (stdin-syms)))
        g     (db/with-db db/load-graph)
        seeds (resolve-seeds g args stdin)]
    (emit-closure (:attrs g) (g direction) seeds
                  (update opts :depth #(or % 1)))))

(defn- cmd-path [args _opts]
  (when-not (= 2 (count args))
    (fail! "path needs FROM and TO"))
  (let [g (db/with-db db/load-graph)
        [from to] (map symbol args)]
    (doseq [s args]
      (when-not ((:attrs g) (symbol s))
        (fail! (str "unknown sym: " s))))
    (if-let [path (graph/shortest-path (:out g) from to)]
      (print-rows! (map (fn [s] [(str s)]) path))
      1)))

(defn- git! [& cmd]
  (let [{:keys [exit out err]} (apply p/sh {:continue true} cmd)]
    (when-not (zero? exit)
      (fail! (str/trim err)))
    out))

(defn- gather-graph
  "One locked read of everything the diff-based commands need."
  []
  (db/with-db (fn [d]
                {:meta  (db/meta-ent d)
                 :graph (db/load-graph d)
                 :spans (db/method-spans d)})))

(defn- diff-vars
  "Vars whose definitions overlap the git diff of the analysed paths vs ref.
  Reuses the already-loaded meta/attrs/method-spans and runs git itself."
  [{:keys [sha paths]} attrs spans ref]
  (let [head (db/git-sha)]
    (when (and sha head (not= sha head))
      (binding [*out* *err*]
        (println (str "warning: db analysed at " (subs sha 0 7)
                      ", HEAD is " (subs head 0 7) "; run intel analyse"))))
    ;; pin the output format so user gitconfig (mnemonicPrefix, external diff
    ;; drivers, quotepath) cannot silently break hunk parsing, and --relative
    ;; makes paths cwd-relative to match the db's :file when intel runs from a
    ;; subdirectory of the repo.
    (let [out    (apply git! "git" "-c" "core.quotepath=false"
                        "diff" "--no-ext-diff" "--relative"
                        "--src-prefix=a/" "--dst-prefix=b/" "-U0" "-M" ref "--"
                        (or (seq paths) ["."]))
          ranges (graph/parse-hunks out)
          vars   (concat (for [[sym a] attrs :when (:file a)] (assoc a :sym sym))
                         spans)]
      (sort-by str (distinct (graph/changed-syms vars ranges))))))

(defn- cmd-diff [args opts]
  (let [{:keys [meta graph spans]} (gather-graph)
        syms (diff-vars meta (:attrs graph) spans (or (first args) "HEAD"))]
    (emit-syms (:attrs graph) syms opts)))

(defn- cmd-affected [args opts]
  (let [{:keys [meta graph spans]} (gather-graph)
        changed (diff-vars meta (:attrs graph) spans (or (first args) "HEAD"))]
    (emit-closure (:attrs graph) (:in graph) changed
                  (assoc opts :depth (or (:depth opts) 0) :seeds true))))

(defn- cmd-dead [args opts]
  (let [{:keys [attrs in]} (db/with-db db/load-graph)
        keep? (arg-filter args)
        select? (result-filter attrs (assoc opts :ext true))
        dead  (->> (graph/dead-vars attrs in opts)
                   (filter #(keep? (:sym %)))
                   (filter #(select? (:sym %)))
                   (sort-by (comp str :sym)))]
    (if (:edn opts)
      (print-edn! (map (fn [{:keys [sym class]}]
                         (assoc (sym-map attrs sym) :class class))
                       dead))
      (print-rows! (map (fn [{:keys [sym class]}]
                          (into [(str sym) (name class)]
                                (when (:long opts) (long-cols attrs sym))))
                        dead)))))

(defn- cmd-untested [args opts]
  (let [{:keys [attrs out]} (db/with-db db/load-graph)
        keep? (arg-filter args)
        select? (result-filter attrs (assoc opts :ext true))
        syms  (->> (graph/untested-vars attrs out opts)
                   (filter keep?)
                   (filter select?)
                   (sort-by str))]
    (emit-syms attrs syms opts)))

;; datalog escape hatch

(defn- schema-page []
  (str "attributes (entities are vars; bare ns syms hold top-level code):\n"
       "  :sym         fq var sym or bare ns sym, unique\n"
       "  :ns          namespace as a symbol (externals included)\n"
       "  :file        source path; on analysed vars and method-body spans\n"
       "  :row :end-row  definition line span\n"
       "  :private     boolean\n"
       "  :test        boolean; deftest, -test ns, or under a test/ path\n"
       "  :defined-by  clojure.core/defn, defmacro, ...\n"
       "  :uses        ref, many; var -> var it uses\n"
       "  :method-of   ref; defmethod/extend body span -> its anchor var\n"
       "\nrules (bound to % by default):\n"
       (str/join "\n" (map #(str "  " %) db/rules))
       "\n\nnotes:\n"
       "- match booleans via predicate, [?v :private ?p] [(false? ?p)];\n"
       "  a literal false in a value position acts as a wildcard\n"
       "- bind symbols via :in arguments, not quoted literals in predicates\n"
       "- recursive rules inside not-join can blow up on big graphs;\n"
       "  prefer intel untested for test-reach questions\n"
       "\nexamples:\n"
       "  # public non-test vars never used outside their own ns\n"
       "  intel q '[:find ?s :where [?v :defined-by _] [?v :private ?p] [(false? ?p)]\n"
       "            [?v :test ?t] [(false? ?t)] [?v :sym ?s] [?v :ns ?n]\n"
       "            (not-join [?v ?n] [?u :uses ?v] [?u :ns ?m] [(not= ?n ?m)])]'\n"
       "  # namespace coupling: cross-ns edges\n"
       "  intel q '[:find ?na ?nb (count ?v) :where\n"
       "            [?u :uses ?v] [?u :ns ?na] [?v :ns ?nb] [(not= ?na ?nb)]]'\n"
       "  # everything that transitively uses a var\n"
       "  intel q '[:find ?s :in $ % ?root :where\n"
       "            [?r :sym ?root] (used+ ?r ?u) [?u :sym ?s]]' my.app/thing"))

(defn- read-query [args opts]
  (cond
    (:file opts)                  (edn/read-string (slurp (:file opts)))
    (= stdin-seed (first args))   (edn/read-string (slurp *in*))
    (seq args)                    (edn/read-string (first args))))

(defn- q-in-syms
  "The query's :in symbols, guaranteeing $ and % lead so the db and rules are
  bound even when the user supplies their own :in."
  [query]
  (let [in (if (map? query)
             (:in query)
             (->> query (drop-while #(not= :in %)) rest
                  (take-while #(not (#{:find :where :with :keys} %)))))]
    (into '[$ %] (remove '#{$ %} in))))

(defn- ensure-in
  "Set the query's :in clause to in-syms, placed right after :find for vector
  queries (replacing any existing :in)."
  [query in-syms]
  (if (map? query)
    (assoc query :in in-syms)
    (let [sections #{:find :in :where :with :keys}
          strip-in (fn [q]
                     (let [[before after] (split-with #(not= :in %) q)]
                       (if (seq after)
                         (concat before (drop-while #(not (sections %)) (rest after)))
                         before)))
          cleaned  (strip-in (vec query))
          [head tail] (split-with #(not (#{:where :with :keys} %)) cleaned)]
      (vec (concat head [:in] in-syms tail)))))

(defn- cmd-q [args opts]
  (if (and (empty? args) (not (:file opts)))
    (do (println (schema-page)) 0)
    (let [query   (read-query args opts)
          qargs   (if (:file opts) args (rest args))
          rules   (if (:rules opts)
                    (into db/rules (edn/read-string (slurp (:rules opts))))
                    db/rules)
          in-syms (q-in-syms query)
          query   (ensure-in query in-syms)
          rows    (db/with-db
                    (fn [d]
                      (let [inputs (loop [[s & more] in-syms, qargs qargs, acc []]
                                     (if-not s
                                       acc
                                       (case s
                                         $ (recur more qargs (conj acc d))
                                         % (recur more qargs (conj acc rules))
                                         (do (when-not (seq qargs)
                                               (fail! (str "no argument for :in " s)))
                                             (recur more (rest qargs)
                                                    (conj acc (edn/read-string (first qargs))))))))]
                        (sort-by pr-str (apply db/q query inputs)))))]
      (if (and (seq rows) (every? #(and (= 1 (count %)) (symbol? (first %))) rows))
        (print-rows! (map (fn [[s]] [(str s)]) rows))
        (print-rows! (map (fn [r] (map pr-str r)) rows))))))

(defn- cmd-schema [_args _opts]
  (println (schema-page))
  (when (fs/exists? db/dir)
    (let [{:keys [attrs out]} (db/with-db db/load-graph)
          syms     (keys attrs)
          nses     (remove namespace syms)
          internal (filter #(:file (attrs %)) syms)]
      (println)
      (println (format "db: %d internal vars, %d namespaces, %d external vars, %d edges"
                       (- (count internal) (count nses))
                       (count nses)
                       (- (count syms) (count internal))
                       (reduce + (map count (vals out)))))))
  0)

;; main

(defn -main [& argv]
  (try
    (let [argv (replace {"-" stdin-seed} (vec argv))
          {:keys [args opts]} (cli/parse-args argv {:spec opt-spec :restrict true})
          [cmd & args] args
          code (if (or (:help opts) (nil? cmd))
                   (do (println help) (if (:help opts) 0 2))
                   (case cmd
                     ("analyse" "analyze") (cmd-analyse args opts)
                     "ls"     (cmd-ls args opts)
                     "deps"   (cmd-deps :out args opts)
                     "_deps"  (cmd-deps :in args opts)
                     "path"     (cmd-path args opts)
                     "diff"     (cmd-diff args opts)
                     "affected" (cmd-affected args opts)
                     "dead"     (cmd-dead args opts)
                     "untested" (cmd-untested args opts)
                     "q"        (cmd-q args opts)
                     "schema" (cmd-schema args opts)
                     "help"   (do (println help) 0)
                     (do (binding [*out* *err*]
                           (println (str "unknown command: " cmd)))
                         (println help)
                         2)))]
      (System/exit code))
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println (str "intel: " (first (str/split-lines (or (ex-message e) "error"))))))
      (System/exit (or (:intel/exit (ex-data e)) 2)))
    (catch Exception e
      (binding [*out* *err*]
        (println (str "intel: " (first (str/split-lines (or (ex-message e) (str e)))))))
      (System/exit 2))))
