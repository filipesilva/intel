(ns filipesilva.intel
  (:require
   [babashka.fs :as fs]
   [babashka.pods :as pods]
   [clojure.string :as str]))

(pods/load-pod 'clj-kondo/clj-kondo "2025.07.26")
(pods/load-pod 'huahaiy/datalevin "0.10.7")

(require '[pod.borkdude.clj-kondo :as clj-kondo]
         '[pod.huahaiy.datalevin :as d])

(def db-dir ".intel/db")

(def schema {:sym  {:db/unique      :db.unique/identity
                    :db/valueType   :db.type/symbol
                    :db/cardinality :db.cardinality/one}
             :uses {:db/valueType   :db.type/ref
                    :db/cardinality :db.cardinality/many}})

(defn var-usage->ent [{:keys [name from from-var to]}]
  {:sym  (if from-var
           (symbol (str from) (str from-var))
           (symbol (str from)))
   :uses {:sym (symbol (str to) (str name))}})

(defn open-conn []
  (fs/create-dirs db-dir)
  (d/get-conn db-dir schema))

(defn analyse [paths]
  (when (fs/exists? db-dir)
    (fs/delete-tree db-dir))
  (let [conn (open-conn)
        ents (->> (clj-kondo/run! {:lint paths :config {:analysis true}})
                  :analysis
                  :var-usages
                  (map var-usage->ent))]
    (d/transact! conn ents)
    (d/close conn)
    (count ents)))

(defn- pull-syms [attr sym]
  (let [conn (open-conn)
        res  (try
               (d/pull (d/db conn) [{attr [:sym]}] [:sym sym])
               (finally (d/close conn)))]
    (->> (get res attr)
         (keep :sym)
         sort)))

(defn deps [sym] (pull-syms :uses sym))
(defn _deps [sym] (pull-syms :_uses sym))

(defn- print-syms [syms]
  (doseq [s syms] (println s)))

(defn -main [& args]
  (case (first args)
    "analyse"
    (let [paths (or (seq (rest args))
                    (filter fs/exists? ["src" "test"]))]
      (when-not (seq paths)
        (println "no paths to analyse")
        (System/exit 1))
      (println "analysing" (str/join " " paths))
      (println "transacted" (analyse paths) "var-usages into" db-dir))

    "deps"
    (print-syms (deps (symbol (second args))))

    "_deps"
    (print-syms (_deps (symbol (second args))))

    (do (println "usage:")
        (println "  intel analyse [PATH...]   lint paths (default: src test) into" db-dir)
        (println "  intel deps SYM            symbols that SYM uses")
        (println "  intel _deps SYM           symbols that use SYM")
        (System/exit 1))))

;; TODO:
;; - figure out useful queries
;; - figure out a nice cli interface
;; - try datalevin storage
;;   - 1 db for each git sha
;;   - use HEAD for current
;;   - can I make a stored db on demand? by sha or branch name?
;;   - think so, just do storage on .intel/refs/
;; - opts
;;   - --depth 0
;;   - --depth ...
;;   - --count
;;   - --filter clojure.core
;;   - --filter datascript.*
;;   - --remove datascript.*
;;   - --private true
;; - express "is used by test ns"
;;   - intel deps filipesilva.foo/test --depth ... --filter filipesilva.*
;; - express "changed between commits"
;;   - intel deps --diff abc1234
;;   - intel deps --sha def5678 --diff abc1234
;; - express "this var uses a lot of stuff", and "a lot of stuff is used by this var"
;;   - intel deps foo/bar --depth ... --count
;;   - intel _deps foo/bar --depth ... --count
;; - express "top vars by uses"
;;   - intel top
;;   - intel _top
;;   - --limit 5 --depth ...
;;   - maybe there's no top, just --count, because `intel deps foo.*` needs to return multiple stuff anyway
;; - get info about if var is public or private
;;   - I don't think I want this, at least for a while
;;   - it doesn't matter much if a var is used by a public or private var
;; - compare datalevin vs datahike
