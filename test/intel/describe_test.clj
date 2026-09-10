(ns intel.describe-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [edamame.core :as e]
            [intel.describe :as describe]))

;; clj-kondo resolves symbols to fq vars by position; the tests mimic that
;; with a resolution map applied to every positioned symbol in the form.

(defn- usage-at [form resolves]
  (let [acc (volatile! {})]
    (walk/postwalk
     (fn [x]
       (when (symbol? x)
         (when-let [fq (resolves x)]
           (when-let [{:keys [row col]} (meta x)]
             (vswap! acc assoc [row col] fq))))
       x)
     form)
    @acc))

(def ^:private core-resolves
  '{when clojure.core/when, when-not clojure.core/when-not, if clojure.core/if
    let clojure.core/let, doseq clojure.core/doseq, case clojure.core/case
    and clojure.core/and, or clojure.core/or, map clojure.core/map
    zero? clojure.core/zero?, name clojure.core/name, = clojure.core/=
    -> clojure.core/->, defn clojure.core/defn, defn- clojure.core/defn-
    cond-> clojure.core/cond->, if-let clojure.core/if-let
    when-let clojure.core/when-let, defmethod clojure.core/defmethod})

(defn- describe-str [src resolves & [sym]]
  (let [form (e/parse-string src {:all true})]
    (describe/render (assoc (describe/describe-form form (usage-at form (merge core-resolves resolves)))
                            :sym (or sym 'my.ns/f)))))

(deftest sketch-test
  (testing "the reference example renders exactly as specified"
    (is (= (str "harbormaster.tasks.hosting-resources/initialize-resources\n"
                "  \"Initializes all resources for dev\"\n"
                "  [_conn]\n"
                "  ? harbormaster.config/dev?\n"
                "    harbormaster.config/env-str\n"
                "    ? * toucan.db/count\n"
                "        ? honeysql.core/call\n"
                "          taoensso.timbre/info\n"
                "          toucan.db/insert!\n"
                "          medley.core/assoc-some\n")
           (describe-str
            "(defn- initialize-resources
  \"Initializes all resources for dev\"
  [_conn]
  (when (config/dev?)
    (let [region       (config/env-str :aws-region)
          dev-defaults {:k8s []}]
      (doseq [[provider resources] dev-defaults
              resource             resources]
        (when (zero?
               (tdb/count HostingSharedResource
                          {:where [:= :provider (hsql/call :cast (name provider))]}))
          (log/info \"initial insert\")
          (tdb/insert! HostingSharedResource
                       (-> {:provider provider}
                           (mc/assoc-some :details (:details resource)))))))))"
            '{config/dev?    harbormaster.config/dev?
              config/env-str harbormaster.config/env-str
              tdb/count      toucan.db/count
              tdb/insert!    toucan.db/insert!
              hsql/call      honeysql.core/call
              log/info       taoensso.timbre/info
              mc/assoc-some  medley.core/assoc-some}
            'harbormaster.tasks.hosting-resources/initialize-resources)))))

(deftest hidden-guard-test
  (testing "a conditional with no visible guard var marks its body vars"
    (is (= "my.ns/f\n  [x]\n  ? my.ns/g\n"
           (describe-str "(defn f [x] (when (= x 1) (g)))"
                         '{g my.ns/g})))))

(deftest loop-siblings-test
  (testing "every var entering the loop is marked, nesting is not implied"
    (is (= "my.ns/f\n  []\n  * my.ns/g\n  * my.ns/h\n"
           (describe-str "(defn f [] (doseq [x xs] (g) (h)))"
                         '{g my.ns/g, h my.ns/h})))))

(deftest if-branches-test
  (testing "then and else both nest under the guard"
    (is (= "my.ns/f\n  []\n  ? my.ns/g\n    my.ns/h\n    my.ns/i\n"
           (describe-str "(defn f [] (if (g) (h) (i)))"
                         '{g my.ns/g, h my.ns/h, i my.ns/i})))))

(deftest and-chain-test
  (testing "and chains guards; the body hangs off the deepest one"
    (is (= "my.ns/f\n  []\n  ? my.ns/g\n    ? my.ns/h\n      my.ns/i\n"
           (describe-str "(defn f [] (when (and (g) (h)) (i)))"
                         '{g my.ns/g, h my.ns/h, i my.ns/i})))))

(deftest case-test
  (testing "case constants are not usages; branch exprs are guarded by the dispatch"
    (is (= "my.ns/f\n  [x]\n  ? my.ns/g\n    my.ns/h\n    my.ns/i\n"
           (describe-str "(defn f [x] (case (g x) :a (h) (i)))"
                         '{g my.ns/g, h my.ns/h, i my.ns/i})))))

(deftest hof-test
  (testing "the fn argument of a core HOF runs in a loop, the collection does not"
    (is (= "my.ns/f\n  [xs]\n  * my.ns/g\n  my.ns/h\n"
           (describe-str "(defn f [xs] (map g (h xs)))"
                         '{g my.ns/g, h my.ns/h})))))

(deftest def-and-multi-arity-test
  (testing "def has no argvec line"
    (is (= "my.ns/x\n  my.ns/g\n"
           (describe-str "(def x (g))" '{g my.ns/g} 'my.ns/x))))
  (testing "each arity renders its argvec and tree"
    (is (= "my.ns/f\n  [x]\n  my.ns/g\n  [x y]\n  my.ns/h\n"
           (describe-str "(defn f ([x] (g x)) ([x y] (h x y)))"
                         '{g my.ns/g, h my.ns/h})))))

(deftest external-test
  (is (= "toucan.db/count\n  external, source not analysed\n"
         (describe/render {:sym 'toucan.db/count :external true}))))

(deftest threading-test
  (testing "a conditional threaded through -> splits its clauses correctly"
    (is (= "my.ns/f\n  [x]\n  ? my.ns/p\n    my.ns/g\n"
           (describe-str "(defn f [x] (-> x (cond-> (p) (g))))"
                         '{p my.ns/p, g my.ns/g}))))
  (testing "case threaded through -> keeps all branches"
    (is (= "my.ns/f\n  [x]\n  ? my.ns/g\n  ? my.ns/h\n"
           (describe-str "(defn f [x] (-> x (case :a (g) (h))))"
                         '{g my.ns/g, h my.ns/h})))))

(deftest binding-defaults-test
  (testing "destructuring :or defaults inside special binders are kept"
    (is (= "my.ns/f\n  [m]\n  ? my.ns/h\n    my.ns/g\n    my.ns/i\n"
           (describe-str "(defn f [m] (if-let [{:keys [x] :or {x (g)}} (h m)] (i x)))"
                         '{g my.ns/g, h my.ns/h, i my.ns/i})))))

(deftest for-when-gates-test
  (testing ":when gates the body per iteration"
    (is (= "my.ns/f\n  [xs]\n  ? * my.ns/p?\n      my.ns/h\n"
           (describe-str "(defn f [xs] (doseq [x xs :when (p? x)] (h x)))"
                         '{p? my.ns/p?, h my.ns/h})))))

(deftest defmethod-dispatch-test
  (testing "the dispatch value is evaluated at definition time"
    (is (= "my.ns/area\n  my.ns/shape-kind\n  [s]\n  my.ns/g\n"
           (describe-str "(defmethod area shape-kind [s] (g s))"
                         '{shape-kind my.ns/shape-kind, g my.ns/g}
                         'my.ns/area)))))

(deftest malformed-binder-test
  (testing "reader-valid but malformed binders degrade to a plain walk"
    (is (= "my.ns/f\n  []\n  my.ns/g\n"
           (describe-str "(defn f [] (when-let x (g)))" '{g my.ns/g})))
    (is (= "my.ns/f\n  []\n  my.ns/g\n"
           (describe-str "(defn f [] (doseq x (g)))" '{g my.ns/g})))))

(deftest stale-render-test
  (is (= "my.ns/f\n  source moved since analyse; run: intel analyse\n"
         (describe/render {:sym 'my.ns/f :stale true}))))
