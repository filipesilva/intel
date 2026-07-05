(ns filipesilva.intel-test
  (:require [clojure.test :refer [deftest is testing]]
            [filipesilva.intel :as intel]))

(def ^:private resolve-seeds @#'intel/resolve-seeds)
(def ^:private q-in-syms @#'intel/q-in-syms)
(def ^:private ensure-in @#'intel/ensure-in)
(def ^:private stdin-seed @#'intel/stdin-seed)

(def ^:private graph
  {:attrs {'my.ns/valid?  {:file "f"}
           'my.ns/valids  {:file "f"}
           'my.ns/other   {:file "f"}
           'app.core/foo  {:file "f"}
           'app.core/bar  {:file "f"}
           'app.core      {:file "f"}
           'clojure.core/map {}}})

(defn- seeds [args stdin]
  (set (doall (resolve-seeds graph args stdin))))

(deftest resolve-seeds-exact-test
  (testing "an exact qualified sym resolves to itself"
    (is (= #{'my.ns/other} (seeds ["my.ns/other"] nil))))
  (testing "a known predicate name is exact, not a ? glob (no overmatch)"
    (is (= #{'my.ns/valid?} (seeds ["my.ns/valid?"] nil)))))

(deftest resolve-seeds-glob-test
  (testing "a glob matches its members"
    (is (= #{'app.core/foo 'app.core/bar} (seeds ["app.core/*"] nil))))
  (testing "a glob matching nothing is an error, not a silent empty result"
    (is (thrown? clojure.lang.ExceptionInfo (seeds ["my.ns/vaild?"] nil)))
    (is (thrown? clojure.lang.ExceptionInfo (seeds ["no.such/*"] nil)))))

(deftest resolve-seeds-bare-ns-test
  (testing "a bare namespace is the ns entity plus its vars"
    (is (= #{'app.core 'app.core/foo 'app.core/bar} (seeds ["app.core"] nil))))
  (testing "an unknown bare namespace is an error"
    (is (thrown? clojure.lang.ExceptionInfo (seeds ["nope"] nil)))))

(deftest resolve-seeds-stdin-test
  (testing "known stdin syms pass through"
    (is (= #{'my.ns/other 'app.core/foo}
           (seeds [stdin-seed] '[my.ns/other app.core/foo]))))
  (testing "an unknown stdin sym is an error, matching direct-arg behavior"
    (is (thrown? clojure.lang.ExceptionInfo
                 (seeds [stdin-seed] '[my.ns/other renamed.ns/gone])))))

(deftest q-in-syms-test
  (testing "$ and % are guaranteed and lead"
    (is (= '[$ %] (q-in-syms '[:find ?s :where [?e :sym ?s]])))
    (is (= '[$ % ?n] (q-in-syms '[:find ?s :in ?n :where [?e :ns ?n]])))
    (is (= '[$ % ?x] (q-in-syms '[:find ?s :in $ ?x :where [?e :sym ?x]])))
    (is (= '[$ % ?r] (q-in-syms {:find '[?s] :in '[$ % ?r]})))))

(deftest ensure-in-test
  (testing "a vector query gets :in after :find"
    (is (= '[:find ?s :in $ % :where [?e :sym ?s]]
           (ensure-in '[:find ?s :where [?e :sym ?s]] '[$ %]))))
  (testing "an existing :in is replaced in place"
    (is (= '[:find ?s :in $ % ?n :where [?e :ns ?n]]
           (ensure-in '[:find ?s :in ?n :where [?e :ns ?n]] '[$ % ?n]))))
  (testing "a map query sets :in directly"
    (is (= {:find '[?s] :in '[$ % ?n]}
           (ensure-in {:find '[?s]} '[$ % ?n])))))
