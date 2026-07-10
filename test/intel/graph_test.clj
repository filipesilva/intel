(ns intel.graph-test
  (:require [clojure.test :refer [deftest is testing]]
            [intel.graph :as graph]))

(deftest rel-file-test
  (let [base "/repo"]
    (testing "every clj-kondo spelling normalizes to the git-relative path"
      (is (= "src/foo.clj" (graph/rel-file base "src/foo.clj")))
      (is (= "src/foo.clj" (graph/rel-file base "./src/foo.clj")))
      (is (= "src/foo.clj" (graph/rel-file base "/repo/src/foo.clj"))))))

(deftest matcher-test
  (testing "globs"
    (is ((graph/matcher "myapp.*") 'myapp.orders/place-order))
    (is ((graph/matcher "myapp.*") 'myapp.orders))
    (is (not ((graph/matcher "myapp.*") 'other.ns/thing)))
    (is ((graph/matcher "*/validate") 'myapp.orders/validate))
    (is ((graph/matcher "myapp.orders/place-?rder") 'myapp.orders/place-order)))
  (testing "bare namespace matches its vars"
    (is ((graph/matcher "clojure.core") 'clojure.core/map))
    (is ((graph/matcher "clojure.core") 'clojure.core))
    (is (not ((graph/matcher "clojure.core") 'clojure.core.async/go))))
  (testing "regex specials in syms are literal"
    (is ((graph/matcher "my.ns/str+") 'my.ns/str+))
    (is (not ((graph/matcher "my.ns/str+") 'my.ns/strr)))))

(deftest bfs-test
  (let [adj {'a #{'b 'c} 'b #{'d} 'd #{'a}}]
    (testing "depth 1"
      (is (= #{'b 'c} (graph/bfs adj ['a] 1))))
    (testing "depth 2"
      (is (= #{'b 'c 'd} (graph/bfs adj ['a] 2))))
    (testing "unbounded is cycle-safe and excludes seeds"
      (is (= #{'b 'c 'd} (graph/bfs adj ['a] 0)))
      (is (= #{'a 'b 'c} (graph/bfs adj ['d] 0))))
    (testing "multiple seeds union, seeds excluded"
      (is (= #{'c 'd} (graph/bfs adj ['a 'b] 1))))))

(deftest shortest-path-test
  (let [adj {'a #{'b 'x} 'b #{'c} 'x #{'y} 'y #{'c}}]
    (is (= '[a b c] (graph/shortest-path adj 'a 'c)))
    (is (= '[a] (graph/shortest-path adj 'a 'a)))
    (is (nil? (graph/shortest-path adj 'c 'a)))))

(def diff-sample
  "diff --git a/src/foo.clj b/src/foo.clj
index 111..222 100644
--- a/src/foo.clj
+++ b/src/foo.clj
@@ -10,2 +12,3 @@ (defn foo+2 []
+(defn changed
+  []
+  :x)
@@ -30 +40 @@
+(def other :y)
diff --git a/src/gone.clj b/src/gone.clj
deleted file mode 100644
--- a/src/gone.clj
+++ /dev/null
@@ -1,5 +0,0 @@
diff --git a/src/bar.clj b/src/bar.clj
--- a/src/bar.clj
+++ b/src/bar.clj
@@ -7,3 +7,0 @@
diff --git a/my dir/x.clj b/my dir/x.clj
--- a/my dir/x.clj\t
+++ b/my dir/x.clj\t
@@ -1 +1 @@
")

(deftest parse-hunks-test
  (let [hunks (graph/parse-hunks diff-sample)]
    (testing "new-side ranges, count defaulting to 1"
      (is (= [[12 14] [40 40]] (hunks "src/foo.clj"))))
    (testing "deleted files are skipped"
      (is (nil? (hunks "src/gone.clj"))))
    (testing "pure deletion keeps a 1-line range"
      (is (= [[7 7]] (hunks "src/bar.clj"))))
    (testing "trailing tab on a spaced path is stripped"
      (is (= [[1 1]] (hunks "my dir/x.clj"))))))

(deftest parse-hunks-content-not-header-test
  (testing "an added source line rendered as +++ is not mistaken for a header"
    (let [diff (str "diff --git a/src/foo.clj b/src/foo.clj\n"
                    "--- a/src/foo.clj\n"
                    "+++ b/src/foo.clj\n"
                    "@@ -1 +2 @@\n"
                    "+++ counter thing\n"
                    "@@ -20 +40 @@\n"
                    "+(def other :y)\n")
          hunks (graph/parse-hunks diff)]
      (is (= ["src/foo.clj"] (keys hunks)))
      (is (= [[2 2] [40 40]] (hunks "src/foo.clj"))))))

(deftest changed-syms-test
  (let [vars [{:sym 'foo/a :file "src/foo.clj" :row 10 :end-row 13}
              {:sym 'foo/b :file "src/foo.clj" :row 20 :end-row 25}
              {:sym 'bar/c :file "src/bar.clj" :row 12 :end-row 14}]]
    (is (= '[foo/a]
           (graph/changed-syms vars {"src/foo.clj" [[12 14]]})))
    (is (= '[]
           (vec (graph/changed-syms vars {"src/foo.clj" [[14 19]]}))))
    (is (= '[bar/c]
           (graph/changed-syms vars {"src/bar.clj" [[1 100]]})))))

(deftest rescue-method-usages-test
  ;; clj-kondo gives each marker usage its own :row/:end-row spanning the whole
  ;; form, so spans are exact and trailing top-level code cannot leak in.
  (let [u (fn [m] (merge {:filename "src/app.clj" :from 'app} m))
        usages
        [;; defn body usage, already attributed to its var
         (u {:from-var 'handler :to 'app :name 'render :name-row 2 :name-col 3})
         ;; (defmethod compile ::a [x] (helper x)) spanning rows 4-5
         (u {:to 'clojure.core :name 'defmethod :row 4 :end-row 5 :name-row 4 :name-col 2})
         (u {:to 'app :name 'compile :name-row 4 :name-col 13})
         (u {:to 'app :name 'helper :name-row 5 :name-col 4})
         ;; (defmethod compile ::b [x] (thing x)) spanning rows 6-7
         (u {:to 'clojure.core :name 'defmethod :row 6 :end-row 7 :name-row 6 :name-col 2})
         (u {:to 'app :name 'compile :name-row 6 :name-col 13})
         (u {:to 'other.ns :name 'thing :name-row 7 :name-col 4})
         ;; (defrecord Widget [] Proto (m [_] (helper))) spanning rows 8-10
         (u {:to 'clojure.core :name 'defrecord :row 8 :end-row 10 :name-row 8 :name-col 2})
         (u {:to 'app :name 'helper :name-row 10 :name-col 6})
         ;; a top-level call after the last marker, with no def after it
         (u {:to 'app :name 'boot! :name-row 12 :name-col 2})]
        var-defs [{:ns 'app :name 'Widget :row 8 :end-row 10
                   :defined-by 'clojure.core/defrecord :filename "src/app.clj"}
                  {:ns 'app :name '->Widget :row 8 :end-row 10
                   :defined-by 'clojure.core/defrecord :filename "src/app.clj"}]
        {:keys [usages spans]} (graph/rescue-method-usages usages var-defs)
        from-sym (fn [row name]
                   (:intel/from-sym
                    (first (filter #(and (= row (:name-row %)) (= name (:name %)))
                                   usages))))]
    (testing "defmethod body attributes to the multimethod"
      (is (= 'app/compile (from-sym 5 'helper)))
      (is (= 'app/compile (from-sym 7 'thing))))
    (testing "defrecord body attributes to the type var"
      (is (= 'app/Widget (from-sym 10 'helper))))
    (testing "defn bodies and top-level calls outside marker forms are untouched"
      (is (nil? (from-sym 2 'render)))
      (is (nil? (from-sym 12 'boot!))))
    (testing "spans are the marker form extents"
      (is (= [['app/compile 4 5] ['app/compile 6 7] ['app/Widget 8 10]]
             (map (juxt :sym :row :end-row) (sort-by :row spans)))))))

(deftest rescue-cljc-duplicate-markers-test
  ;; a cljc file yields clj and cljs twins of each marker at the same position;
  ;; spans must dedupe so the db does not double its method-body entities.
  (let [u (fn [m] (merge {:filename "src/app.cljc" :from 'app} m))
        usages [(u {:to 'clojure.core :name 'defmethod :row 3 :end-row 4 :name-row 3 :name-col 2})
                (u {:to 'cljs.core    :name 'defmethod :row 3 :end-row 4 :name-row 3 :name-col 2})
                (u {:to 'app :name 'compile :name-row 3 :name-col 13})
                (u {:to 'app :name 'helper  :name-row 4 :name-col 4})]
        {:keys [spans]} (graph/rescue-method-usages usages [])]
    (is (= [['app/compile 3 4]]
           (map (juxt :sym :row :end-row) spans)))))

(deftest untested-vars-test
  (let [attrs {'app/covered   {:file "f" :private false :test false}
               'app/uncovered {:file "f" :private false :test false}
               'app/deep      {:file "f" :private false :test false}
               'app/priv      {:file "f" :private true :test false}
               'app/->R       {:file "f" :private false :test false
                               :defined-by 'clojure.core/defrecord}
               'app-test/t    {:file "t" :private false :test true}}
        out   {'app-test/t #{'app/covered}
               'app/covered #{'app/deep}}]
    (is (= '[app/uncovered]
           (vec (graph/untested-vars attrs out {}))))
    (is (= '[app/->R app/uncovered]
           (sort (graph/untested-vars attrs out {:generated true}))))))

(deftest dead-vars-test
  (let [attrs {'app/used    {:file "src/app.clj" :private false :test false}
               'app/unused  {:file "src/app.clj" :private false :test false}
               'app/tested  {:file "src/app.clj" :private false :test false}
               'app/-main   {:file "src/app.clj" :private false :test false}
               'app/priv    {:file "src/app.clj" :private true :test false}
               'app-test/t  {:file "test/app_test.clj" :private false :test true}
               'app-test    {:file "test/app_test.clj" :private false :test true}
               'clojure.core/map {}}
        in    {'app/used   #{'app/-main}
               'app/tested #{'app-test/t 'app-test}}]
    (is (= {'app/unused :unused
            'app/tested :test-only}
           (into {} (map (juxt :sym :class)) (graph/dead-vars attrs in {}))))))
