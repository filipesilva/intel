(ns intel.describe
  "A var's body as a tree of the vars it uses, in source order. Conditional
  tests become ?-marked guard nodes that parent whatever they gate; crossing
  into a loop marks *. Symbols resolve by joining clj-kondo usage positions
  against edamame's parse, so aliases, refers, and shadowing follow
  clj-kondo's rules exactly."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [edamame.core :as e]
            [intel.db :as db]
            [org.httpkit.server :as http]))

;; walk: one parsed form -> nodes {:sym :cd :ld :guard? :children}
;; cd/ld count the invisible-conditional and loop constructs above a node;
;; guard? marks nodes that came from a conditional's test.

(def ^:private hidden-ns #{"clojure.core" "cljs.core"})

(def ^:private cond-heads
  '#{when when-not if if-not when-let if-let when-some if-some when-first
     cond condp case and or some-> some->> cond-> cond->>})

(def ^:private loop-heads
  '#{doseq for loop while dotimes})

(def ^:private thread-heads
  '#{-> ->> doto})

(def ^:private hof-heads
  "Core fns that call their first argument once per element."
  '#{map mapv mapcat pmap filter filterv remove keep keep-indexed map-indexed
     reduce reduce-kv run! some every? not-every? not-any? sort-by group-by
     partition-by take-while drop-while split-with repeatedly iterate
     update-vals update-keys})

(defn- resolve-at [{:keys [usage-at]} form]
  (let [{:keys [row col]} (meta form)]
    (usage-at [row col])))

(defn- core-head
  "The clojure.core name a list head resolves to, nil for anything else."
  [env head]
  (when (symbol? head)
    (when-let [fq (resolve-at env head)]
      (when (hidden-ns (namespace fq))
        (symbol (name fq))))))

(defn- usage-node [env form]
  (when-let [fq (resolve-at env form)]
    (when-not (hidden-ns (namespace fq))
      {:sym fq :cd (:cd env) :ld (:ld env)})))

(declare walk)

(defn- walk-all [env forms]
  (into [] (mapcat #(walk env %)) forms))

(defn- mark-guards [node]
  (-> node
      (assoc :guard? true)
      (update :children #(mapv mark-guards %))))

(defn- append-deepest
  "Hang tail under the last, deepest descendant of node."
  [node tail]
  (if (seq tail)
    (if-let [cs (seq (:children node))]
      (assoc node :children (conj (vec (butlast cs)) (append-deepest (last cs) tail)))
      (assoc node :children (vec tail)))
    node))

(defn- guard-chain
  "Chain guard nodes right to left and hang the branch nodes off the end,
  so each guard's subtree contains everything it gates."
  [guards branches]
  (->> (reverse guards)
       (reduce (fn [tail g] [(append-deepest (mark-guards g) tail)])
               (vec branches))))

(defn- guarded
  "One conditional clause: the vars in tests guard the vars in branches.
  With no visible guard var, branch vars mark themselves conditional."
  [env tests branches]
  (let [guards (walk-all env tests)]
    (if (seq guards)
      (guard-chain guards (walk-all env branches))
      (walk-all (update env :cd inc) branches))))

(defn- chained
  "and/or/some->: each step's vars guard all later steps."
  [env steps]
  (if-let [[s & more] (seq steps)]
    (if more
      (let [guards (walk env s)]
        (if (seq guards)
          (guard-chain guards (chained env more))
          (chained (update env :cd inc) more)))
      (walk env s))
    []))

(defn- thread-step
  "Reinsert a placeholder for the threaded argument of a conditional step,
  so its clauses split at the right positions. Other steps keep their
  shape; for them the missing argument changes nothing."
  [env step first?]
  (if (and (seq? step) (cond-heads (core-head env (first step))))
    (if first?
      (apply list (first step) ::threaded (rest step))
      (apply list (concat step [::threaded])))
    step))

(defn- walk-cond [env head [_ & args]]
  (case head
    (when when-not if if-not)
    (guarded env [(first args)] (rest args))

    (when-let if-let when-some if-some when-first)
    (let [[b & body] args]
      (if (vector? b)
        ;; binding forms walk too: destructuring :or defaults use vars
        (guarded env (take-nth 2 (rest b)) (concat (take-nth 2 b) body))
        (walk-all env args)))

    cond
    (into [] (mapcat (fn [[t b]] (guarded env [t] (when b [b]))))
          (partition-all 2 args))

    condp
    (guarded env (take 2 args) (drop 2 args))

    case
    (let [clauses  (rest args)
          pairs    (partition-all 2 clauses)
          branches (cond-> (into [] (keep #(when (= 2 (count %)) (second %))) pairs)
                     (odd? (count clauses)) (conj (last clauses)))]
      (guarded env [(first args)] branches))

    (and or)
    (chained env args)

    (some-> some->>)
    (chained env (cons (first args)
                       (map #(thread-step env % (= 'some-> head)) (rest args))))

    (cond-> cond->>)
    (into (vec (walk env (first args)))
          (mapcat (fn [[t s]]
                    (guarded env [t]
                             (when s [(thread-step env s (= 'cond-> head))]))))
          (partition-all 2 (rest args)))))

(defn- walk-thread [env head [_ x & steps]]
  (into (vec (walk env x))
        (mapcat #(walk env (thread-step env % (not= '->> head))))
        steps))

(defn- walk-loop [env head [_ & args]]
  (let [deeper (update env :ld inc)]
    (if (= 'while head)
      (guarded deeper [(first args)] (rest args))
      (let [[b & body] args]
        (if-not (vector? b)
          (walk-all env args)
          (case head
            (doseq for)
            (let [[p0 & ps] (partition-all 2 b)
                  gate?     (fn [[k]] (contains? #{:when :while} k))
                  tests     (map second (filter gate? ps))]
              (-> []
                  ;; the first binding form's defaults run per element, its
                  ;; init expr once; later pairs are per-iteration entirely
                  (into (walk-all deeper (take 1 p0)))
                  (into (walk-all env (rest p0)))
                  (into (walk-all deeper (apply concat (remove gate? ps))))
                  (into (if (seq tests)
                          (guarded deeper tests body)
                          (walk-all deeper body)))))

            loop
            (into (into [] (mapcat (fn [[form init]]
                                     (into (vec (walk deeper form)) (walk env init))))
                        (partition-all 2 b))
                  (walk-all deeper body))

            dotimes
            (into (vec (walk-all env [(second b)]))
                  (walk-all deeper body))))))))

(defn- walk [env form]
  (cond
    (symbol? form)
    (if-let [n (usage-node env form)] [n] [])

    (seq? form)
    (if (= 'quote (first form))
      []
      (let [ch (core-head env (first form))]
        (cond
          (cond-heads ch)   (walk-cond env ch form)
          (loop-heads ch)   (walk-loop env ch form)
          (thread-heads ch) (walk-thread env ch form)
          (hof-heads ch)    (into (vec (walk (update env :ld inc) (second form)))
                                  (walk-all env (drop 2 form)))
          :else             (walk-all env form))))

    (map? form)  (walk-all env (mapcat identity form))
    (coll? form) (walk-all env (seq form))
    :else        []))

(defn- flag-nodes
  "Turn cd/ld/guard? into final :cond/:loop flags, relative to each node's
  tree parent: nesting already communicates inherited guards."
  [nodes pcd pld]
  (mapv (fn [{:keys [sym cd ld guard? children]}]
          (cond-> {:sym sym}
            (or guard? (> cd pcd)) (assoc :cond true)
            (> ld pld)             (assoc :loop true)
            (seq children)         (assoc :children (flag-nodes children cd ld))))
        nodes))

;; def forms

(defn- fn-tails [xs]
  (cond
    (vector? (first xs))
    [{:argvec (first xs) :body (rest xs)}]

    (and (seq? (first xs)) (vector? (ffirst xs)))
    (for [[argvec & body] (filter seq? xs)]
      {:argvec argvec :body body})

    :else [{:body xs}]))

(def ^:private fn-defs #{"defn" "defn-" "defmacro" "defmethod" "deftest" "defspec"})

(defn- parse-def
  "Doc, argvecs, and bodies of one top-level def-like form. A defmethod's
  dispatch value is evaluated at definition time, so it contributes a body."
  [form]
  (let [head       (when (symbol? (first form)) (name (first form)))
        defmethod? (= "defmethod" head)
        xs         (drop 2 form)
        dispatch   (when defmethod? (first xs))
        xs         (if defmethod? (rest xs) xs)
        [doc xs]   (if (and (string? (first xs)) (next xs))
                     [(first xs) (rest xs)]
                     [nil xs])
        xs         (if (and (map? (first xs)) (next xs)) (rest xs) xs)]
    {:doc     doc
     :arities (into (if defmethod? [{:body [dispatch]}] [])
                    (if (contains? fn-defs head) (fn-tails xs) [{:body xs}]))}))

(defn describe-form
  "Pure core: doc and arity trees for one parsed def form, given
  clj-kondo usage positions as {[row col] -> fq-sym}."
  [form usage-at]
  (let [{:keys [doc arities]} (parse-def form)
        env {:usage-at usage-at :cd 0 :ld 0}]
    (cond-> {:arities (vec (for [{:keys [argvec body]} arities]
                             (cond-> {:nodes (flag-nodes (walk-all env body) 0 0)}
                               argvec (assoc :argvec argvec))))}
      doc (assoc :doc doc))))

;; render

(defn- render-nodes [nodes indent]
  (str/join
   (for [{:keys [sym children] :as n} nodes]
     (let [marks (str (when (:cond n) "? ") (when (:loop n) "* "))]
       (str (apply str (repeat indent " ")) marks sym "\n"
            (render-nodes children (+ indent (count marks))))))))

(defn render
  "The CLI page for one describe result."
  [{:keys [sym doc arities external stale note]}]
  (str sym "\n"
       (cond
         external "  external, source not analysed\n"
         stale    "  source moved since analyse; run: intel analyse\n"
         :else
         (str (when note (str "  " note "\n"))
              (when doc (str "  " (pr-str doc) "\n"))
              (str/join
               (for [{:keys [argvec nodes]} arities]
                 (str (when argvec (str "  " (pr-str argvec) "\n"))
                      (render-nodes nodes 2))))))))

;; describe: db graph + source files -> data

(def ^:private file-context
  "Parsed forms and clj-kondo usage positions per file. Memoized: a describe
  server assumes the checkout does not change under it."
  (memoize
   (fn [file]
     (try
       {:forms    (e/parse-string-all (slurp file)
                                      {:all          true
                                       :auto-resolve (fn [a] (symbol (str a)))
                                       :readers      (fn [_] identity)
                                       :read-cond    :allow
                                       :features     #{:clj}})
        :usage-at (db/file-usages file)}
       (catch Exception _ nil)))))

(defn- form-at [forms row]
  (some (fn [f]
          (let [{r :row er :end-row} (meta f)]
            (when (and r (<= r row er)) f)))
        forms))

(defn- def-name [form]
  (when (and (seq? form) (symbol? (second form)))
    (name (second form))))

(defn- names?
  "Does this top-level form define sym? Record and type vars also answer
  for their generated ->T and map->T names."
  [form sym]
  (when-let [n (def-name form)]
    (let [v (name sym)]
      (or (= n v) (= v (str "->" n)) (= v (str "map->" n))))))

(defn- primary-form
  "The form for the var's own definition site. The db row is a hint that
  goes stale as the file is edited, so prefer a form that actually names
  the var; a defprotocol at the row means sym is one of its methods."
  [forms sym row]
  (let [rowf (form-at forms row)]
    (cond
      (and rowf (names? rowf sym))
      {:form rowf}

      (some #(names? % sym) forms)
      {:form (first (filter #(names? % sym) forms))}

      (and rowf (= "defprotocol" (some-> (first rowf) name)))
      {:form rowf
       :note (str "protocol method of " (namespace sym) "/" (def-name rowf)
                  "; describe that var for implementation bodies")})))

(defn describe
  "Describe data for sym: {:sym :doc :arities [{:argvec :nodes}]}, with
  :external true when no analysed source covers it and :stale true when the
  analysed location no longer holds the var. Multimethod and protocol vars
  also describe each method body span."
  [{:keys [graph spans]} sym]
  (let [a        (get (:attrs graph) sym)
        fctx     (when (and (:file a) (:row a)) (file-context (:file a)))
        prim     (when fctx (primary-form (:forms fctx) sym (:row a)))
        prim-loc (when prim [(:file a) (:row (meta (:form prim)))])
        parts    (concat
                  (when prim
                    [(cond-> (describe-form (:form prim) (:usage-at fctx))
                       (:note prim) (assoc :note (:note prim)))])
                  (for [{:keys [file row]} (filter #(= sym (:sym %)) spans)
                        :when (not= [file row] prim-loc)
                        :let  [ctx  (file-context file)
                               form (when ctx (form-at (:forms ctx) row))]
                        :when form]
                    (describe-form form (:usage-at ctx))))]
    (cond
      (seq parts)
      (cond-> {:sym sym :arities (vec (mapcat :arities parts))}
        (some :doc parts)  (assoc :doc (some :doc parts))
        (some :note parts) (assoc :note (some :note parts)))

      (:file a) {:sym sym :stale true}
      :else     {:sym sym :external true})))

;; http: stacked panes, one per expansion step

(def ^:private page
  "<!doctype html>
<html><head>
<meta charset=utf-8>
<meta name=viewport content='width=device-width, initial-scale=1'>
<title>intel describe</title>
<style>
:root { --bg:#fff; --fg:#1a1a1a; --muted:#767676; --line:#e4e4e4;
        --accent:#6c4fd8; --cond:#b8860b; --loop:#2779bd; --sel:#f3f0fc; }
@media (prefers-color-scheme: dark) {
  :root { --bg:#17181c; --fg:#dcdde2; --muted:#8b8d94; --line:#2b2d33;
          --accent:#a08cf0; --cond:#d9a441; --loop:#5fa8e0; --sel:#232030; }
}
* { box-sizing:border-box }
body { margin:0; background:var(--bg); color:var(--fg); height:100vh;
       display:flex; flex-direction:column;
       font:14px/1.5 -apple-system, system-ui, sans-serif; }
header { padding:8px 14px; border-bottom:1px solid var(--line); flex:none;
         font-family:ui-monospace, SFMono-Regular, Menlo, monospace; }
header b { color:var(--accent); }
#panes { flex:1; display:flex; overflow-x:auto; overflow-y:hidden; }
.pane { flex:none; width:380px; border-right:1px solid var(--line); overflow-y:auto; }
.section { border-bottom:1px solid var(--line); }
.section > h2 { margin:0; padding:8px 12px; font-size:13px; cursor:pointer;
                position:sticky; top:0; background:var(--bg);
                font-family:ui-monospace, SFMono-Regular, Menlo, monospace; }
.section > h2:hover { color:var(--accent); }
.section.external > h2 { cursor:default; color:var(--muted); }
.section.external > h2:hover { color:var(--muted); }
.section.selected > h2 { background:var(--sel); color:var(--accent); }
.section .body { padding:2px 12px 10px; }
.doc { color:var(--muted); font-style:italic; margin:2px 0; }
.argvec { font-family:ui-monospace, SFMono-Regular, Menlo, monospace;
          color:var(--muted); margin:2px 0; }
.tree { font-family:ui-monospace, SFMono-Regular, Menlo, monospace;
        font-size:13px; white-space:pre; }
.tree .var { cursor:pointer; }
.tree .var:hover { text-decoration:underline; }
.m-cond { color:var(--cond); font-weight:600; }
.m-loop { color:var(--loop); font-weight:600; }
.ext-note { color:var(--muted); font-style:italic; }
</style>
</head><body>
<header>intel describe <b id=entry></b></header>
<div id=panes></div>
<script>
const ENTRY = __ENTRY__;
document.getElementById('entry').textContent = ENTRY;
const panes = document.getElementById('panes');
const cache = new Map();
let gen = 0;

function fetchDesc(sym) {
  if (!cache.has(sym)) {
    const p = fetch('/api/describe?var=' + encodeURIComponent(sym))
      .then(r => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.json(); });
    p.catch(() => cache.delete(sym));
    cache.set(sym, p);
  }
  return cache.get(sym);
}

function treeSyms(nodes, acc) {
  (nodes || []).forEach(n => { acc.push(n.sym); treeSyms(n.children, acc); });
  return acc;
}

function allSyms(d) {
  const acc = [];
  (d.arities || []).forEach(a => treeSyms(a.nodes, acc));
  return [...new Set(acc)];
}

function renderTree(el, nodes, indent, paneIdx, hostSym) {
  (nodes || []).forEach(n => {
    const line = document.createElement('div');
    line.appendChild(document.createTextNode(' '.repeat(indent)));
    let mlen = 0;
    if (n.cond) {
      const s = document.createElement('span');
      s.className = 'm-cond'; s.textContent = '? ';
      line.appendChild(s); mlen += 2;
    }
    if (n.loop) {
      const s = document.createElement('span');
      s.className = 'm-loop'; s.textContent = '* ';
      line.appendChild(s); mlen += 2;
    }
    const v = document.createElement('span');
    v.className = 'var'; v.textContent = n.sym;
    v.onclick = () => jump(paneIdx, hostSym, n.sym);
    line.appendChild(v);
    el.appendChild(line);
    renderTree(el, n.children, indent + mlen, paneIdx, hostSym);
  });
}

function section(desc, paneIdx) {
  const s = document.createElement('div');
  s.className = 'section'; s.dataset.sym = desc.sym;
  const h = document.createElement('h2');
  h.textContent = desc.sym;
  s.appendChild(h);
  const b = document.createElement('div');
  b.className = 'body';
  if (desc.external || desc.stale) {
    s.classList.add('external');
    const n = document.createElement('div');
    n.className = 'ext-note';
    n.textContent = desc.external ? 'external, source not analysed'
                                  : 'source moved since analyse; run: intel analyse';
    b.appendChild(n);
  } else {
    if (desc.note) {
      const nt = document.createElement('div');
      nt.className = 'doc'; nt.textContent = desc.note;
      b.appendChild(nt);
    }
    if (desc.doc) {
      const d = document.createElement('div');
      d.className = 'doc'; d.textContent = JSON.stringify(desc.doc);
      b.appendChild(d);
    }
    (desc.arities || []).forEach(a => {
      if (a.argvec) {
        const av = document.createElement('div');
        av.className = 'argvec'; av.textContent = a.argvec;
        b.appendChild(av);
      }
      const t = document.createElement('div');
      t.className = 'tree';
      renderTree(t, a.nodes, 0, paneIdx, desc.sym);
      b.appendChild(t);
    });
    h.onclick = () => select(paneIdx, desc.sym);
  }
  s.appendChild(b);
  return s;
}

function truncate(idx) {
  while (panes.children.length > idx) panes.removeChild(panes.lastChild);
}

async function addPane(idx, syms, g) {
  truncate(idx);
  const p = document.createElement('div');
  p.className = 'pane';
  panes.appendChild(p);
  const descs = await Promise.all(syms.map(fetchDesc));
  if (g !== gen) return;
  descs.forEach(d => p.appendChild(section(d, idx)));
  panes.scrollLeft = panes.scrollWidth;
}

async function select(paneIdx, sym) {
  const g = ++gen;
  const pane = panes.children[paneIdx];
  if (!pane) return;
  [...pane.querySelectorAll('.section')]
    .forEach(s => s.classList.toggle('selected', s.dataset.sym === sym));
  const d = await fetchDesc(sym);
  if (g !== gen) return;
  const syms = allSyms(d);
  if (syms.length) await addPane(paneIdx + 1, syms, g);
  else truncate(paneIdx + 1);
}

async function jump(paneIdx, hostSym, sym) {
  await select(paneIdx, hostSym);
  const next = panes.children[paneIdx + 1];
  if (!next) return;
  const sec = [...next.querySelectorAll('.section')].find(s => s.dataset.sym === sym);
  if (!sec) return;
  await select(paneIdx + 1, sym);
  sec.scrollIntoView({ block: 'nearest' });
}

(async function () {
  await addPane(0, [ENTRY], ++gen);
  await select(0, ENTRY);
})().catch(e => console.error(e));
</script>
</body></html>")

(defn- json-shape [{:keys [sym doc arities external stale note]}]
  (cond-> {:sym (str sym)}
    doc      (assoc :doc doc)
    note     (assoc :note note)
    external (assoc :external true)
    stale    (assoc :stale true)
    arities  (assoc :arities
                    (mapv (fn [{:keys [argvec nodes]}]
                            (cond-> {:nodes nodes}
                              argvec (assoc :argvec (pr-str argvec))))
                          arities))))

(defn- query-var [qs]
  (try
    (some->> qs
             (re-find #"(?:^|&)var=([^&]+)")
             second
             (#(java.net.URLDecoder/decode % "UTF-8"))
             symbol)
    (catch Exception _ nil)))

(defn- handler [describe* entry {:keys [uri] :as req}]
  (cond
    (= "/" uri)
    {:status  200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body    (str/replace page "__ENTRY__" (json/generate-string (str entry)))}

    (= "/api/describe" uri)
    (if-let [sym (query-var (:query-string req))]
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string (json-shape (describe* sym)))}
      {:status 400 :body "missing ?var="})

    :else {:status 404 :body "not found"}))

(defn serve!
  "Serve the stacked-panes reader entry-pointed at sym. Blocks until killed."
  [g sym {:keys [port]}]
  (let [port      (or port 7373)
        describe* (memoize #(describe g %))]
    (http/run-server #(handler describe* sym %) {:port port :ip "127.0.0.1"})
    (binding [*out* *err*]
      (println (str "describe server at http://localhost:" port "/")))
    @(promise)))
