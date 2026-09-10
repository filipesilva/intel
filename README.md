# intel

Analyse your Clojure code with Datalog.

intel loads the var-usage graph of a codebase, extracted by
[clj-kondo](https://github.com/clj-kondo/clj-kondo), into a
[Datalevin](https://github.com/juji-io/datalevin) database. Canned commands
answer the questions developers ask while refactoring and reviewing PRs;
raw Datalog covers the questions nobody anticipated. Analysis is static, so
it works on any checkout without deps or a REPL, and
[babashka](https://github.com/babashka/babashka) keeps every command fast.

## Install

```sh
bbin install io.github.filipesilva/intel
```

## Use

Build the database once per checkout state:

```sh
$ intel analyse
analysed src test: 75 namespaces, 1288 vars, 15709 edges (3.3s)
```

Then query it:

```sh
$ intel deps myapp.orders/place-order        # what does this use?
$ intel _deps myapp.orders/validate          # who uses this?
$ intel _deps myapp.orders/validate -d 0     # ...transitively?
$ intel path myapp.handlers/checkout myapp.email/send!   # why does A reach B?
$ intel dead                                 # unused and test-only vars
$ intel untested                             # vars no test reaches
$ intel ls 'myapp.orders/*' -l               # vars with file:line and flags
```

### Run only the tests a change can affect

`intel affected REF` is the reverse-transitive closure of the vars that
changed vs a git ref: everything whose behaviour those changes can reach.
Add `--test` and it becomes the list of tests worth running:

```sh
$ intel affected origin/master --test
myapp.orders-test/place-order-test
myapp.checkout-test/full-flow-test
```

The output is fully-qualified test vars, which runners select directly, or
collapse it to namespaces for runners that work at that grain:

```sh
# cognitect test-runner, by var
clojure -X:test :vars "[$(intel affected origin/master --test | sed 's/^/,/')]"
# kaocha, by namespace
intel affected origin/master --test | sed 's|/.*||' | sort -u \
  | xargs -I{} echo --focus {} | xargs bin/kaocha
```

Without `--test`, `intel affected` is the whole blast radius (the change
plus every downstream var), so a PR's real size is `intel affected origin/master
| wc -l`, not its line count. Exit codes are grep-style (0 results, 1 empty,
2 error), so CI gates are one-liners:

```sh
intel affected origin/master --test >/dev/null \
  || echo "warning: no test reaches this change"
```

`deps`, `_deps`, and `affected` share the same options (`--depth`, `--count`,
`--filter`/`--remove` globs, `--test`/`--no-test`, `-l`, `--edn`); `deps` and
`_deps` also take glob seeds and read seeds from stdin as `-`, so anything not
already a command composes:

```sh
$ intel _deps 'myapp.*' --count | head                   # most-used vars
$ intel deps 'myapp.*' -d 0 --count --no-test | head     # biggest closures
$ intel deps 'myapp.domain.*' -d 0 --filter 'myapp.web.*'  # layering violations
```

## Reading code semantically

`intel describe` renders one var's body as a tree of the vars it uses, in
source order: `?` marks a var behind (or deciding) a conditional, `*` marks a
var behind a loop, and nesting shows what a guard gates. clojure.core is
elided; project and library vars both show.

```sh
$ intel describe intel/cmd-deps
intel/cmd-deps
  [direction args opts]
  ? intel/fail!
  ? * intel/stdin-seed
      intel/stdin-syms
  intel.db/with-db
  intel.db/load-graph
  intel/resolve-seeds
  intel/emit-closure
```

Add `--http` (and optionally `--port N`, default 7373) to read the same tree
as stacked panes in the browser, in the spirit of Obsidian's stacked tabs and
Andy Matuschak's sliding notes: the first pane describes the entry var, and
selecting any var opens the next pane with every var it mentions expanded.

```sh
$ intel describe intel/cmd-deps --http
describe server at http://localhost:7373/
```

`describe` is experimental. It re-reads the var's source at call time, so it
stays honest about the file even when the rest of the db is stale.

## Custom queries

The schema is small enough to hold in your head; `intel schema` prints it
with a rules library and copy-paste examples. `intel q` runs any Datalog
query, with transitive-closure rules bound to `%` by default:

```sh
$ intel q '[:find ?s :in $ % ?root :where
            [?r :sym ?root] (used+ ?r ?u) [?u :sym ?s]]' myapp.db/query
```

`intel q -f FILE` runs a query from a file, so a repo can keep a directory
of house queries.

## Limitations

Analysis is static, so `affected` (and any blast radius) is a lower bound.
Code inside `defmethod`, `extend-protocol`, `extend-type`, `defrecord`, and
`deftype` bodies is attributed to the multimethod, protocol, or type var,
which is usually what you want, but which method implementation runs is
decided at runtime. Dispatch through `resolve` or dynamic vars is invisible.
So `intel affected --test` is excellent for the edit-run-repeat inner loop and
for ordering a suite fast-first, but it can miss a genuinely affected test;
keep a full run as the backstop before merging, rather than using it to
permanently skip tests. Vars referenced only inside syntax-quoted macro bodies
can appear unused. `diff` maps changed lines against the current database, so
vars deleted by a change are not reported; their broken callers surface at
compile time anyway. For libraries, public API vars legitimately appear in
`intel dead` output as `unused`; scope with a pattern or `--remove` to cut
that noise.

## Development

```sh
bb test                      # unit tests
bb -m intel ...  # run from source
```
