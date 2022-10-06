# GRPH


## About

`grph` is an in-memory immutable graph structure with
[datalog](https://en.wikipedia.org/wiki/Datalog) query language. The data
structure and interfaces are generic, but it is primarily intended to
be used in my generative art experiments.. It builds on several of the ideas
and issues I encountered when writing the `weir` system which is currently a
part of [weird](https://github.com/inconvergent/weird).

The main features is that the graph data structure builds on fset in order to
be immutable. And the separation of graph structure and spatial data.

Having an immutable data structure means that is it possible to have
conditional transactions. And it makes it easier to do certain kinds of
animation.

Having a separation between the graph and the spatial information makes it
easier to work with higher dimensional data.

The datalog "dialect" in `grph` is a simplified subset of the query language in
[Datomic](https://docs.datomic.com/cloud/query/query-data-reference.html).  It
might be expanded and/or improved in the future. See the examples.

![Lines](img/lines.png)


## Components

The `grph` system has two packages:

1. `grph` contains the graph structure, as well as the query interface (datalog
   compiler).
2. `xgrph` expands `grph` with some spatial awareness.

See the [docs folder](docs) for auomatically generated docs (incomplete).


## Examples

Some examples of use are included in the [examples folder](examples).

Here is a small example that demonstrates a few datalog queries for a small
graph:

```lisp
  (let ((g (grph:ingest-facts (grph:grph)
             '((0 :A 1) (0 :C 1) (1 :A 3) (1 :A 2) (1 :A 0) (1 :C 0)
               (2 :A 1) (3 :C 7) (3 :B 5) (3 :C 5) (3 :B 4) (3 :A 1)
               (4 :B 3) (4 :B 5) (4 :E 5) (5 :B 3) (5 :C 3) (5 :B 4)
               (5 :E 4) (7 :C 3) (99 :X 77)))))

    (print (grph:qry g :select (?x ?y)
                       :where (and (?x :a ?y) (?x :c ?y))))
    ;> '((1 0) (0 1))

    (print (grph:qry g :select (?x ?y)
                       :where (and (?x :c ?y)
                                   (not (or (?x :a 1) (?x :a 3))))))
    ;> '((7 3) (5 3))

    (print (grph:qry g :select ?r
                       :where (or-join ?r (and (?r :a ?a) (?a :b 5))
                                          (?r :c 0)
                                          (?r :e 5))))
    ;> '((4) (1)))
```


## Dependencies

  - [cl-veq](https://github.com/inconvergent/cl-veq). Must be installed from
    the Github repo (not quicklisp).
  - [fset](https://fset.common-lisp.dev/). Automatically installed via
    quicklisp.
  - [lparallel](https://lparallel.org/). Not required by default, but some
    queries can be significantly faster when `grph` is loaded with
    `:grph-parallel` in `*feautures*`; in which case `lparallel` will be
    automatically installed via quicklisp.
  - [weird](https://github.com/inconvergent/weird). Some of the examples depend
    on `weird`. But the `grph` system does not require `weird` directly.


## Tests

Tests can be executed using: (asdf:test-system :grph). Or by executing
`./run-tests.sh`.

## Note

This code is likely to change with little or no warning. You should not use
this for anything remotely important. Make sure to fork the repository if you
need it to remain stable.

