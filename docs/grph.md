#### GRPH:\*DIR-MODE\*

```
valid edge direction modes.

 ; GRPH:*DIR-MODE*
 ;   [symbol]
 ; 
 ; *DIR-MODE* names a special variable:
 ;   Value: (:-> :<- :<>)
```

#### GRPH:\*POS-MODE\*

```
valid spatial modes.

 ; GRPH:*POS-MODE*
 ;   [symbol]
 ; 
 ; *POS-MODE* names a special variable:
 ;   Value: (:REL :ABS)
```

#### GRPH:@BOTH

```
list all verts of a that are bi-directional.

 ; GRPH:@BOTH
 ;   [symbol]
 ; 
 ; @BOTH names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (UNSIGNED-BYTE 31))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list all verts of a that are bi-directional.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@EDGES

```
list of lists of all edges.

 ; GRPH:@EDGES
 ;   [symbol]
 ; 
 ; @EDGES names a compiled function:
 ;   Lambda-list: (G &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list of lists of all edges.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@EITHER

```
list both inbound and outbond verts of a.

 ; GRPH:@EITHER
 ;   [symbol]
 ; 
 ; @EITHER names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (UNSIGNED-BYTE 31))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list both inbound and outbond verts of a.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@ENUM

```
total number of edges in graph.

 ; GRPH:@ENUM
 ;   [symbol]
 ; 
 ; @ENUM names a compiled function:
 ;   Lambda-list: (G)
 ;   Derived type: (FUNCTION (GRPH:GRPH)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     total number of edges in graph.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@IN

```
list all outboud verts of a.

 ; GRPH:@IN
 ;   [symbol]
 ; 
 ; @IN names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (UNSIGNED-BYTE 31))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list all outboud verts of a.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@INV

```
:missing:todo:

 ; GRPH:@INV
 ;   [symbol]
```

#### GRPH:@MEM

```
t if edge (a b) exists.

 ; GRPH:@MEM
 ;   [symbol]
 ; 
 ; @MEM names a compiled function:
 ;   Lambda-list: (G A B &AUX (ESET (@ (ADJ G) A)))
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH (UNSIGNED-BYTE 31) (UNSIGNED-BYTE 31)) *)
 ;   Documentation:
 ;     t if edge (a b) exists.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@OUT

```
list all outboud verts of a.

 ; GRPH:@OUT
 ;   [symbol]
 ; 
 ; @OUT names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (UNSIGNED-BYTE 31))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list all outboud verts of a.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@PROP

```
get val of prop, p, for key, k. should be an edge k=(a b) or a vert, k=a.

 ; GRPH:@PROP
 ;   [symbol]
 ; 
 ; @PROP names a compiled function:
 ;   Lambda-list: (G K &OPTIONAL P)
 ;   Derived type: (FUNCTION (GRPH:GRPH T &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get val of prop, p, for key, k. should be an edge k=(a b) or a vert, k=a.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@VERTS

```
list of all connected verts.

 ; GRPH:@VERTS
 ;   [symbol]
 ; 
 ; @VERTS names a compiled function:
 ;   Lambda-list: (G &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list of all connected verts.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@VMAX

```
get highest vertex index.

 ; GRPH:@VMAX
 ;   [symbol]
 ; 
 ; @VMAX names a compiled function:
 ;   Lambda-list: (G &AUX (RES 0))
 ;   Derived type: (FUNCTION (GRPH:GRPH)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     get highest vertex index.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@VNUM

```
count all connected verts.

 ; GRPH:@VNUM
 ;   [symbol]
 ; 
 ; @VNUM names a compiled function:
 ;   Lambda-list: (G &AUX (RES 0))
 ;   Derived type: (FUNCTION (GRPH:GRPH)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     count all connected verts.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:ADD

```
new edge (a b). optionally set prop, p, (with val).
returns: (values g created?)

 ; GRPH:ADD
 ;   [symbol]
 ; 
 ; ADD names a compiled function:
 ;   Lambda-list: (G A B &OPTIONAL PROPS)
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH (UNSIGNED-BYTE 31) (UNSIGNED-BYTE 31)
 ;                             &OPTIONAL LIST)
 ;                  (VALUES GRPH:GRPH BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     new edge (a b). optionally set prop, p, (with val).
 ;     returns: (values g created?)
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:ADD!

```
add edge edge and re-bind. returns: (a b) or nil.

 ; GRPH:ADD!
 ;   [symbol]
 ; 
 ; ADD! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL PROPS)
 ;   Documentation:
 ;     add edge edge and re-bind. returns: (a b) or nil.
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:ANY-EDGE

```
:missing:todo:

 ; GRPH:ANY-EDGE
 ;   [symbol]
 ; 
 ; ANY-EDGE names a macro:
 ;   Lambda-list: (G ?P)
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:CANCEL

```
:missing:todo:

 ; GRPH:CANCEL
 ;   [symbol]
```

#### GRPH:COLLECT-WHILE

```
:missing:todo:

 ; GRPH:COLLECT-WHILE
 ;   [symbol]
 ; 
 ; COLLECT-WHILE names a macro:
 ;   Lambda-list: ((&KEY (INIT (QUOTE (LIST))) (TEST (QUOTE NOT))
 ;                  (LIM 1000) (CRES (GENSYM COLLECT-RES))
 ;                  (CITR (GENSYM COLLECT-ITR)))
 ;                 &BODY BODY)
 ;   Source file: /data/x/grph/src/qry.lisp
```

#### GRPH:COMPILE-QUERY

```
:missing:todo:

 ; GRPH:COMPILE-QUERY
 ;   [symbol]
```

#### GRPH:DEAD-ENDS

```
:missing:todo:

 ; GRPH:DEAD-ENDS
 ;   [symbol]
 ; 
 ; DEAD-ENDS names a macro:
 ;   Lambda-list: (G ?P)
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:DEL

```
delete edge (a b). deletes associated props.
returns: (values g deleted?)

 ; GRPH:DEL
 ;   [symbol]
 ; 
 ; DEL names a compiled function:
 ;   Lambda-list: (G A B)
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH (UNSIGNED-BYTE 31) (UNSIGNED-BYTE 31))
 ;                  (VALUES GRPH:GRPH BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     delete edge (a b). deletes associated props.
 ;     returns: (values g deleted?)
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:DEL!

```
del edge and re-bind. returns: deleted?

 ; GRPH:DEL!
 ;   [symbol]
 ; 
 ; DEL! names a macro:
 ;   Lambda-list: (G A B)
 ;   Documentation:
 ;     del edge and re-bind. returns: deleted?
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:DISTINCT

```
t if values in rest are distinct.

 ; GRPH:DISTINCT
 ;   [symbol]
 ; 
 ; DISTINCT names a compiled function:
 ;   Lambda-list: (&REST REST &AUX (N (LENGTH REST)))
 ;   Derived type: (FUNCTION * (VALUES BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     t if values in rest are distinct.
 ;   Source file: /data/x/grph/src/qry-runtime.lisp
```

#### GRPH:DSB

```
:missing:todo:

 ; GRPH:DSB
 ;   [symbol]
 ; 
 ; DSB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:EDGE-SET->PATH

```
convert edge set: ((3 4) (4 5) (5 6) (1 2) (6 1) (2 3))
into a path: (4 5 6 1 2 3)
second result is a boolean for whether it is a cycle.

 ; GRPH:EDGE-SET->PATH
 ;   [symbol]
 ; 
 ; EDGE-SET->PATH names a compiled function:
 ;   Lambda-list: (ES)
 ;   Derived type: (FUNCTION (LIST) (VALUES T BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     convert edge set: ((3 4) (4 5) (5 6) (1 2) (6 1) (2 3))
 ;     into a path: (4 5 6 1 2 3)
 ;     second result is a boolean for whether it is a cycle.
 ;   Source file: /data/x/grph/src/edge-set.lisp
```

#### GRPH:EXT-SYMBOLS?

```
list all external symbols in pkg. use :verbose to inlcude docstring.
use :pretty to print verbose output to stdout in a readable form.

 ; GRPH:EXT-SYMBOLS?
 ;   [symbol]
 ; 
 ; EXT-SYMBOLS? names a macro:
 ;   Lambda-list: (PKG &OPTIONAL MODE)
 ;   Documentation:
 ;     list all external symbols in pkg. use :verbose to inlcude docstring.
 ;     use :pretty to print verbose output to stdout in a readable form.
 ;   Source file: /data/x/grph/src/docs.lisp
```

#### GRPH:FILAMENT-ENDS

```
:missing:todo:

 ; GRPH:FILAMENT-ENDS
 ;   [symbol]
 ; 
 ; FILAMENT-ENDS names a macro:
 ;   Lambda-list: (G ?P)
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:FIRST<

```
equvialent to (and (< r1 r2) (< r1 r3) ...). r1 is evaluated only once.

 ; GRPH:FIRST<
 ;   [symbol]
 ; 
 ; FIRST< names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     equvialent to (and (< r1 r2) (< r1 r3) ...). r1 is evaluated only once.
 ;   Source file: /data/x/grph/src/qry-runtime.lisp
```

#### GRPH:FIRST>

```
equvialent to (and (> r1 r2) (> r1 r3) ...). r1 is evaluated only once.

 ; GRPH:FIRST>
 ;   [symbol]
 ; 
 ; FIRST> names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     equvialent to (and (> r1 r2) (> r1 r3) ...). r1 is evaluated only once.
 ;   Source file: /data/x/grph/src/qry-runtime.lisp
```

#### GRPH:GATHER-MATCH

```
:missing:todo:

 ; GRPH:GATHER-MATCH
 ;   [symbol]
 ; 
 ; GATHER-MATCH names a macro:
 ;   Lambda-list: (G L P R)
 ;   Source file: /data/x/grph/src/qry-match.lisp
```

#### GRPH:GRP

```
for val = :black and s = :color, creates list of two props
((:color :black) :black). this is useful for making both :colour and :black
filterable via @prop or in queries.

eg: (add! g a b (grp :black :color))

 ; GRPH:GRP
 ;   [symbol]
 ; 
 ; GRP names a compiled function:
 ;   Lambda-list: (VAL &OPTIONAL (S G))
 ;   Derived type: (FUNCTION (SYMBOL &OPTIONAL SYMBOL)
 ;                  (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     for val = :black and s = :color, creates list of two props
 ;     ((:color :black) :black). this is useful for making both :colour and :black
 ;     filterable via @prop or in queries.
 ;     
 ;     eg: (add! g a b (grp :black :color))
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:GRPH

```
:missing:todo:

 ; GRPH:GRPH
 ;   [symbol]
 ; 
 ; GRPH names a compiled function:
 ;   Lambda-list: (&OPTIONAL (ADJ NILMAP) (NUM-EDGES 0) (PROPS NILMAP)
 ;                 (MID NILMAP))
 ;   Derived type: (FUNCTION
 ;                  (&OPTIONAL FSET:MAP (UNSIGNED-BYTE 31) FSET:MAP
 ;                   FSET:MAP)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/grph/src/grph.lisp
 ; 
 ; GRPH names the structure-class #<STRUCTURE-CLASS GRPH:GRPH>:
 ;   Documentation:
 ;     create a directed graph instance with no spatial awareness.
 ;     
 ;     assuming the following graph, where all edges are undirected:
 ;     
 ;       x-y-u
 ;       |   |
 ;     a-b-c-d-o
 ;       |
 ;       y
 ;     
 ;     this terminology is used :
 ;       - ab, by and do are (simple) filaments.
 ;       - bcd and bxyud are segments.
 ;       - (simple) filaments are segments.
 ;       - bcduyx(b) is a cycle.
 ;       - b and d are multi intersection points/vertices
 ;       - a, y, o are dead-ends.
 ;       - a, b, c, y are incident of b
 ; 
 ;   Class precedence-list: GRPH, STRUCTURE-OBJECT, SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Slots:
 ;     ADJ
 ;       Type: FSET:MAP
 ;       Initform: NILMAP
 ;     PROPS
 ;       Type: FSET:MAP
 ;       Initform: NILMAP
 ;     MID
 ;       Type: FSET:MAP
 ;       Initform: NILMAP
 ;     NUM-EDGES
 ;       Type: GRPH:PN
 ;       Initform: 0
```

#### GRPH:INGEST-EDGES

```
ingest a list of edges with props. eg: ((0 :a 3) (8 :_ 9)).

 ; GRPH:INGEST-EDGES
 ;   [symbol]
 ; 
 ; INGEST-EDGES names a compiled function:
 ;   Lambda-list: (G F)
 ;   Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     ingest a list of edges with props. eg: ((0 :a 3) (8 :_ 9)).
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:ITR-ADJ

```
iterate all adjacent verts, b, of a. use modes (:-> :<- :>< :<>).

 ; GRPH:ITR-ADJ
 ;   [symbol]
 ; 
 ; ITR-ADJ names a macro:
 ;   Lambda-list: ((G A B &OPTIONAL (MODE OUT)) &BODY BODY)
 ;   Documentation:
 ;     iterate all adjacent verts, b, of a. use modes (:-> :<- :>< :<>).
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:ITR-EDGES

```
iterate all edges, as either a=(v1 v2) or a=v1, b=v2.

 ; GRPH:ITR-EDGES
 ;   [symbol]
 ; 
 ; ITR-EDGES names a macro:
 ;   Lambda-list: ((G A &OPTIONAL B) &BODY BODY)
 ;   Documentation:
 ;     iterate all edges, as either a=(v1 v2) or a=v1, b=v2.
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:ITR-VERTS

```
iterate all connected verts, as a.

 ; GRPH:ITR-VERTS
 ;   [symbol]
 ; 
 ; ITR-VERTS names a macro:
 ;   Lambda-list: ((G A) &BODY BODY)
 ;   Documentation:
 ;     iterate all connected verts, as a.
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:LSORT

```
radix sort list of lists.

 ; GRPH:LSORT
 ;   [symbol]
 ; 
 ; LSORT names a compiled function:
 ;   Lambda-list: (L &OPTIONAL (FX (FUNCTION <)) &AUX (L (COPY-LIST L)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL FUNCTION)
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     radix sort list of lists.
 ;   Source file: /data/x/grph/src/qry-runtime.lisp
```

#### GRPH:MATCH

```
execute body with alist f as every bindable var for every fact in the graph
that matches the pattern (lft mid rht). f is on the form ((?A . 0) (?P . :a)).

 ; GRPH:MATCH
 ;   [symbol]
 ; 
 ; MATCH names a macro:
 ;   Lambda-list: ((G F LFT MID RHT) &BODY BODY)
 ;   Documentation:
 ;     execute body with alist f as every bindable var for every fact in the graph
 ;     that matches the pattern (lft mid rht). f is on the form ((?A . 0) (?P . :a)).
 ;   Source file: /data/x/grph/src/qry-match.lisp
```

#### GRPH:MULTI-ISECTS

```
:missing:todo:

 ; GRPH:MULTI-ISECTS
 ;   [symbol]
 ; 
 ; MULTI-ISECTS names a macro:
 ;   Lambda-list: (G ?P)
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:MVB

```
:missing:todo:

 ; GRPH:MVB
 ;   [symbol]
 ; 
 ; MVB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:MVC

```
:missing:todo:

 ; GRPH:MVC
 ;   [symbol]
 ; 
 ; MVC names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:PN

```
:missing:todo:

 ; GRPH:PN
 ;   [symbol]
 ; 
 ; PN names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 31))
 ;   Expansion: (UNSIGNED-BYTE 31)
```

#### GRPH:PRT

```
:missing:todo:

 ; GRPH:PRT
 ;   [symbol]
 ; 
 ; PRT names a compiled function:
 ;   Lambda-list: (O &OPTIONAL S)
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES (OR STRING NULL) &OPTIONAL))
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:QRY

```
evaluate a trivial datalog query against g.

:ex
  (qry g :select (?x ?y)
         :where (and (?x :c ?y) (not (or (?x :a 1)
                                         (?x :a 3)))))
will return tuples (?x ?y) matching the query.
other alternatives are (selected vars are available when using these keywords):
 - :pairs T; same as the default, but return the full result pairs
 - :collect [this]; same as default, but collect this instead of just the selected vars
 - :then [this]; execute this code, returns nil
 - :first [this]; execut this, but for the first match only.

other modifiers:
 - :in [vars]; use values of vars bound outside the query.
 - :using [vars]; mutate the graph for every returned tuple, see examples
 - :res [symb]; bind query result to symb inside :collect, :then, :first
 - :itr [symb]; counter available inside :collect, :then, :first
 - :db T; print some useful debug info about the compiled query.

see examples for more usage.

 ; GRPH:QRY
 ;   [symbol]
 ; 
 ; QRY names a macro:
 ;   Lambda-list: (G &KEY DB IN USING SELECT WHEN WHERE THEN COLLECT FIRST
 ;                 PAIRS (ITR (GENSYM QRY-ITR)) (PROC (QUOTE IDENTITY))
 ;                 (RES (GENSYM QRY-RES)))
 ;   Documentation:
 ;     evaluate a trivial datalog query against g.
 ;     
 ;     :ex
 ;       (qry g :select (?x ?y)
 ;              :where (and (?x :c ?y) (not (or (?x :a 1)
 ;                                              (?x :a 3)))))
 ;     will return tuples (?x ?y) matching the query.
 ;     other alternatives are (selected vars are available when using these keywords):
 ;      - :pairs T; same as the default, but return the full result pairs
 ;      - :collect [this]; same as default, but collect this instead of just the selected vars
 ;      - :then [this]; execute this code, returns nil
 ;      - :first [this]; execut this, but for the first match only.
 ;     
 ;     other modifiers:
 ;      - :in [vars]; use values of vars bound outside the query.
 ;      - :using [vars]; mutate the graph for every returned tuple, see examples
 ;      - :res [symb]; bind query result to symb inside :collect, :then, :first
 ;      - :itr [symb]; counter available inside :collect, :then, :first
 ;      - :db T; print some useful debug info about the compiled query.
 ;     
 ;     see examples for more usage.
 ;   Source file: /data/x/grph/src/qry.lisp
```

#### GRPH:QRY-COLLECT-WHILE

```
:missing:todo:

 ; GRPH:QRY-COLLECT-WHILE
 ;   [symbol]
 ; 
 ; QRY-COLLECT-WHILE names a macro:
 ;   Lambda-list: (G &REST REST)
 ;   Source file: /data/x/grph/src/qry.lisp
```

#### GRPH:RQRY

```
evaluate simple datalog programs top-down. all rule names (with * prefix)
are bound as variables that can be used in :then.

ex:
  (rqry g :rules ((*st-reach (?x ?y) (?x _ ?y)) ; trivial
                  (*st-reach (?x ?y) (and (?x _ ?z) (*st-reach ?z ?y))) ; linear
                  (*li-reach (?x ?u) (and (*st-reach ?x _) (?x ?u _))) ; simple
                  (*ans-a (?y) (*st-reach 4 ?y)) ; simple (w/filter)
                  (*ans-b (?u) (*li-reach 1 ?u))) ; simple (w/filter)
          :then (print (list *st-reach *li-reach *ans-a *ans-b))

note the difference between rule types:
 - trivial rules contain only queries that can be passed directly to qry
 - simple rules reference earlier rules, but not themselves
 - linear rules have (only) one self-reference (references to earlier
  rules are allowed.)

 ; GRPH:RQRY
 ;   [symbol]
 ; 
 ; RQRY names a macro:
 ;   Lambda-list: (G &KEY (LIM 1000) RULES THEN)
 ;   Documentation:
 ;     evaluate simple datalog programs top-down. all rule names (with * prefix)
 ;     are bound as variables that can be used in :then.
 ;     
 ;     ex:
 ;       (rqry g :rules ((*st-reach (?x ?y) (?x _ ?y)) ; trivial
 ;                       (*st-reach (?x ?y) (and (?x _ ?z) (*st-reach ?z ?y))) ; linear
 ;                       (*li-reach (?x ?u) (and (*st-reach ?x _) (?x ?u _))) ; simple
 ;                       (*ans-a (?y) (*st-reach 4 ?y)) ; simple (w/filter)
 ;                       (*ans-b (?u) (*li-reach 1 ?u))) ; simple (w/filter)
 ;               :then (print (list *st-reach *li-reach *ans-a *ans-b))
 ;     
 ;     note the difference between rule types:
 ;      - trivial rules contain only queries that can be passed directly to qry
 ;      - simple rules reference earlier rules, but not themselves
 ;      - linear rules have (only) one self-reference (references to earlier
 ;       rules are allowed.)
 ;   Source file: /data/x/grph/src/qry-rules.lisp
```

#### GRPH:STOP

```
:missing:todo:

 ; GRPH:STOP
 ;   [symbol]
```

#### GRPH:WALK-GRPH

```
walk the grph and return tuples: ((path closed?) ...)

 ; GRPH:WALK-GRPH
 ;   [symbol]
 ; 
 ; WALK-GRPH names a macro:
 ;   Lambda-list: (G PROP &KEY (LIM 100000))
 ;   Documentation:
 ;     walk the grph and return tuples: ((path closed?) ...)
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

