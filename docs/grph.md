#### GRPH:\*AGGREGATES\*

```
valid aggregate clauses in qry: (CNT GRP)
```

#### GRPH:\*CLAUSES\*

```
valid query clauses: (AND NOT OR OR-JOIN NOT-JOIN Q % F FACT UNIQ)
```

#### GRPH:\*DIR-MODES\*

```
valid edge direction modes: (-> <- <> ><)
```

#### GRPH:\*PARALLEL\*

```
set to nil at compile time to disable parallism. default: T
```

#### GRPH:\*POS-MODES\*

```
valid spatial modes: (ABS REL)
```

#### GRPH:@BOTH

```
 ; GRPH:@BOTH
 ;   [symbol]
 ; 
 ; @BOTH names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list all verts of a that are bi-directional.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@EDGES

```
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
 ; GRPH:@EITHER
 ;   [symbol]
 ; 
 ; @EITHER names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list both inbound and outbond verts of a.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@ENUM

```
 ; GRPH:@ENUM
 ;   [symbol]
 ; 
 ; @ENUM names a compiled function:
 ;   Lambda-list: (G)
 ;   Derived type: (FUNCTION (GRPH:GRPH)
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     total number of edges in graph.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@IN

```
 ; GRPH:@IN
 ;   [symbol]
 ; 
 ; @IN names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list all outboud verts of a.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@MEM

```
 ; GRPH:@MEM
 ;   [symbol]
 ; 
 ; @MEM names a compiled function:
 ;   Lambda-list: (G A B &AUX (ESET (@ (ADJ G) A)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32) (SIGNED-BYTE 32))
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     t if edge (a b) exists.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@MID

```
 ; GRPH:@MID
 ;   [symbol]
 ; 
 ; @MID names a compiled function:
 ;   Lambda-list: (G K &OPTIONAL P)
 ;   Derived type: (FUNCTION (GRPH:GRPH T &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get val of prop, p, for key, k. should be a prop (keyword).
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@OUT

```
 ; GRPH:@OUT
 ;   [symbol]
 ; 
 ; @OUT names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list all outboud verts of a.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@PNUM

```
 ; GRPH:@PNUM
 ;   [symbol]
 ; 
 ; @PNUM names a compiled function:
 ;   Lambda-list: (G)
 ;   Derived type: (FUNCTION (GRPH:GRPH) *)
 ;   Documentation:
 ;     total number of props in graph.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@PROP

```
 ; GRPH:@PROP
 ;   [symbol]
 ; 
 ; @PROP names a compiled function:
 ;   Lambda-list: (G K &OPTIONAL P)
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH (OR LIST (SIGNED-BYTE 32)) &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get val of prop, p, for key, k should be edge (a b); or vert.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@VCNT

```
 ; GRPH:@VCNT
 ;   [symbol]
 ; 
 ; @VCNT names a compiled function:
 ;   Lambda-list: (G &AUX (RES 0))
 ;   Derived type: (FUNCTION (GRPH:GRPH)
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     count all connected verts.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:@VERTS

```
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
 ; GRPH:@VMAX
 ;   [symbol]
 ; 
 ; @VMAX names a compiled function:
 ;   Lambda-list: (G &AUX (RES 0))
 ;   Derived type: (FUNCTION (GRPH:GRPH)
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Documentation:
 ;     get highest vertex index.
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:ADD

```
 ; GRPH:ADD
 ;   [symbol]
 ; 
 ; ADD names a compiled function:
 ;   Lambda-list: (G A B &OPTIONAL PROPS)
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH (SIGNED-BYTE 32) (SIGNED-BYTE 32) &OPTIONAL
 ;                             (OR LIST KEYWORD FSET:SET))
 ;                  (VALUES GRPH:GRPH BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     new edge (a b). optionally set prop, p.
 ;     returns: (values g created?)
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:ADD!

```
 ; GRPH:ADD!
 ;   [symbol]
 ; 
 ; ADD! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL PROPS)
 ;   Documentation:
 ;     add edge edge and re-bind. returns: (a b) or nil.
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:ADD\*!

```
 ; GRPH:ADD*!
 ;   [symbol]
 ; 
 ; ADD*! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL (MODES ->) PROPS)
 ;   Documentation:
 ;     add edge edge and re-bind. returns: (a b) or nil. modes: (-> <- <>)
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:CANCEL

```
(cancel) can be used in some contexts (using, qry) to cancel
the transaction and discard all changes
```

#### GRPH:COLLECT-WHILE

```
:missing:todo:

 ; GRPH:COLLECT-WHILE
 ;   [symbol]
 ; 
 ; COLLECT-WHILE names a macro:
 ;   Lambda-list: ((&KEY (INIT (QUOTE (LIST))) (TEST (QUOTE NOT))
 ;                  (LIM 1000) (CRES (GENSYM CRES)) (CITR (GENSYM CITR)))
 ;                 &BODY BODY)
 ;   Source file: /data/x/grph/src/qry-extra.lisp
```

#### GRPH:COMPILE-QUERY

```
:missing:todo:

 ; GRPH:COMPILE-QUERY
 ;   [symbol]
```

#### GRPH:CONNECTED-VERTS

```
 ; GRPH:CONNECTED-VERTS
 ;   [symbol]
 ; 
 ; CONNECTED-VERTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Documentation:
 ;     get all connected verts.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:DEAD-ENDS

```
 ; GRPH:DEAD-ENDS
 ;   [symbol]
 ; 
 ; DEAD-ENDS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that have exactly one adjacent verts: [g-] ?y-?x ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:DEL

```
 ; GRPH:DEL
 ;   [symbol]
 ; 
 ; DEL names a compiled function:
 ;   Lambda-list: (G A B)
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32) (SIGNED-BYTE 32))
 ;                  (VALUES GRPH:GRPH BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     delete edge (a b). deletes associated props.
 ;     returns: (values g deleted?)
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:DEL!

```
 ; GRPH:DEL!
 ;   [symbol]
 ; 
 ; DEL! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL P)
 ;   Documentation:
 ;     del edge and re-bind. returns: deleted?
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:DEL-DEAD-ENDS

```
 ; GRPH:DEL-DEAD-ENDS
 ;   [symbol]
 ; 
 ; DEL-DEAD-ENDS names a compiled function:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Derived type: (FUNCTION (GRPH:GRPH &OPTIONAL SYMBOL)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     delete dead-ends until there are no more dead ends left. ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:DEL-DEAD-ENDS!

```
:missing:todo:

 ; GRPH:DEL-DEAD-ENDS!
 ;   [symbol]
```

#### GRPH:DEL-PROPS

```
:missing:todo:

 ; GRPH:DEL-PROPS
 ;   [symbol]
 ; 
 ; DEL-PROPS names a compiled function:
 ;   Lambda-list: (G AB PROPS)
 ;   Derived type: (FUNCTION (GRPH:GRPH LIST (OR LIST FSET:SET))
 ;                  (VALUES GRPH:GRPH T &OPTIONAL))
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:DISTINCT

```
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

#### GRPH:EDGE-SET

```
 ; GRPH:EDGE-SET
 ;   [symbol]
 ; 
 ; EDGE-SET names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Documentation:
 ;     get edge set. ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:EDGE-SET->HT

```
 ; GRPH:EDGE-SET->HT
 ;   [symbol]
 ; 
 ; EDGE-SET->HT names a compiled function:
 ;   Lambda-list: (ES &OPTIONAL
 ;                 (HT (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL HASH-TABLE)
 ;                  (VALUES HASH-TABLE &OPTIONAL))
 ;   Documentation:
 ;     convert edge set to hash table.
 ;   Source file: /data/x/grph/src/edge-set.lisp
```

#### GRPH:EDGE-SET->PATH

```
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

#### GRPH:ENSURE-LIST

```
 ; GRPH:ENSURE-LIST
 ;   [symbol]
 ; 
 ; ENSURE-LIST names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (T) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return l if l is a nil/list. otherwise return (list l).
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:EXT-SYMBOLS?

```
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

#### GRPH:FIRST<

```
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
 ; GRPH:GATHER-MATCH
 ;   [symbol]
 ; 
 ; GATHER-MATCH names a macro:
 ;   Lambda-list: (G L P R)
 ;   Documentation:
 ;     return list of matches for (l p r).
 ;   Source file: /data/x/grph/src/qry-match.lisp
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
 ;                  (&OPTIONAL FSET:MAP (UNSIGNED-BYTE 32) FSET:MAP
 ;                   FSET:MAP)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/grph/src/grph.lisp
 ; 
 ; GRPH names the structure-class #<STRUCTURE-CLASS GRPH:GRPH>:
 ;   Documentation:
 ;     create a graph instance.
 ;     
 ;     assuming the following graph, where all edges are bi directional:
 ;     
 ;       x-y-u
 ;       |   |
 ;     a-b-c-d-o-l
 ;       |
 ;       y
 ;     
 ;     the following terminology is used:
 ;       - ab, by and dol are (simple) filaments
 ;       - bcd and bxyud are segments.
 ;       - (simple) filaments are segments.
 ;       - bcduyx(b) is a cycle.
 ;       - b and d are multi intersection points/vertices
 ;       - a, y, l are dead-ends.
 ;       - a, b, c, y are incident of b
 ;   Class precedence-list: GRPH, STRUCTURE-OBJECT, SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Sealed.
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
 ;       Type: VEQ:PN
 ;       Initform: 0
```

#### GRPH:HT->EDGE-SET

```
 ; GRPH:HT->EDGE-SET
 ;   [symbol]
 ; 
 ; HT->EDGE-SET names a compiled function:
 ;   Lambda-list: (HT)
 ;   Derived type: (FUNCTION (HASH-TABLE) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     inverse of edge-set->ht.
 ;   Source file: /data/x/grph/src/edge-set.lisp
```

#### GRPH:INGEST-EDGES

```
 ; GRPH:INGEST-EDGES
 ;   [symbol]
 ; 
 ; INGEST-EDGES names a compiled function:
 ;   Lambda-list: (EDGES &OPTIONAL (G (GRPH)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL GRPH:GRPH)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     ingest a list of edges with props. eg: ((0 :a 3) ...). and return a grph.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:INGEST-PROPS-EDGES

```
 ; GRPH:INGEST-PROPS-EDGES
 ;   [symbol]
 ; 
 ; INGEST-PROPS-EDGES names a compiled function:
 ;   Lambda-list: (PEDGES &OPTIONAL (G (GRPH)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL GRPH:GRPH)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     ingest list of props and flattened edges. see props-edges.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:ITR-ADJ

```
 ; GRPH:ITR-ADJ
 ;   [symbol]
 ; 
 ; ITR-ADJ names a macro:
 ;   Lambda-list: ((G A B &OPTIONAL (MODES ->)) &BODY BODY)
 ;   Documentation:
 ;     iterate all adjacent verts, b, of a. modes: (-> <- >< <>).
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:ITR-EDGES

```
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
 ; GRPH:ITR-VERTS
 ;   [symbol]
 ; 
 ; ITR-VERTS names a macro:
 ;   Lambda-list: ((G A) &BODY BODY)
 ;   Documentation:
 ;     iterate all connected verts, as a.
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:LADD\*!

```
:missing:todo:

 ; GRPH:LADD*!
 ;   [symbol]
 ; 
 ; LADD*! names a macro:
 ;   Lambda-list: (G E &OPTIONAL (MODES ->) PROPS)
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:LAST\*

```
 ; GRPH:LAST*
 ;   [symbol]
 ; 
 ; LAST* names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     last item in list.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:LDEL!

```
 ; GRPH:LDEL!
 ;   [symbol]
 ; 
 ; LDEL! names a macro:
 ;   Lambda-list: (G E &OPTIONAL P)
 ;   Documentation:
 ;     del edge ab=(a b) and re-bind. returns: deleted?
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:LQRY

```
 ; GRPH:LQRY
 ;   [symbol]
 ; 
 ; LQRY names a compiled function:
 ;   Lambda-list: (G &KEY DB SELECT WHERE THEN COLLECT)
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH &KEY (:DB BOOLEAN) (:SELECT T) (:WHERE T)
 ;                             (:THEN T) (:COLLECT T))
 ;                  *)
 ;   Documentation:
 ;     compile and evaluate queries at runtime. ex:
 ;       (let ((g (grph)) (q '(or (?x ?p ?y) (?y ?p ?x))))
 ;         (add! g 1 2)
 ;         (print (lqry g :select '(?x ?p ?y) :where q)))
 ;   Source file: /data/x/grph/src/qry.lisp
```

#### GRPH:LSORT

```
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

#### GRPH:LST->MAP

```
 ; GRPH:LST->MAP
 ;   [symbol]
 ; 
 ; LST->MAP names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert fset:map to list.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:LST->SET

```
 ; GRPH:LST->SET
 ;   [symbol]
 ; 
 ; LST->SET names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert list to fset:set.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:LST->SET-FX

```
 ; GRPH:LST->SET-FX
 ;   [symbol]
 ; 
 ; LST->SET-FX names a compiled function:
 ;   Lambda-list: (LL &OPTIONAL (FX (FUNCTION IDENTITY)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL FUNCTION)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     make an fset:set with (fx o) for every o in ll. see set->lst-fx.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:MAKE

```
:missing:todo:

 ; GRPH:MAKE
 ;   [symbol]
 ; 
 ; MAKE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (ADJ NILMAP) (NUM-EDGES 0) (PROPS NILMAP)
 ;                 (MID NILMAP))
 ;   Derived type: (FUNCTION
 ;                  (&OPTIONAL FSET:MAP (UNSIGNED-BYTE 32) FSET:MAP
 ;                   FSET:MAP)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:MAP->LST

```
 ; GRPH:MAP->LST
 ;   [symbol]
 ; 
 ; MAP->LST names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert fset:map to list.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:MATCH

```
 ; GRPH:MATCH
 ;   [symbol]
 ; 
 ; MATCH names a macro:
 ;   Lambda-list: ((G F LFT MID RHT) &BODY BODY)
 ;   Documentation:
 ;     execute body with alist f with vars for every fact in the graph
 ;     that matches the pattern (lft mid rht). f is on the form ((?A . 0) (?P . :a)).
 ;   Source file: /data/x/grph/src/qry-match.lisp
```

#### GRPH:MEMO

```
 ; GRPH:MEMO
 ;   [symbol]
 ; 
 ; MEMO names a compiled function:
 ;   Lambda-list: (FX &AUX (HT (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION (FUNCTION) (VALUES FUNCTION &OPTIONAL))
 ;   Documentation:
 ;     return function that memoizes calls to fx.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:MODIFY!

```
 ; GRPH:MODIFY!
 ;   [symbol]
 ; 
 ; MODIFY! names a macro:
 ;   Lambda-list: ((G* SYM &KEY (OUT G*)) &BODY BODY)
 ;   Documentation:
 ;     batch modify g in a transaction. more efficient for loading a large number
 ;     of edges and/or props. faster for larger batches. g will be available
 ;     unchanged inside the context. and the changes are applied at the end. use
 ;     :out to bind the result to a different variable.
 ;     
 ;     ex: (modify! (g mygrp)
 ;           (loop for a = (rnd:rndi n) for b = (rnd:rndi n) repeat 10
 ;                 do ; NOTE:
 ;                    ; (mygrp-cancel) aborts the transaction,
 ;                    ; (mygrp-stop) stops the transaction, but keeps the changes,
 ;                    (rnd:either (mygrp-> a b '(:x :c))
 ;                                (mygrp<> a b '(:y :d)))))
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:MULTI-ISECTS

```
 ; GRPH:MULTI-ISECTS
 ;   [symbol]
 ; 
 ; MULTI-ISECTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that have 3 or more adjacent verts. ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:NORMALISE-FOLD

```
 ; GRPH:NORMALISE-FOLD
 ;   [symbol]
 ; 
 ; NORMALISE-FOLD names a compiled function:
 ;   Lambda-list: (G)
 ;   Derived type: (FUNCTION (GRPH:GRPH) (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     remove all edges (a b) where a > b, and create edge (b a) if it does not exist.
 ;     also moves all properties from (a b) to (b a).
 ;   Source file: /data/x/grph/src/qry-extra.lisp
```

#### GRPH:NORMALISE-FOLD!

```
 ; GRPH:NORMALISE-FOLD!
 ;   [symbol]
 ; 
 ; NORMALISE-FOLD! names a macro:
 ;   Lambda-list: (G)
 ;   Documentation:
 ;     remove all edges (a b) where a > b, and create edge (b a) if it does not exist.
 ;     also moves all properties from (a b) to (b a).
 ;   Source file: /data/x/grph/src/qry-extra.lisp
```

#### GRPH:NUM-EITHER

```
 ; GRPH:NUM-EITHER
 ;   [symbol]
 ; 
 ; NUM-EITHER names a compiled function:
 ;   Lambda-list: (G ?X &OPTIONAL (?P _))
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32) &OPTIONAL SYMBOL)
 ;                  (VALUES (UNSIGNED-BYTE 44) &OPTIONAL))
 ;   Documentation:
 ;     number of adjacent verts to ?x. ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:PATH!

```
 ; GRPH:PATH!
 ;   [symbol]
 ; 
 ; PATH! names a macro:
 ;   Lambda-list: (G PATH &OPTIONAL (MODES (QUOTE (OPEN ->))) PROPS)
 ;   Documentation:
 ;     add path (a b c ...). modes (-> <- <> open closed)
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:PATH->EDGE-SET

```
 ; GRPH:PATH->EDGE-SET
 ;   [symbol]
 ; 
 ; PATH->EDGE-SET names a compiled function:
 ;   Lambda-list: (PATH &KEY CLOSED)
 ;   Derived type: (FUNCTION (LIST &KEY (:CLOSED BOOLEAN))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return edge set from cycle.
 ;     ex: (1 2 3 4 5) -> ((1 2) (2 3) (3 4) (4 5))
 ;     if closed is t, (1 5) will be included in the above output.
 ;   Source file: /data/x/grph/src/edge-set.lisp
```

#### GRPH:PDEL!

```
:missing:todo:

 ; GRPH:PDEL!
 ;   [symbol]
```

#### GRPH:PROP

```
 ; GRPH:PROP
 ;   [symbol]
 ; 
 ; PROP names a compiled function:
 ;   Lambda-list: (G K PROPS)
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH (OR LIST (SIGNED-BYTE 32))
 ;                             (OR LIST KEYWORD FSET:SET))
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     set prop, p, of edge or vert, k.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/grph/src/grph.lisp
```

#### GRPH:PROP!

```
 ; GRPH:PROP!
 ;   [symbol]
 ; 
 ; PROP! names a macro:
 ;   Lambda-list: (G K PROPS)
 ;   Documentation:
 ;     add edge/vert prop for key, k.
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:PROPS-EDGES

```
 ; GRPH:PROPS-EDGES
 ;   [symbol]
 ; 
 ; PROPS-EDGES names a compiled function:
 ;   Lambda-list: (G)
 ;   Derived type: (FUNCTION (GRPH:GRPH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list of lists of prop with flattend list of edges. see ingest-props-edges
 ;   Source file: /data/x/grph/src/grph-walk.lisp
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
 ; GRPH:QRY
 ;   [symbol]
 ; 
 ; QRY names a macro:
 ;   Lambda-list: (G &KEY DB IN USING SELECT WHERE COLLECT THEN FIRST
 ;                 PAIRS)
 ;   Documentation:
 ;     evaluate a trivial (datalog-like) query against g.
 ;     ex: (qry g :select (?x ?y)
 ;                :where (and (?x :c ?y)
 ;                            (not (or (?x :a 1)
 ;                                     (?x :a 3)))))
 ;     will return tuples (?x ?y) matching the query; all verts with :a property to
 ;     either 1 or 3 other alternatives are (selected vars are available when using
 ;     these keywords):
 ;      - :pairs T; same as the default, but return the full result pairs
 ;      - :collect [this]; same as default, but collect this instead of just the selected vars
 ;      - :then [this]; execute this code, returns nil
 ;      - :first [this]; execut this, but for the first match only.
 ;     
 ;     other modifiers:
 ;      - :in [vars]; use values of vars bound outside the query.
 ;      - :using [vars]; mutate the graph for every returned tuple, see examples
 ;      - :db T; print some useful debug info about the compiled query.
 ;     
 ;     see examples for more usage.
 ;   Source file: /data/x/grph/src/qry.lisp
```

#### GRPH:QRY-COLLECT-WHILE

```
 ; GRPH:QRY-COLLECT-WHILE
 ;   [symbol]
 ; 
 ; QRY-COLLECT-WHILE names a macro:
 ;   Lambda-list: (G &REST REST)
 ;   Documentation:
 ;     (let ((?a 2) (?b 1))
 ;       (grph:qry-collect-while g
 ;          :init (list ?a ?b) :in ?b
 ;          :select ?n :where (and (or (?b _ ?n) (?n _ ?b))
 ;                                 (% (not (member ?n cres))))
 ;          :first (progn (setf ?b ?n) ?n)
 ;          :cres cres))
 ;   Source file: /data/x/grph/src/qry-extra.lisp
```

#### GRPH:RELNEIGH

```
 ; GRPH:RELNEIGH
 ;   [symbol]
 ; 
 ; RELNEIGH names a compiled function:
 ;   Lambda-list: (INDS DSTFX &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (LIST FUNCTION) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     create list of edges in the relative neigborhood graph of inds according to
 ;     (dstfx i j) for indices i,j in inds.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:RQRY

```
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
 ;               :then (print (list *st-reach *li-reach *ans-a *ans-b)))
 ;     
 ;     note the difference between rule types:
 ;      - trivial rules contain only queries that can be passed directly to qry
 ;      - simple rules reference earlier rules, but not themselves
 ;      - linear rules have (only) one self-reference (references to earlier
 ;        rules are allowed.)
 ;   Source file: /data/x/grph/src/qry-rules.lisp
```

#### GRPH:SEGMENT-ISECTS

```
 ; GRPH:SEGMENT-ISECTS
 ;   [symbol]
 ; 
 ; SEGMENT-ISECTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that do not have exactly 2 adjacent verts. ie. the set of dead
 ;     ends and multi isects. ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:SET->LST

```
 ; GRPH:SET->LST
 ;   [symbol]
 ; 
 ; SET->LST names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert fset:set to list.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:SET->LST-FX

```
 ; GRPH:SET->LST-FX
 ;   [symbol]
 ; 
 ; SET->LST-FX names a compiled function:
 ;   Lambda-list: (SS &OPTIONAL (FX (FUNCTION IDENTITY)) &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (FSET:SET &OPTIONAL FUNCTION)
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     inverse of lst->set-fx.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:SPLIT!

```
 ; GRPH:SPLIT!
 ;   [symbol]
 ; 
 ; SPLIT! names a macro:
 ;   Lambda-list: (G A B C &OPTIONAL (MODES (QUOTE (->))))
 ;   Documentation:
 ;     delete edge and insert c according to mode c is only evaluated once, and
 ;     only if the mode is triggered. returns nil if nothing happens otherwise
 ;     returns what c evaluates to.
 ;     
 ;     modes:
 ;       ->, <- : delete ab or ba if it exists and create either acb or bca
 ;       <>     : deletes both edges if both exist and creates acb and bca
 ;       ><     : does -> or <- or both
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:STOP

```
(stop) can be used in some contexts (using, qry) to stop
the transaction, but keep the changes
```

#### GRPH:TO-VECTOR

```
 ; GRPH:TO-VECTOR
 ;   [symbol]
 ; 
 ; TO-VECTOR names a compiled function:
 ;   Lambda-list: (INIT)
 ;   Derived type: (FUNCTION (LIST) (VALUES SIMPLE-VECTOR &OPTIONAL))
 ;   Documentation:
 ;     make non-adjustable array with init contents.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:TWO-ISECTS

```
 ; GRPH:TWO-ISECTS
 ;   [symbol]
 ; 
 ; TWO-ISECTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that have exactly 2 adjacent verts [g-] ?y1-?x-?y2 [-g] ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:USING

```
 ; GRPH:USING
 ;   [symbol]
 ; 
 ; USING names a macro:
 ;   Lambda-list: ((&REST USING) &BODY BODY)
 ;   Documentation:
 ;     transaction context:
 ;     
 ;       (let ((a (list)))
 ;         (grph:using (^a) ; ^[var] prefix is required, and [var] must be bound outside
 ;           (push 1 ^a)    ; ^a is now '(1)
 ;           (push 2 ^a)    ; ^a is now '(2 1)
 ;           (print a))     ;  a is still nil
 ;         (print a))       ; ^a is bound to a, so a is now (2 1)
 ;     
 ;     (grph:cancel) and (grph:stop) can be used inside the macro to stop and discard
 ;     or keep changes respectively.
 ;     
 ;     NOTE: this behaviour is only guaranteed for immutable data structures
 ;     (eg grph, fset); or for operations that do not mutate their operands, such as
 ;     push.
 ;   Source file: /data/x/grph/src/macros.lisp
```

#### GRPH:V?

```
 ; GRPH:V?
 ;   [symbol]
 ; 
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T) &AUX
 ;                 (V
 ;                  (SLOT-VALUE (FIND-SYSTEM (QUOTE GRPH))
 ;                              (QUOTE VERSION))))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     return/print grph version.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:VECTOR-FIRST

```
 ; GRPH:VECTOR-FIRST
 ;   [symbol]
 ; 
 ; VECTOR-FIRST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (VECTOR) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     first element of vector.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:VECTOR-LAST

```
 ; GRPH:VECTOR-LAST
 ;   [symbol]
 ; 
 ; VECTOR-LAST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (VECTOR) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     last element of vector.
 ;   Source file: /data/x/grph/src/utils.lisp
```

#### GRPH:WALK-EDGE-SET

```
 ; GRPH:WALK-EDGE-SET
 ;   [symbol]
 ; 
 ; WALK-EDGE-SET names a compiled function:
 ;   Lambda-list: (G ES &AUX (EDGES (EDGE-SET->HT ES)))
 ;   Derived type: (FUNCTION (GRPH:GRPH LIST) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return a list of paths ((p1 closed?) (p2 closed?) ...) from edge set from g.
 ;     every edge is included exactly once. ignores edge dir.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:WALK-EDGE-SET-SEGMENTS

```
 ; GRPH:WALK-EDGE-SET-SEGMENTS
 ;   [symbol]
 ; 
 ; WALK-EDGE-SET-SEGMENTS names a compiled function:
 ;   Lambda-list: (G ES &AUX (EDGES (EDGE-SET->HT ES)))
 ;   Derived type: (FUNCTION (GRPH:GRPH LIST) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     walk edge set and split into segments.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:WALK-GRPH

```
 ; GRPH:WALK-GRPH
 ;   [symbol]
 ; 
 ; WALK-GRPH names a compiled function:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Derived type: (FUNCTION (GRPH:GRPH &OPTIONAL SYMBOL) *)
 ;   Documentation:
 ;     walk graph via walk-edge-set.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

#### GRPH:WALK-GRPH-SEGMENTS

```
 ; GRPH:WALK-GRPH-SEGMENTS
 ;   [symbol]
 ; 
 ; WALK-GRPH-SEGMENTS names a compiled function:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Derived type: (FUNCTION (GRPH:GRPH &OPTIONAL SYMBOL) *)
 ;   Documentation:
 ;     walk graph via walk-edge-set-segments.
 ;   Source file: /data/x/grph/src/grph-walk.lisp
```

