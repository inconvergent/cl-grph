## `grph:*aggregates*`
```
 ; GRPH:*AGGREGATES*
 ;   [symbol]
 ; 
 ; *AGGREGATES* names a special variable:
 ;   Value: (:CNT :GRP)
 ;   Documentation:
 ;     valid aggregate clauses in qry.
```

## `grph:*clauses*`
```
 ; GRPH:*CLAUSES*
 ;   [symbol]
 ; 
 ; *CLAUSES* names a special variable:
 ;   Value: (:AND :NOT :OR :OR-JOIN :NOT-JOIN :Q :% :F :FACT :UNIQ)
 ;   Documentation:
 ;     valid query clauses.
```

## `grph:*dev*`
```
 ; GRPH:*DEV*
 ;   [symbol]
 ; 
 ; *DEV* names a special variable:
 ;   Value: NIL
 ;   Documentation:
 ;     compile in dev mode.
```

## `grph:*dir-modes*`
```
 ; GRPH:*DIR-MODES*
 ;   [symbol]
 ; 
 ; *DIR-MODES* names a special variable:
 ;   Value: (:-> :<- :<> :><)
 ;   Documentation:
 ;     valid edge direction modes.
```

## `grph:*parallel*`
```
:missing:

 ; GRPH:*PARALLEL*
 ;   [symbol]
 ; 
 ; *PARALLEL* names a special variable:
 ;   Declared type: BOOLEAN
 ;   Value: NIL
```

## `grph:*pos-modes*`
```
 ; GRPH:*POS-MODES*
 ;   [symbol]
 ; 
 ; *POS-MODES* names a special variable:
 ;   Value: (:ABS :REL)
 ;   Documentation:
 ;     valid spatial modes.
```

## `grph:@both`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@edges`
```
 ; GRPH:@EDGES
 ;   [symbol]
 ; 
 ; @EDGES names a compiled function:
 ;   Lambda-list: (G &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list of lists of all edges.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@either`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@enum`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@in`
```
 ; GRPH:@IN
 ;   [symbol]
 ; 
 ; @IN names a compiled function:
 ;   Lambda-list: (G A &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH (SIGNED-BYTE 32))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list all inboud verts of a.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@mem`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@mid`
```
 ; GRPH:@MID
 ;   [symbol]
 ; 
 ; @MID names a compiled function:
 ;   Lambda-list: (G K &OPTIONAL P)
 ;   Derived type: (FUNCTION (GRPH:GRPH T &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get props of k (edge ab or vert); or check of p is a prop of k.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@out`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@pnum`
```
 ; GRPH:@PNUM
 ;   [symbol]
 ; 
 ; @PNUM names a compiled function:
 ;   Lambda-list: (G)
 ;   Derived type: (FUNCTION (GRPH:GRPH) *)
 ;   Documentation:
 ;     total number of props in graph.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@prop`
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
 ;     get props of k (edge ab or vert); or check if p is a prop of k.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@vcnt`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@verts`
```
 ; GRPH:@VERTS
 ;   [symbol]
 ; 
 ; @VERTS names a compiled function:
 ;   Lambda-list: (G &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (GRPH:GRPH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list of all connected verts.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:@vmax`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:add`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:add!`
```
 ; GRPH:ADD!
 ;   [symbol]
 ; 
 ; ADD! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL PROPS)
 ;   Documentation:
 ;     add edge edge and re-bind. returns: (a b) or nil.
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:add*!`
```
 ; GRPH:ADD*!
 ;   [symbol]
 ; 
 ; ADD*! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL (MODES ->) PROPS)
 ;   Documentation:
 ;     add edge edge and re-bind. returns: (a b) or nil. modes: (-> <- <>)
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:cancel`
```
(cancel) can be used in some contexts (using, qry) to cancel
the transaction and discard all changes
```

## `grph:collapse!`
```
 ; GRPH:COLLAPSE!
 ;   [symbol]
 ; 
 ; COLLAPSE! names a macro:
 ;   Lambda-list: (G A B)
 ;   Documentation:
 ;     collapse edge ab[x]; create all edges ax/xa with all props.
 ;     NOTE: all ba edges/props are also deleted.
 ;     returns ab if it existed; or nil
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:collect-while`
```
:missing:

 ; GRPH:COLLECT-WHILE
 ;   [symbol]
 ; 
 ; COLLECT-WHILE names a macro:
 ;   Lambda-list: ((&KEY (INIT (QUOTE (LIST))) (TEST (QUOTE NOT))
 ;                  (LIM 1000) (CRES (GENSYM CRES)) (CITR (GENSYM CITR)))
 ;                 &BODY BODY)
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:compile-query`
```
:missing:

 ; GRPH:COMPILE-QUERY
 ;   [symbol]
```

## `grph:connected-verts`
```
 ; GRPH:CONNECTED-VERTS
 ;   [symbol]
 ; 
 ; CONNECTED-VERTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Documentation:
 ;     get all connected verts.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:dead-ends`
```
 ; GRPH:DEAD-ENDS
 ;   [symbol]
 ; 
 ; DEAD-ENDS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that have exactly one adjacent verts: [g-] ?y-?x ignores edge dir.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:del`
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
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:del!`
```
 ; GRPH:DEL!
 ;   [symbol]
 ; 
 ; DEL! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL P)
 ;   Documentation:
 ;     del edge and re-bind. returns: deleted?
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:del-dead-ends`
```
 ; GRPH:DEL-DEAD-ENDS
 ;   [symbol]
 ; 
 ; DEL-DEAD-ENDS names a compiled function:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Derived type: (FUNCTION (T &OPTIONAL SYMBOL)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     delete dead-ends until there are no more dead ends left. ignores edge dir.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:del-dead-ends!`
```
:missing:

 ; GRPH:DEL-DEAD-ENDS!
 ;   [symbol]
```

## `grph:del-prop`
```
 ; GRPH:DEL-PROP
 ;   [symbol]
 ; 
 ; DEL-PROP names a compiled function:
 ;   Lambda-list: (G K PROP)
 ;   Derived type: (FUNCTION (GRPH:GRPH (OR LIST FIXNUM) SYMBOL)
 ;                  (VALUES GRPH:GRPH BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     delete prop from k.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:del-props`
```
 ; GRPH:DEL-PROPS
 ;   [symbol]
 ; 
 ; DEL-PROPS names a compiled function:
 ;   Lambda-list: (G K PROPS)
 ;   Derived type: (FUNCTION (T (OR LIST FIXNUM) (OR LIST FSET:SET))
 ;                  (VALUES GRPH:GRPH T &OPTIONAL))
 ;   Documentation:
 ;     delete props from k
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:del-props!`
```
 ; GRPH:DEL-PROPS!
 ;   [symbol]
 ; 
 ; DEL-PROPS! names a macro:
 ;   Lambda-list: (G K P)
 ;   Documentation:
 ;     del edge and re-bind. returns: deleted?
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:distinct`
```
 ; GRPH:DISTINCT
 ;   [symbol]
 ; 
 ; DISTINCT names a compiled function:
 ;   Lambda-list: (&REST REST &AUX (N (LENGTH REST)))
 ;   Derived type: (FUNCTION * (VALUES BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     t if values in rest are distinct.
 ;   Source file: /home/anders/x/grph/src/qry-runtime.lisp
```

## `grph:edge-set`
```
 ; GRPH:EDGE-SET
 ;   [symbol]
 ; 
 ; EDGE-SET names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _))
 ;   Documentation:
 ;     get [normalized] edge set. ignores edge dir.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:edge-set->ht`
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
 ;     convert edge set to hash table. normalize all edges to have the smallest index first.
 ;   Source file: /home/anders/x/grph/src/edge-set.lisp
```

## `grph:edge-set->path`
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
 ;   Source file: /home/anders/x/grph/src/edge-set.lisp
```

## `grph:ensure-list`
```
 ; GRPH:ENSURE-LIST
 ;   [symbol]
 ; 
 ; ENSURE-LIST names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (T) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return l if l is a nil/list. otherwise return (list l).
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:es/normalize`
```
 ; GRPH:ES/NORMALIZE
 ;   [symbol]
 ; 
 ; ES/NORMALIZE names a compiled function:
 ;   Lambda-list: (ES)
 ;   Derived type: (FUNCTION (T) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     all edges are rotated to have the smallest index first. duplicates are removed.
 ;   Source file: /home/anders/x/grph/src/edge-set.lisp
```

## `grph:ext-symbols?`
```
 ; GRPH:EXT-SYMBOLS?
 ;   [symbol]
 ; 
 ; EXT-SYMBOLS? names a macro:
 ;   Lambda-list: (PKG &OPTIONAL MODE)
 ;   Documentation:
 ;     list all external symbols in pkg. use :verbose to inlcude docstring.
 ;     use :pretty to print verbose output to stdout in a readable form.
 ;   Source file: /home/anders/x/grph/src/docs.lisp
```

## `grph:first<`
```
 ; GRPH:FIRST<
 ;   [symbol]
 ; 
 ; FIRST< names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     equvialent to (and (< r1 r2) (< r1 r3) ...). r1 is evaluated only once.
 ;   Source file: /home/anders/x/grph/src/qry-runtime.lisp
```

## `grph:first>`
```
 ; GRPH:FIRST>
 ;   [symbol]
 ; 
 ; FIRST> names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     equvialent to (and (> r1 r2) (> r1 r3) ...). r1 is evaluated only once.
 ;   Source file: /home/anders/x/grph/src/qry-runtime.lisp
```

## `grph:gather-match`
```
 ; GRPH:GATHER-MATCH
 ;   [symbol]
 ; 
 ; GATHER-MATCH names a macro:
 ;   Lambda-list: (G L P R)
 ;   Documentation:
 ;     return list of matches for (l p r).
 ;   Source file: /home/anders/x/grph/src/qry-match.lisp
```

## `grph:grph`
```
:missing:

 ; GRPH:GRPH
 ;   [symbol]
 ; 
 ; GRPH names a compiled function:
 ;   A constructor for GRPH:GRPH
 ;   Lambda-list: (&OPTIONAL (ADJ NILMAP) (NUM-EDGES 0) (PROPS NILMAP)
 ;                 (MID NILMAP))
 ;   Derived type: (FUNCTION
 ;                  (&OPTIONAL FSET:MAP (UNSIGNED-BYTE 32) FSET:MAP
 ;                   FSET:MAP)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/grph/src/grph.lisp
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

## `grph:ht->edge-set`
```
 ; GRPH:HT->EDGE-SET
 ;   [symbol]
 ; 
 ; HT->EDGE-SET names a compiled function:
 ;   Lambda-list: (HT)
 ;   Derived type: (FUNCTION (HASH-TABLE) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     inverse of edge-set->ht.
 ;   Source file: /home/anders/x/grph/src/edge-set.lisp
```

## `grph:ingest-edges`
```
 ; GRPH:INGEST-EDGES
 ;   [symbol]
 ; 
 ; INGEST-EDGES names a compiled function:
 ;   Lambda-list: (EDGES &OPTIONAL (G (GRPH)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL T)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     ingest a list of edges with props. eg: ((0 :a 3) ...). and return a grph.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:ingest-props-edges`
```
 ; GRPH:INGEST-PROPS-EDGES
 ;   [symbol]
 ; 
 ; INGEST-PROPS-EDGES names a compiled function:
 ;   Lambda-list: (PEDGES &OPTIONAL (G (GRPH)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL T)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     ingest list of props and flattened edges. see props-edges.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:itr-adj`
```
 ; GRPH:ITR-ADJ
 ;   [symbol]
 ; 
 ; ITR-ADJ names a macro:
 ;   Lambda-list: ((G A B &OPTIONAL (MODES ->)) &BODY BODY)
 ;   Documentation:
 ;     iterate all adjacent verts, b, of a. modes: (-> <- >< <>).
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:itr-edges`
```
 ; GRPH:ITR-EDGES
 ;   [symbol]
 ; 
 ; ITR-EDGES names a macro:
 ;   Lambda-list: ((G A &OPTIONAL B) &BODY BODY)
 ;   Documentation:
 ;     iterate all edges, as either a=(v1 v2) or a=v1, b=v2.
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:itr-verts`
```
 ; GRPH:ITR-VERTS
 ;   [symbol]
 ; 
 ; ITR-VERTS names a macro:
 ;   Lambda-list: ((G A) &BODY BODY)
 ;   Documentation:
 ;     iterate all connected verts, as a.
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:ladd*!`
```
:missing:

 ; GRPH:LADD*!
 ;   [symbol]
 ; 
 ; LADD*! names a macro:
 ;   Lambda-list: (G E &OPTIONAL (MODES ->) PROPS)
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:last*`
```
 ; GRPH:LAST*
 ;   [symbol]
 ; 
 ; LAST* names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     last item in list.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:ldel!`
```
 ; GRPH:LDEL!
 ;   [symbol]
 ; 
 ; LDEL! names a macro:
 ;   Lambda-list: (G E &OPTIONAL P)
 ;   Documentation:
 ;     del edge ab=(a b) and re-bind. returns: deleted?
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:lqry`
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
 ;   Source file: /home/anders/x/grph/src/qry.lisp
```

## `grph:lsort`
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
 ;   Source file: /home/anders/x/grph/src/qry-runtime.lisp
```

## `grph:lst->map`
```
 ; GRPH:LST->MAP
 ;   [symbol]
 ; 
 ; LST->MAP names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert fset:map to list.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:lst->set`
```
 ; GRPH:LST->SET
 ;   [symbol]
 ; 
 ; LST->SET names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert list to fset:set.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:lst->set-fx`
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
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:make`
```
:missing:

 ; GRPH:MAKE
 ;   [symbol]
 ; 
 ; MAKE names a compiled function:
 ;   A constructor for GRPH:GRPH
 ;   Lambda-list: (&OPTIONAL (ADJ NILMAP) (NUM-EDGES 0) (PROPS NILMAP)
 ;                 (MID NILMAP))
 ;   Derived type: (FUNCTION
 ;                  (&OPTIONAL FSET:MAP (UNSIGNED-BYTE 32) FSET:MAP
 ;                   FSET:MAP)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:map->lst`
```
 ; GRPH:MAP->LST
 ;   [symbol]
 ; 
 ; MAP->LST names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert fset:map to list.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:match`
```
 ; GRPH:MATCH
 ;   [symbol]
 ; 
 ; MATCH names a macro:
 ;   Lambda-list: ((G F LFT MID RHT) &BODY BODY)
 ;   Documentation:
 ;     execute body with alist f with vars for every fact in the graph
 ;     that matches the pattern (lft mid rht). f is on the form ((?A . 0) (?P . :a)).
 ;   Source file: /home/anders/x/grph/src/qry-match.lisp
```

## `grph:memo`
```
 ; GRPH:MEMO
 ;   [symbol]
 ; 
 ; MEMO names a compiled function:
 ;   Lambda-list: (FX &AUX (HT (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION (FUNCTION) (VALUES FUNCTION &OPTIONAL))
 ;   Documentation:
 ;     return function that memoizes calls to fx.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:modify!`
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
 ;     transaction operations:
 ;      - ([sym]-cancel) : aborts the transaction,
 ;      - ([sym]-stop)   : stops the transaction, but keeps the changes,
 ;     
 ;     ex: (modify! (g mygrp)
 ;           (loop for a = (rnd:rndi n) for b = (rnd:rndi n) repeat 10
 ;                 do (rnd:either (mygrp-> a b '(:x :c))
 ;                                (mygrp<> a b '(:y :d)))))
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:multi-isects`
```
 ; GRPH:MULTI-ISECTS
 ;   [symbol]
 ; 
 ; MULTI-ISECTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that have 3 or more adjacent verts. ignores edge dir.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:normalize-edges`
```
 ; GRPH:NORMALIZE-EDGES
 ;   [symbol]
 ; 
 ; NORMALIZE-EDGES names a compiled function:
 ;   Lambda-list: (G &OPTIONAL (MODE ><))
 ;   Derived type: (FUNCTION (T &OPTIONAL KEYWORD)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     remove bi-directional edges, preserve properties.
 ;     
 ;     ->: normalize by removing ba if b > a.
 ;         ensure that ab exists. and inherits props from ba
 ;     ><: only normalize (to ab) if both ab and ba exists.
 ;         otherwise preserve edge direction.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:normalize-edges!`
```
 ; GRPH:NORMALIZE-EDGES!
 ;   [symbol]
 ; 
 ; NORMALIZE-EDGES! names a macro:
 ;   Lambda-list: (G &OPTIONAL (MODE ><))
 ;   Documentation:
 ;     normalize with normalize-edges (see this).
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:num-either`
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
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:path!`
```
 ; GRPH:PATH!
 ;   [symbol]
 ; 
 ; PATH! names a macro:
 ;   Lambda-list: (G PATH &OPTIONAL (MODES (QUOTE (OPEN ->))) PROPS)
 ;   Documentation:
 ;     add path (a b c ...). modes (-> <- <> open closed)
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:path->edge-set`
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
 ;     if closed is t, (5 1) will be included in the above output.
 ;   Source file: /home/anders/x/grph/src/edge-set.lisp
```

## `grph:prop`
```
 ; GRPH:PROP
 ;   [symbol]
 ; 
 ; PROP names a compiled function:
 ;   Lambda-list: (G K PROPS)
 ;   Derived type: (FUNCTION
 ;                  (T (OR LIST (SIGNED-BYTE 32))
 ;                   (OR LIST KEYWORD FSET:SET))
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     set prop, p, of edge or vert, k.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:prop!`
```
 ; GRPH:PROP!
 ;   [symbol]
 ; 
 ; PROP! names a macro:
 ;   Lambda-list: (G K PROPS)
 ;   Documentation:
 ;     add edge/vert prop for key, k.
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:props-edges`
```
 ; GRPH:PROPS-EDGES
 ;   [symbol]
 ; 
 ; PROPS-EDGES names a compiled function:
 ;   Lambda-list: (G)
 ;   Derived type: (FUNCTION (GRPH:GRPH) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     list of lists of prop with flattend list of edges. see ingest-props-edges
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:prt`
```
:missing:

 ; GRPH:PRT
 ;   [symbol]
 ; 
 ; PRT names a compiled function:
 ;   Lambda-list: (O &OPTIONAL S)
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES (OR STRING NULL) &OPTIONAL))
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:qry`
```
 ; GRPH:QRY
 ;   [symbol]
 ; 
 ; QRY names a macro:
 ;   Lambda-list: (G &KEY DB IN USING SELECT WHERE COLLECT THEN FIRST
 ;                 PAIRS (PAR *PARALLEL*))
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
 ;   Source file: /home/anders/x/grph/src/qry.lisp
```

## `grph:qry-collect-while`
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
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:relneigh`
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
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:rqry`
```
 ; GRPH:RQRY
 ;   [symbol]
 ; 
 ; RQRY names a macro:
 ;   Lambda-list: (G &KEY (LIM 1000) RULES THEN (PAR *PARALLEL*))
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
 ;   Source file: /home/anders/x/grph/src/qry-rules.lisp
```

## `grph:segment-isects`
```
 ; GRPH:SEGMENT-ISECTS
 ;   [symbol]
 ; 
 ; SEGMENT-ISECTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that do not have exactly 2 adjacent verts. ie. the set of dead
 ;     ends and multi isects. ignores edge dir.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:set->lst`
```
 ; GRPH:SET->LST
 ;   [symbol]
 ; 
 ; SET->LST names a macro:
 ;   Lambda-list: (F)
 ;   Documentation:
 ;     convert fset:set to list.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:set->lst-fx`
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
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:split!`
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
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:sprop`
```
 ; GRPH:SPROP
 ;   [symbol]
 ; 
 ; SPROP names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: FUNCTION
 ;   Documentation:
 ;     make a special prop. special props have a distinct prefix: :/g/.
 ;     and a category. eg: :/g/id/. where :id is the category.
 ;     special props behave like all other props, but they can have special
 ;     behaviour in some limited cases.
 ;     
 ;     see compound paths in grph:walk macro.
 ;     
 ;     possible future special props are types. eg
 ;       :/g/bzspl/ for bezier curves.
 ;     or even :/g/circ/, where one edge denotes a center and radius. which would work
 ;     well w/ svg export.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:sprop-id`
```
 ; GRPH:SPROP-ID
 ;   [symbol]
 ; 
 ; SPROP-ID names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SP-CAT ^SID^))
 ;   Derived type: (FUNCTION (&OPTIONAL T) *)
 ;   Documentation:
 ;     generate a unique special prop :/g/id/[gensym].
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:sprop?`
```
 ; GRPH:SPROP?
 ;   [symbol]
 ; 
 ; SPROP? names a compiled function:
 ;   Lambda-list: (S)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     is this a special prop? returns s or nil.
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:stop`
```
(stop) can be used in some contexts (using, qry) to stop
the transaction, but keep the changes
```

## `grph:to-vector`
```
 ; GRPH:TO-VECTOR
 ;   [symbol]
 ; 
 ; TO-VECTOR names a compiled function:
 ;   Lambda-list: (INIT &OPTIONAL (TYPE (QUOTE LIST)))
 ;   Derived type: (FUNCTION (LIST &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
 ;   Documentation:
 ;     make non-adjustable array with init contents.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:two-isects`
```
 ; GRPH:TWO-ISECTS
 ;   [symbol]
 ; 
 ; TWO-ISECTS names a macro:
 ;   Lambda-list: (G &OPTIONAL (P _) Y)
 ;   Documentation:
 ;     verts that have exactly 2 adjacent verts [g-] ?y1-?x-?y2 [-g] ignores edge dir.
 ;   Source file: /home/anders/x/grph/src/grph-queries.lisp
```

## `grph:unpack-sprop`
```
 ; GRPH:UNPACK-SPROP
 ;   [symbol]
 ; 
 ; UNPACK-SPROP names a compiled function:
 ;   Lambda-list: (S)
 ;   Derived type: (FUNCTION (KEYWORD) (VALUES T T &OPTIONAL))
 ;   Documentation:
 ;     unpack eg. :/g/id/abc into values :id and :abc
 ;   Source file: /home/anders/x/grph/src/grph.lisp
```

## `grph:using`
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
 ;   Source file: /home/anders/x/grph/src/macros.lisp
```

## `grph:v?`
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
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:vector-first`
```
 ; GRPH:VECTOR-FIRST
 ;   [symbol]
 ; 
 ; VECTOR-FIRST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (VECTOR) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     first element of vector.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:vector-last`
```
 ; GRPH:VECTOR-LAST
 ;   [symbol]
 ; 
 ; VECTOR-LAST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (VECTOR) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     last element of vector.
 ;   Source file: /home/anders/x/grph/src/utils.lisp
```

## `grph:walk`
```
 ; GRPH:WALK
 ;   [symbol]
 ; 
 ; WALK names a macro:
 ;   Lambda-list: ((G &OPTIONAL (P (GENSYM PATH)) (C (GENSYM CLOSED?))
 ;                  (SID (GENSYM SID)))
 ;                 (MODES &KEY (PROP _) (ES NIL) (SP-CAT ID)
 ;                  (SP-DEFAULT /G/_DEFAULT/))
 ;                 &BODY BODY)
 ;   Documentation:
 ;     walk edges in graph as tuples of (p c). where p is a path and c is t if p is closed.
 ;     
 ;     ex:
 ;     
 ;       (grph:walk (g) (collect prop path))
 ;     
 ;       (grph:walk (g p c)
 ;                  ((dir segments) :prop :path)
 ;         (print (list (reverse p) c)))
 ;     
 ;     modes:
 ;     
 ;     * :progn    : don't collect result.                                           [default]
 ;       :collect  : collect results as a list.
 ;     
 ;     * :paths    : greedily walk to make as long paths as possible. verts can      [default]
 ;                   be visited multiple times. handles [pure] loops.
 ;       :segments : split paths into segments. handles [pure] loops
 ;       :edges    : just return edges
 ;     
 ;     * :simple   : dont group by special prop                                      [default]
 ;       :compound : group by special prop [cat :id / :/g/id/]
 ;     * :keep     : keep non-compond paths in :/g/_default/                         [default]
 ;       :drop     : drop non-compound paths
 ;     
 ;     * :any      : use paths as they come out of the walker                        [default]
 ;       :dir      : greedily attempt to align path direction with edges in the
 ;                   graph by comparing the first edge in p with the graph, and
 ;                   aligning the path to the edge direction.
 ;                   useful for loops, and with the :segments mode.
 ; 
 ;   Source file: /home/anders/x/grph/src/grph-walk.lisp
```

