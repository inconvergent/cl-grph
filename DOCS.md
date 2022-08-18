# grph DOCUMENTATION

### Explanation

:todo:

### Symbols

To load "grph":
  Load 1 ASDF system:
    grph
; Loading "grph"

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
 ;   Source file: src/grph.lisp
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
 ;   Source file: src/grph.lisp
```

#### GRPH:@INV

```
get val of prop, p, for key, k. should be an edge k=(a b) or a vert, k=a.

 ; GRPH:@INV
 ;   [symbol]
 ;
 ; @INV names a compiled function:
 ;   Lambda-list: (G K &OPTIONAL P)
 ;   Derived type: (FUNCTION (GRPH:GRPH T &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get val of prop, p, for key, k. should be an edge k=(a b) or a vert, k=a.
 ;   Source file: src/grph.lisp
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
 ;   Source file: src/grph.lisp
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
 ;   Source file: src/grph.lisp
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
 ;   Source file: src/grph.lisp
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
 ;   Source file: src/grph.lisp
```

#### GRPH:@VNUM

```
list all connected verts.

 ; GRPH:@VNUM
 ;   [symbol]
 ;
 ; @VNUM names a compiled function:
 ;   Lambda-list: (G &AUX (RES 0))
 ;   Derived type: (FUNCTION (GRPH:GRPH)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     list all connected verts.
 ;   Source file: src/grph.lisp
```

#### GRPH:ADD

```
new edge (a b). optionally set prop, p, (with val).

 ; GRPH:ADD
 ;   [symbol]
 ;
 ; ADD names a compiled function:
 ;   Lambda-list: (G A B &OPTIONAL P (VAL T))
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH (UNSIGNED-BYTE 31) (UNSIGNED-BYTE 31)
 ;                             &OPTIONAL T T)
 ;                  (VALUES (OR NULL GRPH:GRPH) BOOLEAN &OPTIONAL))
 ;   Documentation:
 ;     new edge (a b). optionally set prop, p, (with val).
 ;   Source file: src/grph.lisp
```

#### GRPH:ADD!

```
add edge edge and re-bind.

 ; GRPH:ADD!
 ;   [symbol]
 ;
 ; ADD! names a macro:
 ;   Lambda-list: (G A B &OPTIONAL PROP (VAL T))
 ;   Documentation:
 ;     add edge edge and re-bind.
 ;   Source file: src/macros.lisp
```

#### GRPH:COMPILE-QUERY

```
compile a datalog query.

facts reduce should be the name of a function that will be used to
consider all relevant facts for a given stage. see facts-qry for an example.

 ; GRPH:COMPILE-QUERY
 ;   [symbol]
 ;
 ; COMPILE-QUERY names a macro:
 ;   Lambda-list: (FACT-REDUCE &KEY WHERE (SELECT (ALL-VARS WHERE)) IN)
 ;   Documentation:
 ;     compile a datalog query.
 ;
 ;     facts reduce should be the name of a function that will be used to
 ;     consider all relevant facts for a given stage. see facts-qry for an example.
 ;   Source file: src/qry.lisp
```

#### GRPH:DEL

```
delete edge (a b). deletes associated props.

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
 ;   Source file: src/grph.lisp
```

#### GRPH:DEL!

```
del edge and re-bind.

 ; GRPH:DEL!
 ;   [symbol]
 ;
 ; DEL! names a macro:
 ;   Lambda-list: (G A B)
 ;   Documentation:
 ;     del edge and re-bind.
 ;   Source file: src/macros.lisp
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
 ;   Source file: src/docs.lisp
```

#### GRPH:FACTS-QRY

```
run this datalog qry on all input facts.

 ; GRPH:FACTS-QRY
 ;   [symbol]
 ;
 ; FACTS-QRY names a macro:
 ;   Lambda-list: (FACTS &REST QRY)
 ;   Documentation:
 ;     run this datalog qry on all input facts.
 ;   Source file: src/qry.lisp
```

#### GRPH:GRPH

```
:missing:todo:

 ; GRPH:GRPH
 ;   [symbol]
 ;
 ; GRPH names a compiled function:
 ;   Lambda-list: (&OPTIONAL (ADJ NILMAP) (NUM-EDGES 0) (PROPS NILMAP)
 ;                 (INV NILMAP))
 ;   Derived type: (FUNCTION
 ;                  (&OPTIONAL FSET:MAP (UNSIGNED-BYTE 31) FSET:MAP
 ;                   FSET:MAP)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/grph.lisp
 ;
 ; GRPH names the structure-class #<STRUCTURE-CLASS GRPH:GRPH>:
 ;   Documentation:
 ;     create undirected graph instance with no spatial awareness.
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
 ;   Class precedence-list: GRPH:GRPH, STRUCTURE-OBJECT,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Slots:
 ;     GRPH::ADJ
 ;       Type: FSET:MAP
 ;       Initform: GRPH::NILMAP
 ;     GRPH::PROPS
 ;       Type: FSET:MAP
 ;       Initform: GRPH::NILMAP
 ;     GRPH::INV
 ;       Type: FSET:MAP
 ;       Initform: GRPH::NILMAP
 ;     GRPH::NUM-EDGES
 ;       Type: GRPH::PN
 ;       Initform: 0
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
 ;   Source file: src/macros.lisp
```

#### GRPH:ITR-OUT

```
iterate all outboud verts, b, of a.

 ; GRPH:ITR-OUT
 ;   [symbol]
 ;
 ; ITR-OUT names a macro:
 ;   Lambda-list: ((G A B) &BODY BODY)
 ;   Documentation:
 ;     iterate all outboud verts, b, of a.
 ;   Source file: src/macros.lisp
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
 ;   Source file: src/macros.lisp
```

#### GRPH:PROP

```
set prop, p, of edge or vert, k.

 ; GRPH:PROP
 ;   [symbol]
 ;
 ; PROP names a compiled function:
 ;   Lambda-list: (G K P &OPTIONAL (VAL T))
 ;   Derived type: (FUNCTION (GRPH:GRPH LIST SYMBOL &OPTIONAL T)
 ;                  (VALUES GRPH:GRPH &OPTIONAL))
 ;   Documentation:
 ;     set prop, p, of edge or vert, k.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/grph.lisp
```

#### GRPH:PROP!

```
set prop and re-bind.

 ; GRPH:PROP!
 ;   [symbol]
 ;
 ; PROP! names a macro:
 ;   Lambda-list: (G K PROP &OPTIONAL (VAL T))
 ;   Documentation:
 ;     set prop and re-bind.
 ;   Source file: src/macros.lisp
```

