#### XGRPH:2@

```
:missing:todo:

 ; XGRPH:2@
 ;   [symbol]
 ; 
 ; 2@ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2@NUM

```
:missing:todo:

 ; XGRPH:2@NUM
 ;   [symbol]
 ; 
 ; 2@NUM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2@VERT

```
:missing:todo:

 ; XGRPH:2@VERT
 ;   [symbol]
 ; 
 ; 2@VERT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2@VERTS

```
:missing:todo:

 ; XGRPH:2@VERTS
 ;   [symbol]
 ; 
 ; 2@VERTS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2APPEND!

```
:missing:todo:

 ; XGRPH:2APPEND!
 ;   [symbol]
 ; 
 ; 2APPEND! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2CENTER-CONNECTED

```
 ; XGRPH:2CENTER-CONNECTED
 ;   [symbol]
 ; 
 ; 2CENTER-CONNECTED names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %2CENTER-CONNECTED
 ;     ARGS: (G POS &OPTIONAL (X 0.0) (Y 0.0) MAX-SIDE)
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2CUT

```
 ; XGRPH:2CUT
 ;   [symbol]
 ; 
 ; 2CUT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %2CUT
 ;     ARGS: (G POS (VA 4 LINE))
 ;     DOCSTRING: cut g/pos along line. returns g, pos and a list of (vi si) where vi is a new
 ;     vertex index in pos and s is the lerp along line
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2CUT!

```
 ; XGRPH:2CUT!
 ;   [symbol]
 ; 
 ; 2CUT! names a macro:
 ;   Lambda-list: (G POS &REST REST)
 ;   Documentation:
 ;     cut g/pos along line. returns a list of (vi si) where vi is a new vertex
 ;     index in pos and s is the lerp along line
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2CUT-TO-AREA

```
 ; XGRPH:2CUT-TO-AREA
 ;   [symbol]
 ; 
 ; 2CUT-TO-AREA names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %2CUT-TO-AREA
 ;     ARGS: (G POS &OPTIONAL (TOP 0.0) (LFT 0.0) (RHT 1000.0) (BOT 1000.0))
 ;     DOCSTRING: removes all edges outside envelope.
 ;     all edges intersecting the envelope will be deleted, a new vert will be
 ;     inserted on the intersection; connected to the inside vert.
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2CUT-TO-AREA!

```
:missing:todo:

 ; XGRPH:2CUT-TO-AREA!
 ;   [symbol]
 ; 
 ; 2CUT-TO-AREA! names a macro:
 ;   Lambda-list: (G POS &REST REST)
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2INTERSECT-ALL

```
 ; XGRPH:2INTERSECT-ALL
 ;   [symbol]
 ; 
 ; 2INTERSECT-ALL names a compiled function:
 ;   Lambda-list: (G POS)
 ;   Derived type: (FUNCTION (GRPH:GRPH FSET:SEQ)
 ;                  (VALUES GRPH:GRPH FSET:SEQ &OPTIONAL))
 ;   Documentation:
 ;     creates intersections for all edges in g such that it becomes a planar graph.
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2INTERSECT-ALL!

```
:missing:todo:

 ; XGRPH:2INTERSECT-ALL!
 ;   [symbol]
 ; 
 ; 2INTERSECT-ALL! names a macro:
 ;   Lambda-list: (G POS &REST REST)
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2L@

```
:missing:todo:

 ; XGRPH:2L@
 ;   [symbol]
 ; 
 ; 2L@ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2MIRROR

```
 ; XGRPH:2MIRROR
 ;   [symbol]
 ; 
 ; 2MIRROR names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %2MIRROR
 ;     ARGS: (G POS (VA 2 A B) &OPTIONAL SIDEFX)
 ;     DOCSTRING: mirror around line ab.
 ;     optionally delete edges on the side of ab where (sidefx (cross ab va) 0f0)
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2MIRROR!

```
:missing:todo:

 ; XGRPH:2MIRROR!
 ;   [symbol]
 ; 
 ; 2MIRROR! names a macro:
 ;   Lambda-list: (G POS &REST REST)
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:2MOVE!

```
:missing:todo:

 ; XGRPH:2MOVE!
 ;   [symbol]
 ; 
 ; 2MOVE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2PATH!

```
:missing:todo:

 ; XGRPH:2PATH!
 ;   [symbol]
 ; 
 ; 2PATH! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2SPLIT!

```
:missing:todo:

 ; XGRPH:2SPLIT!
 ;   [symbol]
 ; 
 ; 2SPLIT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2VERT!

```
:missing:todo:

 ; XGRPH:2VERT!
 ;   [symbol]
 ; 
 ; 2VERT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2VERTS!

```
:missing:todo:

 ; XGRPH:2VERTS!
 ;   [symbol]
 ; 
 ; 2VERTS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:2VSET!

```
:missing:todo:

 ; XGRPH:2VSET!
 ;   [symbol]
 ; 
 ; 2VSET! names a macro:
 ;   Lambda-list: (S I POS)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3@

```
:missing:todo:

 ; XGRPH:3@
 ;   [symbol]
 ; 
 ; 3@ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3@NUM

```
:missing:todo:

 ; XGRPH:3@NUM
 ;   [symbol]
 ; 
 ; 3@NUM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3@VERT

```
:missing:todo:

 ; XGRPH:3@VERT
 ;   [symbol]
 ; 
 ; 3@VERT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3@VERTS

```
:missing:todo:

 ; XGRPH:3@VERTS
 ;   [symbol]
 ; 
 ; 3@VERTS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3APPEND!

```
:missing:todo:

 ; XGRPH:3APPEND!
 ;   [symbol]
 ; 
 ; 3APPEND! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3CUT-ALL

```
 ; XGRPH:3CUT-ALL
 ;   [symbol]
 ; 
 ; 3CUT-ALL names a compiled function:
 ;   Lambda-list: (G POS FX)
 ;   Derived type: (FUNCTION (GRPH:GRPH FSET:SEQ FUNCTION)
 ;                  (VALUES GRPH:GRPH FSET:SEQ &OPTIONAL))
 ;   Documentation:
 ;     cut every edge where they intersect in 2d according to projection fx.
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:3CUT-ALL!

```
:missing:todo:

 ; XGRPH:3CUT-ALL!
 ;   [symbol]
 ; 
 ; 3CUT-ALL! names a macro:
 ;   Lambda-list: (G POS FX)
 ;   Source file: /data/x/grph/src/xgrph-isect.lisp
```

#### XGRPH:3L@

```
:missing:todo:

 ; XGRPH:3L@
 ;   [symbol]
 ; 
 ; 3L@ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3MOVE!

```
:missing:todo:

 ; XGRPH:3MOVE!
 ;   [symbol]
 ; 
 ; 3MOVE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3PATH!

```
:missing:todo:

 ; XGRPH:3PATH!
 ;   [symbol]
 ; 
 ; 3PATH! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3SPLIT!

```
:missing:todo:

 ; XGRPH:3SPLIT!
 ;   [symbol]
 ; 
 ; 3SPLIT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3VERT!

```
:missing:todo:

 ; XGRPH:3VERT!
 ;   [symbol]
 ; 
 ; 3VERT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3VERTS!

```
:missing:todo:

 ; XGRPH:3VERTS!
 ;   [symbol]
 ; 
 ; 3VERTS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:3VSET!

```
:missing:todo:

 ; XGRPH:3VSET!
 ;   [symbol]
 ; 
 ; 3VSET! names a macro:
 ;   Lambda-list: (S I POS)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:@

```
:missing:todo:

 ; XGRPH:@
 ;   [symbol]
 ; 
 ; @ names a macro:
 ;   Lambda-list: (DIM S &REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:@NUM

```
:missing:todo:

 ; XGRPH:@NUM
 ;   [symbol]
 ; 
 ; @NUM names a macro:
 ;   Lambda-list: (DIM S)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:@VERT

```
 ; XGRPH:@VERT
 ;   [symbol]
 ; 
 ; @VERT names a macro:
 ;   Lambda-list: (DIM S I)
 ;   Documentation:
 ;     get vert i as values.
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:@VERTS

```
 ; XGRPH:@VERTS
 ;   [symbol]
 ; 
 ; @VERTS names a macro:
 ;   Lambda-list: (DIM S L)
 ;   Documentation:
 ;     get verts in l as fvec.
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:APPEND!

```
:missing:todo:

 ; XGRPH:APPEND!
 ;   [symbol]
 ; 
 ; APPEND! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:FXPOS!

```
:missing:todo:

 ; XGRPH:FXPOS!
 ;   [symbol]
 ; 
 ; FXPOS! names a macro:
 ;   Lambda-list: ((G POS I) &BODY BODY)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:L@

```
:missing:todo:

 ; XGRPH:L@
 ;   [symbol]
 ; 
 ; L@ names a macro:
 ;   Lambda-list: (DIM S &REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:MOVE!

```
:missing:todo:

 ; XGRPH:MOVE!
 ;   [symbol]
 ; 
 ; MOVE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:PATH!

```
 ; XGRPH:PATH!
 ;   [symbol]
 ; 
 ; PATH! names a macro:
 ;   Lambda-list: (DIM G S PATH &OPTIONAL MODES PROPS)
 ;   Documentation:
 ;     add path. modes: (-> <- <>)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:POS

```
 ; XGRPH:POS
 ;   [symbol]
 ; 
 ; POS names a compiled function:
 ;   Lambda-list: (&OPTIONAL (D 0.0))
 ;   Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
 ;                  (VALUES FSET:WB-SEQ &OPTIONAL))
 ;   Documentation:
 ;     initialze xgrph pos. d is the default fset:seq value.
 ;   Source file: /data/x/grph/src/xgrph.lisp
 ; 
 ; POS names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: FSET:SEQ
```

#### XGRPH:SPLIT!

```
 ; XGRPH:SPLIT!
 ;   [symbol]
 ; 
 ; SPLIT! names a macro:
 ;   Lambda-list: (DIM G S A B X)
 ;   Documentation:
 ;     delete edge (a b) and add edges (a x) (x b).
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:TRIANGULATE-EDGE-SET

```
 ; XGRPH:TRIANGULATE-EDGE-SET
 ;   [symbol]
 ; 
 ; TRIANGULATE-EDGE-SET names a compiled function:
 ;   Lambda-list: (EDGE-SET FX)
 ;   Derived type: (FUNCTION (LIST T) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     triangulate the hull defined by edge set, using the fx provided where
 ;       (funcall fx i ... k) is (values pix piy ... pkx pky)
 ;   Source file: /data/x/grph/src/xgrph-triangulate.lisp
```

#### XGRPH:VERT!

```
 ; XGRPH:VERT!
 ;   [symbol]
 ; 
 ; VERT! names a macro:
 ;   Lambda-list: (DIM S &REST REST)
 ;   Documentation:
 ;     add vert. from these values.
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:VERTS!

```
 ; XGRPH:VERTS!
 ;   [symbol]
 ; 
 ; VERTS! names a macro:
 ;   Lambda-list: (DIM S PATH)
 ;   Documentation:
 ;     add verts. from path of type fvec.
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

#### XGRPH:VSET!

```
:missing:todo:

 ; XGRPH:VSET!
 ;   [symbol]
 ; 
 ; VSET! names a macro:
 ;   Lambda-list: (S I POS)
 ;   Source file: /data/x/grph/src/xgrph.lisp
```

