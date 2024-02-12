#### GRPH/IO:EXPORT-DAT

```
 ; GRPH/IO:EXPORT-DAT
 ;   [symbol]
 ; 
 ; EXPORT-DAT names a compiled function:
 ;   Lambda-list: (FN O &OPTIONAL (PFX .dat))
 ;   Derived type: (FUNCTION (STRING T &OPTIONAL STRING)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     write o to fn. see import-dat.
 ;   Source file: /data/x/grph/src/xgrph-io.lisp
```

#### GRPH/IO:GEXPORT

```
 ; GRPH/IO:GEXPORT
 ;   [symbol]
 ; 
 ; GEXPORT names a compiled function:
 ;   Lambda-list: (G &KEY (POS (POS 0.0)) (DIM 2) META)
 ;   Derived type: (FUNCTION
 ;                  (GRPH:GRPH &KEY (:POS FSET:SEQ)
 ;                             (:DIM (UNSIGNED-BYTE 32)) (:META LIST))
 ;                  (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     serialize g. see gimport.
 ;   Source file: /data/x/grph/src/xgrph-io.lisp
```

#### GRPH/IO:GIMPORT

```
 ; GRPH/IO:GIMPORT
 ;   [symbol]
 ; 
 ; GIMPORT names a compiled function:
 ;   Lambda-list: (O)
 ;   Derived type: (FUNCTION (LIST) (VALUES T T T &OPTIONAL))
 ;   Documentation:
 ;     deserialize g. see gexport.
 ;   Source file: /data/x/grph/src/xgrph-io.lisp
```

#### GRPH/IO:GREAD

```
 ; GRPH/IO:GREAD
 ;   [symbol]
 ; 
 ; GREAD names a compiled function:
 ;   Lambda-list: (FN)
 ;   Derived type: (FUNCTION (STRING) *)
 ;   Documentation:
 ;     read grph from fn. see gwrite.
 ;   Source file: /data/x/grph/src/xgrph-io.lisp
```

#### GRPH/IO:GWRITE

```
 ; GRPH/IO:GWRITE
 ;   [symbol]
 ; 
 ; GWRITE names a compiled function:
 ;   Lambda-list: (FN G &KEY (POS (POS 0.0)) (DIM 2) META)
 ;   Derived type: (FUNCTION
 ;                  (STRING GRPH:GRPH &KEY (:POS FSET:SEQ)
 ;                          (:DIM (UNSIGNED-BYTE 32)) (:META LIST))
 ;                  *)
 ;   Documentation:
 ;     write grph to fn. see gread.
 ;   Source file: /data/x/grph/src/xgrph-io.lisp
```

#### GRPH/IO:GWRITE-SCRIPT

```
 ; GRPH/IO:GWRITE-SCRIPT
 ;   [symbol]
 ; 
 ; GWRITE-SCRIPT names a macro:
 ;   Lambda-list: ((FN G &KEY (POS (POS 0.0)) (DIM 2) META) &BODY BODY)
 ;   Documentation:
 ;     write grph and body (:script) to fn.
 ;   Source file: /data/x/grph/src/xgrph-io.lisp
```

#### GRPH/IO:IMPORT-DAT

```
 ; GRPH/IO:IMPORT-DAT
 ;   [symbol]
 ; 
 ; IMPORT-DAT names a compiled function:
 ;   Lambda-list: (FN &OPTIONAL (PFX .dat))
 ;   Derived type: (FUNCTION (STRING &OPTIONAL STRING)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read data from fn. see export-dat.
 ;   Source file: /data/x/grph/src/xgrph-io.lisp
```

