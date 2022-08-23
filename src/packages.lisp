
(defpackage #:grph
  (:use #:common-lisp)
  (:import-from #:fset #:@ #:contains? #:less #:empty-set
                #:empty-map #:do-map #:do-set)
  (:nicknames #:cl-grph)
  (:export
    #:@edges #:@enum #:@in #:@out #:@verts #:@vnum #:@prop #:@mem #:@inv
    #:add #:del #:add! #:del! #:prop #:prop!
    #:itr-edges #:itr-adj #:itr-verts
    #:ext-symbols?
    #:compile-query #:facts-qry #:qry #:prt #:match
    #:grph))

