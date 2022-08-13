
(defpackage #:grph
  (:use #:common-lisp)
  (:import-from #:fset #:@ #:contains? #:less #:empty-set
                #:empty-map #:do-map #:do-set)
  (:nicknames #:cl-grph)
  (:export
    #:@edges #:@enum #:@incident #:@verts #:@vnum #:@prop #:@mem
    #:add #:del #:add! #:del!  #:prop #:prop!
    #:itr-edges #:itr-incident #:itr-verts
    #:ext-symbols?
    #:compile-query #:facts-qry
    #:grph))

