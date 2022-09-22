
(defpackage #:grph
  (:use #:common-lisp)
  (:import-from #:fset #:@ #:contains? #:less #:empty-set
                #:empty-map #:do-map #:do-set)
  (:nicknames #:cl-grph)
  (:export
    #:ext-symbols? #:pn
    #:@edges #:@enum #:@in #:@out #:@verts #:@vnum #:@prop #:@mem #:@inv
    #:add #:del #:add! #:del! #:prop #:prop!
    #:itr-edges #:itr-adj #:itr-verts
    #:grph #:prt
    #:compile-query #:match
    #:qry #:qry-collect-while
    #:stop #:cancel
    #:collect-while
    #:edge-set->path
    #:grp #:distinct #:smallest-first))

(defpackage #:xgrph
  (:use #:common-lisp)
  (:export #:@verts #:2@verts #:3@verts
           #:@vert #:2@vert #:3@vert
           #:@num #:2@num #:3@num
           #:@ #:2@ #:3@
           #:pos
           #:vert! #:2vert! #:3vert!
           #:verts! #:2verts! #:3verts!
           #:move! #:2move! #:3move!
           #:path! #:2path! #:3path!
           #:split! #:2split! #:3split!
           #:append! #:2append! #:3append!
           #:append-inv! #:2append-inv! #:3append-inv!
           ))

