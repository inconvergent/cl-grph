(defpackage #:grph
  (:use #:common-lisp)
  (:nicknames #:cl-grph)
  (:import-from #:fset
    #:@ #:contains? #:less #:empty-set #:empty-map #:do-map #:do-set)
  (:export
    #:*dir-mode* #:*pos-mode*
    #:mvb #:dsb #:mvc
    #:ext-symbols? #:pn
    #:@edges #:@enum #:@in #:@out #:@either #:@both
    #:@verts #:@vnum #:@vmax #:@prop #:@mem #:@inv
    #:add #:del #:add! #:del!
    #:itr-edges #:itr-adj #:itr-verts
    #:grph #:prt
    #:compile-query #:match
    #:qry #:qry-collect-while #:stop #:cancel #:collect-while
    #:rqry
    #:edge-set->path
    #:gather-match #:ingest-edges
    #:grp #:distinct #:first< #:first> #:lsort
    #:walk-grph #:any-edge #:dead-ends #:multi-isects #:filament-ends))

(defpackage #:xgrph
  (:use #:common-lisp)
  (:import-from #:fset
    #:empty-seq #:seq)
  (:import-from #:grph
    #:*dir-mode* #:*pos-mode* #:mvb #:dsb #:mvc #:pn #:add! #:del!)
  (:export
    #:@verts #:2@verts #:3@verts
    #:@vert #:2@vert #:3@vert
    #:@num #:2@num #:3@num
    #:@ #:2@ #:3@
    #:pos
    #:vert! #:2vert! #:3vert!
    #:verts! #:2verts! #:3verts!
    #:move! #:2move! #:3move!
    #:vset! #:2vset! #:3vset!
    #:path! #:2path! #:3path!
    #:split! #:2split! #:3split!
    #:append! #:2append! #:3append!
    #:append-inv! #:2append-inv! #:3append-inv!))

