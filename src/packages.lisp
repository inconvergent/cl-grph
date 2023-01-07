(defpackage #:grph
  (:use #:common-lisp)
  (:nicknames #:cl-grph)
  (:import-from #:fset
    #:@ #:contains? #:less #:empty-set #:empty-map #:do-map #:do-set)
  (:export
    #:*dir-mode* #:*pos-mode* #:*valid-clauses*
    #:mvb #:dsb #:mvc
    #:ext-symbols? #:pn
    #:@edges #:@enum #:@in #:@out #:@either #:@both #:num-either
    #:@verts #:@vnum #:@vmax #:@prop #:@mem #:@inv
    #:add #:add! #:add*! #:del #:del! #:path! #:del-props #:modify!
    #:itr-edges #:itr-adj #:itr-verts
    #:grph #:prt
    #:compile-query #:match
    #:qry #:qry-collect-while #:stop #:cancel #:collect-while
    #:rqry
    #:to-vector #:ensure-list #:last* #:vector-last #:vector-first
    #:gather-match #:ingest-edges
    #:grp #:distinct #:first< #:first> #:lsort
    #:walk-grph #:walk-edge-set
    #:any-edge #:dead-ends #:multi-isects #:filament-ends
    #:del-simple-filaments
    #:path->edge-set #:edge-set->path
    #:normalise-fold #:normalise-fold!
    #:using
    #:-> #:<- #:<> #:><))

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
    #:pos #:fxpos!
    #:vert! #:2vert! #:3vert!
    #:verts! #:2verts! #:3verts!
    #:move! #:2move! #:3move!
    #:vset! #:2vset! #:3vset!
    #:path! #:2path! #:3path!
    #:split! #:2split! #:3split!
    #:append! #:2append! #:3append!
    #:append-inv! #:2append-inv! #:3append-inv!
    #:2intersect-all #:2intersect-all!
    #:2cut-to-area #:2cut-to-area!
    #:triangulate-edge-set))

