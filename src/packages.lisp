(defpackage #:grph
  (:use #:common-lisp)
  (:nicknames #:cl-grph)
  (:import-from #:fset
    #:@ #:contains? #:less #:empty-set #:empty-map #:do-map #:do-set)
  (:export
    #:*dir-mode* #:*pos-mode* #:*valid-clauses*
    #:mvb #:dsb #:mvc
    #:to-vector #:ensure-list #:last* #:vector-last #:vector-first
    #:ext-symbols? #:pn
    #:@edges #:@enum #:@in #:@out #:@either #:@both #:num-either
    #:@verts #:@vcnt #:@vmax #:@prop #:@mem #:@inv
    #:add #:add! #:add*! #:del #:del! #:ldel! #:path! #:del-props #:modify!
    #:itr-edges #:itr-adj #:itr-verts
    #:grph #:make #:prt
    #:compile-query #:match
    #:qry #:lqry #:rqry
    #:stop #:cancel
    #:collect-while
    #:qry-collect-while
    #:gather-match #:ingest-edges
    #:grp #:distinct #:first< #:first> #:lsort
    #:walk-grph #:walk-edge-set
    #:dead-ends #:edge-set
    #:segment-isects #:multi-isects #:two-isects
    #:del-dead-ends #:del-dead-ends!
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
    #:3cut-all #:3cut-all!
    #:2cut-to-area #:2cut-to-area!
    #:triangulate-edge-set))

