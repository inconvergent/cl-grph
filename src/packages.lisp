(defpackage #:grph
  (:use #:common-lisp)
  (:nicknames #:cl-grph)
  (:import-from #:veq #:dsb #:mvc #:mvb #:awg #:awf #:pn)
  (:import-from #:fset #:@ #:contains? #:do-map #:do-set #:empty-map
                #:empty-set #:less #:member?)
  (:export
    #:*dir-modes* #:*pos-modes* #:*clauses* *aggregates*
    #:to-vector #:ensure-list #:last* #:vector-last #:vector-first
    #:ext-symbols?
    #:@edges #:@enum #:@in #:@out #:@either #:@both #:num-either
    #:@verts #:@vcnt #:@vmax #:@prop #:@mem
    #:add #:del #:del-props
    #:add! #:add*! #:del! #:ldel! #:pdel!
    #:path! #:modify! #:split!
    #:memo #:relneigh
    #:itr-edges #:itr-adj #:itr-verts
    #:grph #:make #:prt
    #:compile-query #:match
    #:qry #:lqry #:rqry #:stop #:cancel #:using
    #:collect-while #:qry-collect-while
    #:connected-verts
    #:gather-match #:ingest-edges #:ingest-props-edges
    #:grp #:distinct #:first< #:first> #:lsort
    #:walk-grph #:walk-edge-set
    #:dead-ends #:edge-set #:props-edges
    #:segment-isects #:multi-isects #:two-isects
    #:del-dead-ends #:del-dead-ends!
    #:path->edge-set #:edge-set->path #:edge-set->ht #:ht->edge-set
    #:normalise-fold #:normalise-fold!
    #:lst->set #:lst->map #:set->lst #:map->lst
    #:set->lst-fx #:lst->set-fx))

(defpackage #:xgrph
  (:use #:common-lisp)
  (:import-from #:fset #:empty-seq #:seq)
  (:import-from #:veq #:dsb #:mvc #:mvb #:awg #:pn)
  (:import-from #:grph #:*dir-modes* #:*pos-modes* #:add! #:del!)
  (:export
    #:@vert #:2@vert #:3@vert #:@verts #:2@verts #:3@verts
    #:@ #:2@ #:3@ #:l@ #:2l@ #:3l@
    #:vert! #:2vert! #:3vert! #:verts! #:2verts! #:3verts!
    #:move! #:2move! #:3move!
    #:@num #:2@num #:3@num
    #:pos #:itr-vset!
    #:vset! #:2vset! #:3vset!
    #:path! #:2path! #:3path!
    #:split! #:2split! #:3split!
    #:append! #:2append! #:3append!
    #:2intersect-all #:2intersect-all!
    #:2cut #:2cut! #:3cut-all #:3cut-all!
    #:2mirror! #:2mirror
    #:2center-connected
    #:2cut-to-area #:2cut-to-area!
    #:triangulate-edge-set))

(defpackage #:grph/io
  (:use #:common-lisp)
  (:import-from #:fset #:do-map #:do-set #:empty-map #:empty-set)
  (:import-from #:veq #:dsb #:mvc #:mvb)
  (:import-from #:grph #:grph)
  (:export #:export-dat #:import-dat #:gexport #:gimport
           #:gwrite #:gread #:gwrite-script))

