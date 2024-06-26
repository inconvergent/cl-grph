(defpackage #:grph
  (:use #:common-lisp)
  (:nicknames #:cl-grph)
  (:import-from #:veq #:dsb #:mvc #:mvb #:awg #:awf #:pn #:in #:group #:ungroup)
  (:import-from #:fset #:@ #:contains? #:do-map #:do-set #:empty-map
                #:empty-set #:less #:member?)
  (:export
    #:*parallel* #:*dir-modes* #:*pos-modes* #:*clauses* *aggregates* #:v? #:ext-symbols?
    #:to-vector #:ensure-list #:last* #:vector-last #:vector-first
    #:@edges #:@enum #:@pnum #:@in #:@out #:@either #:@both #:num-either
    #:@verts #:@vcnt #:@vmax #:@prop #:@mem #:@mid
    #:grph #:make #:prt
    #:add #:del #:del-props #:prop
    #:add! #:add*! #:ladd*! #:del! #:ldel! #:pdel! #:prop!
    #:path! #:modify! #:split!
    #:itr-edges #:itr-adj #:itr-verts
    #:compile-query #:match
    #:qry #:lqry #:rqry #:stop #:cancel #:using
    #:collect-while #:qry-collect-while
    #:connected-verts #:memo #:relneigh
    #:gather-match #:ingest-edges #:ingest-props-edges
    #:distinct #:first< #:first> #:lsort
    #:walk-grph #:walk-edge-set #:walk-grph-segments #:walk-edge-set-segments
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
  (:import-from #:veq #:dsb #:mvc #:mvb #:awg #:pn #:in)
  (:import-from #:grph #:*dir-modes* #:*pos-modes* #:add! #:del! #:grph)
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
    #:triangulate-edge-set
    #:2vfx
    #:es->adj-ht
    #:adj->left-most-vert
    #:adj-strip-filaments
    #:find-cycle-basis #:adj-find-cycle-basis
    #:find-outline #:adj-find-outline))

(defpackage #:grph/io
  (:use #:common-lisp)
  (:import-from #:fset #:do-map #:do-set #:empty-map #:empty-set)
  (:import-from #:veq #:dsb #:mvc #:mvb #:awg #:pn #:in)
  (:import-from #:grph #:grph)
  (:export #:export-dat #:import-dat #:gexport #:gimport
           #:gwrite #:gread #:gwrite-script))

