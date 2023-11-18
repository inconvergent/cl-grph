(in-package :grph)

(init-config (optimize safety (speed 1) debug (space 2))
             (optimize (safety 1) (speed 3) (debug 1) (space 2)))

(defparameter *clauses* '(:and :not :or :or-join :not-join :q :% :f :fact :uniq))
(map-docstring '*clauses* (format nil "valid query clauses: ~a" *clauses*) :nodesc)
(defparameter *aggregates* '(:cnt :grp))
(map-docstring '*aggregates* (format nil "valid aggregate clauses in qry: ~a" *aggregates*) :nodesc)
(defparameter *dir-modes* '(:-> :<- :<> :><))
(map-docstring '*dir-modes* (format nil"valid edge direction modes: ~a" *dir-modes*) :nodesc)
(defparameter *pos-modes* '(:abs :rel))
(map-docstring '*pos-modes* (format nil "valid spatial modes: ~a" *pos-modes*) :nodesc)

(map-docstring 'cancel "(cancel) can be used in some contexts (using, qry) to cancel
the transaction and discard all changes" :nodesc)
(map-docstring 'stop "(stop) can be used in some contexts (using, qry) to stop
the transaction, but keep the changes" :nodesc)

(defun psel (k)
  #+:grph-parallel (ecase k ((:and :fact :f :q) 'qry-and) (:or 'qry-or)
                            (:not 'qry-not) (:% 'p/qry-filter) (:let 'lparallel:plet))
  #-:grph-parallel (ecase k ((:and :fact :f :q) 'qry-and) (:or 'qry-or)
                            (:not 'qry-not) (:% 'qry-filter)  (:let 'let)))

