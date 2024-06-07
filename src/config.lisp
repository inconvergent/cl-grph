(in-package :grph)

(init-config (optimize safety (speed 1) debug (space 2))
             (optimize (safety 1) (speed 3) (debug 1) (space 2)))

(declaim (boolean *parallel*))

(defvar *parallel* nil)
(defparameter *aggregates* '(:cnt :grp))
(defparameter *clauses* '(:and :not :or :or-join :not-join :q :% :f :fact :uniq))
(defparameter *dir-modes* '(:-> :<- :<> :><))
(defparameter *pos-modes* '(:abs :rel))

(map-docstring '*aggregates* (format nil "valid aggregate clauses in qry: ~a" *aggregates*) :nodesc)
(map-docstring '*clauses* (format nil "valid query clauses: ~a" *clauses*) :nodesc)
(map-docstring '*dir-modes* (format nil"valid edge direction modes: ~a" *dir-modes*) :nodesc)
(map-docstring '*pos-modes* (format nil "valid spatial modes: ~a" *pos-modes*) :nodesc)
(map-docstring 'cancel "(cancel) can be used in some contexts (using, qry) to cancel
the transaction and discard all changes" :nodesc)
(map-docstring 'stop "(stop) can be used in some contexts (using, qry) to stop
the transaction, but keep the changes" :nodesc)

