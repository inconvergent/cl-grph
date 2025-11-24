(in-package :grph)

(init-config (optimize safety (speed 1) debug (space 2))
             (optimize (safety 1) (speed 3) (debug 1) (space 2)))

(declaim (boolean *parallel*))

(defvar *parallel* nil)
(defparameter *aggregates* '(:cnt :grp)
  "valid aggregate clauses in qry.")
(defparameter *clauses* '(:and :not :or :or-join :not-join :q :% :f :fact :uniq)
  "valid query clauses.")
(defparameter *dir-modes* '(:-> :<- :<> :><)
  "valid edge direction modes.")
(defparameter *pos-modes* '(:abs :rel)
  "valid spatial modes.")

(map-docstring 'cancel "(cancel) can be used in some contexts (using, qry) to cancel
the transaction and discard all changes" :nodesc)
(map-docstring 'stop "(stop) can be used in some contexts (using, qry) to stop
the transaction, but keep the changes" :nodesc)

