(in-package :grph)

(defun p/some-subsets (a b)
  (declare (optimize speed (safety 0)) (list a b))
  (lparallel:psome (lambda (o)
                     (declare (list o))
                     (subsetp o a :test #'equal)) b))

(defun p/qry-not (orig not* &optional select)
  (declare (optimize speed (safety 0)) (list orig not*))
  (lparallel:premove-if (lambda (a)
                          (declare (list a))
                          (p/some-subsets a (if select (select-vars not* select) not*)))
                        orig))

(defun p/qry-and (aa bb)
  (declare (optimize speed (safety 0)) (list aa bb))
  (when (< (length aa) (length bb)) (rotatef aa bb))
  (dedupe-matches
    (lparallel:pmapcan
      (lambda (a)
        (loop for b in bb
              unless (conflict a b)
              collect (union a b :test #'equal)))
      aa)))

(defun p/qry-filter (a b fx)
  (declare (optimize speed (safety 0)) (list a) (ignore b) (function fx))
  (lparallel:premove-if-not fx a))

; TODO: parallel dedupe-matches?
; TODO: parallel select-vars?
; TODO: parallel qry-or?
