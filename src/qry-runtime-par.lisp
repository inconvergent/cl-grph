(in-package :grph)

; pretty silly parallel versions of some reducers etc. still a lot faster than
; the serial code in many cases

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

(defun p/qry-and (aa bb &rest rest)
  (declare (optimize speed (safety 0)) (list aa bb) (ignore rest))
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


(defun p/union (aa bb)
  (declare (optimize speed (safety 0)) (list aa bb))
  (when (< (length aa) (length bb)) (rotatef aa bb))
  (concatenate 'list aa
     (lparallel:premove-if
       (lambda (b) (member b aa :test #'equal))
       bb)))

(defun p/qry-or (aa bb &optional select)
  (declare (optimize speed (safety 0)) (list aa bb select))
  ; this creates duplicates in (select x), but we need dedupe either way
  (dedupe-matches
    (if select
        (p/union (select-vars aa select) (select-vars bb select) :test #'equal)
        (p/union aa bb :test #'equal))))

; TODO: parallel dedupe-matches?
; TODO: parallel select-vars?

