(in-package :grph)

; the serial versions are currently faster, except for filter (maybe)
; TODO: there are probably ways to parallelize some of the current serial reducers?

(defun p/qry-filter (a b fx)
  (declare (optimize speed (safety 0)) (list a) (ignore b) (function fx))
  (lparallel:premove-if-not fx a))

