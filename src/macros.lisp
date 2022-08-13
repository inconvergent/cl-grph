(in-package :grph)

(defmacro -srt (a b) (declare (symbol a b)) `(if (< ,a ,b) (list a b) (list b a)))
(defmacro adj (g) (declare (symbol g)) `(grph-adj ,g))
(defmacro props (g) (declare (symbol g)) `(grph-props ,g))

; TODO: itr prop edges, itr prop verts?
; TODO: ignore half?
(defmacro itr-edges ((g a &optional b) &body body)
  (declare (symbol g a b))
  "iterate all edges, as either a or (a b)."
  (awg (a* b* eset)
    `(do-map (,a* ,eset (adj ,g))
      (do-set (,b* ,eset)
        ,(if b `(let ((,a ,a*) (,b ,b*)) ,@body)
               `(let ((,a (list ,a* ,b*))) ,@body))))))

(defmacro itr-incident ((g a b) &body body)
  (declare (symbol g b))
  "iterate all incident verts, b, of a."
  (awg (a* b* eset)
    `(let* ((,a* ,a)
            (,eset (@ (adj ,g) ,a*)))
      (when ,eset (do-set (,b* ,eset)
                    (let ((,b ,b*)) ,@body))))))

(defmacro itr-verts ((g a) &body body)
  (declare (symbol g a))
  "iterate all connected verts, as a."
  (awg (visited b)
    `(let ((,visited (make-hash-table :test #'eql)))
      (labels ((,visited (,b) (when (not (gethash ,b ,visited))
                                    (setf (gethash ,b ,visited) t)
                                    t)))
        (itr-edges (,g ,a ,b)
          (when (not (,visited ,a)) (let ((,a ,a)) ,@body))
          (when (not (,visited ,b)) (let ((,a ,b)) ,@body)))))))

(defmacro add! (g a b &optional prop (val t))
  (declare (symbol g))
  "add edge edge and re-bind."
  `(setf ,g (add ,g ,a ,b ,@(when prop `(,prop ,val)))))

(defmacro del! (g a b)
  (declare (symbol g))
  "del edge and re-bind."
  `(setf ,g (del ,g ,a ,b)))

(defmacro prop! (g k prop &optional (val t val?))
  (declare (symbol g))
  "set prop and re-bind."
  `(setf ,g (prop ,g ,k ,prop ,@(when val? `(,val)))))

