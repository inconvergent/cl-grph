(in-package :grph)

(defmacro srt (a b) (declare (symbol a b)) `(if (< ,a ,b) (list a b) (list b a)))
(defmacro adj (g) (declare (symbol g)) `(grph-adj ,g))
(defmacro mid (g) (declare (symbol g)) `(grph-mid ,g))
(defmacro props (g) (declare (symbol g)) `(grph-props ,g))


(defmacro get-multi-rel (props k &key prop (default nil))
  (awg (pmap)
    `(let ((,pmap (@ ,props ,k)))
      ,(if prop `(and ,pmap (values (@ ,pmap ,prop)))
                `(or ,pmap ,default)))))

(defmacro set-multi-rel (props k p &optional (val nil val?)
                                   &aux (default (if val?  'nilmap 'nilset)))
  (awg (pk props*)
    `(let* ((,props* ,props)
            (,pk (@ ,props* ,k)))
      (declare (ignorable ,pk))
      (fset:with ,props* ,k ; props is a map
        ; pk is a map if val is provided, otherwise set
        (fset:with (or ,pk ,default) ,p
                   ,@(if val? `(,val)))))))

; TODO: optional p drops entire k
(defmacro del-multi-rel (props k &optional p)
  (awg (props* pk)
    `(let* ((,props* ,props)
            (,pk (@ ,props* ,k)))
       (declare (ignorable ,pk))
       ,(if p `(fset:with ,props* ,k
                 (when ,pk (fset:less ,pk ,p)))
              `(fset:less ,props* ,k)))))

; TODO: itr prop edges, itr prop verts?
; TODO: ignore half?
; TODO: optional arg for rev?
(defmacro itr-edges ((g a &optional b) &body body)
  (declare (symbol g a))
  "iterate all edges, as either a=(v1 v2) or a=v1, b=v2."
  (unless (or (not b) (symbolp b)) (error "bad arg to itr-edges"))
  (awg (a* b* eset has)
    `(do-map (,a* ,eset (adj ,g))
      (do-map (,b* ,has ,eset)
        (when ,has
           ,(if b `(let ((,a ,a*) (,b ,b*)) (declare (ignorable ,a ,b)) ,@body)
               `(let ((,a (list ,a* ,b*))) ,@body)))))))

(defmacro itr-adj ((g a b &optional (mode :out)) &body body)
  (declare (symbol g b))
  "iterate all outboud verts, b, of a."
  (awg (a* b* eset has)
    `(let* ((,a* ,a)
            (,eset (@ (adj ,g) ,a*)))
      (when ,eset (do-map (,b* ,has ,eset)
                    (,@(if (eq mode :out) `(when ,has) `(unless ,has))
                      (let ((,b ,b*)) ,@body)))))))

(defmacro itr-verts ((g a) &body body)
  (declare (symbol g a))
  "iterate all connected verts, as a."
  (awg (seen b)
    `(let ((,seen (make-hash-table :test #'eql)))
      (labels ((,seen (,b) (when (not (gethash ,b ,seen))
                             (setf (gethash ,b ,seen) t)
                             t)))
        (itr-edges (,g ,a ,b)
          (when (not (,seen ,a)) (let ((,a ,a)) (declare (ignorable ,a)) ,@body))
          (when (not (,seen ,b)) (let ((,a ,b)) (declare (ignorable ,a)) ,@body)))))))

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

