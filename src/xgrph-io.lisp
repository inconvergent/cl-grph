(in-package :grph/io)

(defvar *pfx* ".grph")

(defun serialize-vert-props (g &aux (res (list)))
  (declare (grph:grph g))
  (fset:do-map (p o (grph::mid g))
    (let ((lst (remove-if-not (lambda (v) (typecase v (veq:in v)))
                              (grph:set->lst o))))
      (when lst (push `(,p . ,lst) res))))
  res)

(defun export-dat (fn o &optional (pfx ".dat"))
  (declare (string fn pfx)) "write o to fn. see import-dat."
  (with-open-file (f (grph::mkstr fn pfx) :direction :output :if-exists :supersede)
    (format f "~s~%" o) nil))
(defun import-dat (fn &optional (pfx ".dat"))
  (declare (string fn pfx)) "read data from fn. see export-dat."
  (with-open-file (f (grph::mkstr fn pfx) :direction :input) (read f)))

(defun gexport (g &key (pos (xgrph:pos 0f0)) (dim 2) meta)
  (declare (grph:grph g) (xgrph:pos pos) (pn dim) (list meta)) "serialize g. see gimport."
  `((:file . ((:ver . :v1) (:pkg . :grph/io) (:type . :single))) (:dim . ,dim)
    (:edges . ,(grph:props-edges g)) (:verts . ,(serialize-vert-props g))
    (:pos . ,(fset:convert 'list pos)) (:meta . (,@meta))))
(defun gimport (o)
  (declare (list o)) "deserialize g. see gexport."
  (labels ((gk (k) (cdr (assoc k o :test #'eq))))
    (let ((g (grph:ingest-props-edges (gk :edges))))
      (loop for (p . vv) in (gk :verts)
            do (loop for v in vv do (grph:prop! g v (list p))))
      (values g (fset:with-default (fset:convert 'fset:seq (gk :pos)) 0f0)
                (gk :meta)))))

(defun gwrite (fn g &key (pos (xgrph:pos 0f0)) (dim 2) meta)
  (declare (string fn) (grph:grph g) (xgrph:pos pos) (pn dim) (list meta))
  "write grph to fn. see gread."
  (export-dat fn (gexport g :pos pos :dim dim :meta meta) *pfx*))
(defun gread (fn)
  (declare (string fn)) "read grph from fn. see gwrite."
  (gimport (import-dat fn *pfx*)))

(defmacro gwrite-script ((fn g &key (pos (xgrph:pos 0f0)) (dim 2) meta (out t))
                               &body body)
  (declare (symbol g pos)) "write grph and body (:script) to fn."
  (awg (fn*) `(let ((,fn* ,fn)) ,@(veq::tree-replace body :fn fn*)
                ,(if out `(gwrite ,fn* ,g :pos ,pos :dim ,dim
                                  :meta '((:script . ,body) ,@meta))))))

