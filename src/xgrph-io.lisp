(in-package :grph/io)

(defvar *pfx* ".grph")

(defun export-dat (fn o &optional (pfx ".dat"))
  (declare (string fn pfx)) "write o to fn."
  (with-open-file (f (grph::mkstr fn pfx) :direction :output :if-exists :supersede)
    (format f "~s~%" o) nil))
(defun import-dat (fn &optional (pfx ".dat"))
  (declare (string fn pfx)) "read data from fn."
  (with-open-file (f (grph::mkstr fn pfx) :direction :input) (read f)))

(defun gexport (g &key (pos (xgrph:pos 0f0)) (dim 2) meta)
  (declare (grph:grph g) (xgrph:pos pos) (veq:pn dim) (list meta)) "serialize g."
  `((:file . ((:ver . :v0) (:pkg . :grph/io) (:type . :single)))
    (:dim . ,dim) (:meta . (,@meta))
    (:edges . ,(grph:props-edges g)) (:pos . ,(fset:convert 'list pos))))
(defun gimport (o)
  (declare (list o)) "deserialize g."
  (labels ((gk (k) (cdr (assoc k o :test #'eq))))
    (values (grph:ingest-props-edges (gk :edges))
            (fset:with-default (fset:convert 'fset:seq (gk :pos)) 0f0)
            (gk :meta))))

(defun gwrite (fn g &key (pos (xgrph:pos 0f0)) (dim 2) meta)
  (declare (string fn) (grph:grph g) (xgrph:pos pos) (veq:pn dim) (list meta))
  "write grph to fn."
  (export-dat fn (gexport g :pos pos :dim dim :meta meta) *pfx*))
(defun gread (fn)
  (declare (string fn)) "read grph from fn."
  (gimport (import-dat fn *pfx*)))

(defmacro gwrite-script ((fn g &key (pos (xgrph:pos 0f0)) (dim 2) meta) &body body)
  (declare (symbol g pos)) "write grph and body (:script) to fn."
  (veq::awg (fn*)
     `(let ((,fn* ,fn)) ,@(veq::tree-replace body :fn fn*)
                        (gwrite ,fn* ,g :pos ,pos :dim ,dim
                                :meta '((:script . ,body) ,@meta)))))

