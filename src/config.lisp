(in-package :grph)

; ; from: http://cl-cookbook.sourceforge.net/os.html
; (defun vgetenv (name &optional default)
;   #+CMU (let ((x (assoc name ext:*environment-list* :test #'string=)))
;           (if x (cdr x) default))
;   #-CMU (or #+Allegro (sys:getenv name)
;             #+CLISP (ext:getenv name)
;             #+ECL (si:getenv name)
;             #+SBCL (sb-unix::posix-getenv name)
;             #+LISPWORKS (lispworks:environment-variable name)
;             default))

; TODO: configure *opt*
; (defmacro init-config (dev-vals vals)
;   (if (> (length (string-downcase (vgetenv "DEV" ""))) 0)
;     `(progn (defvar *dev* t)
;             (defvar *opt* ',dev-vals)
;             (format t "~&---------!!!!! GRPH COMPILED IN DEVMODE !!!!!---------
; --------- ~a~%" ',dev-vals))
;     `(progn (defvar *dev* nil)
;             (defvar *opt* ',vals))))

; (declaim (single-float *eps*) (boolean *dev*) (cons *opt*))

; (defparameter *eps* #.(* 1f0 single-float-epsilon))

; (init-config (optimize safety (speed 1) debug (space 2))
;              (optimize (safety 1) (speed 3) debug space))

(defparameter *clauses* '(:and :not :or :or-join :not-join
                                :q :% :f :fact :uniq))
(map-docstring '*clauses*
  (format nil "valid query clauses: ~a" *clauses*) :nodesc)

(defparameter *aggregates* '(:cnt :grp))
(map-docstring '*aggregates*
  (format nil "valid aggregate clauses in qry: ~a" *aggregates*) :nodesc)

(defparameter *dir-modes* '(:-> :<- :<> :><))
(map-docstring '*dir-modes*
  (format nil"valid edge direction modes: ~a" *dir-modes*) :nodesc)

(defparameter *pos-modes* '(:abs :rel))
(map-docstring '*pos-modes*
  (format nil "valid spatial modes: ~a" *pos-modes*) :nodesc)

; (mapcar (lambda (s) (map-docstring s "edge direction mode" :nodesc)) '(-> <- <> ><))

(map-docstring 'cancel "(cancel) can be used in some contexts (using, qry) to cancel
the transaction and discard all changes" :nodesc)
(map-docstring 'stop "(stop) can be used in some contexts (using, qry) to stop
the transaction, but keep the changes" :nodesc)

(defun psel (k)
  #+:grph-parallel
  (ecase k ((:and :fact :f :q) 'qry-and) (:or 'qry-or)
           (:not 'qry-not) (:% 'p/qry-filter) (:let 'lparallel:plet))
  #-:grph-parallel
  (ecase k ((:and :fact :f :q) 'qry-and) (:or 'qry-or)
           (:not 'qry-not) (:% 'qry-filter)  (:let 'let)))

