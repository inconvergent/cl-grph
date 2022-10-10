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

(defvar *valid-clauses* '(:and :not :or :or-join :not-join :q :%))
(defvar *dir-mode* '(:-> :<- :<>))
(defvar *pos-mode* '(:rel :abs))

