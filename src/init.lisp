(in-package :grph)

(defmacro init-config (dev-vals vals)
  (if (> (length (string-downcase (veq::vgetenv "DEV" ""))) 0)
    `(progn (defvar *dev* t) (defvar *opt* ',dev-vals)
            (format t "~&---------!!!!! GRPH COMPILED IN DEVMODE !!!!!---------
--------- ~a~%" ',dev-vals))
    `(progn (defvar *dev* nil) (defvar *opt* ',vals))))
