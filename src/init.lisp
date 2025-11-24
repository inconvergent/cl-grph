(in-package :grph)

(defmacro init-config (dev-vals vals)
  (if (> (length (string-downcase (veq::vgetenv "DEV" ""))) 0)
    `(progn (defvar *dev* t #1="compile in dev mode.") (defvar *opt* ',dev-vals #2="optimization config.")
            (format t "~&---------!!!!! GRPH COMPILED IN DEVMODE !!!!!---------
--------- ~a~%" ',dev-vals))
    `(progn (defvar *dev* nil #1#) (defvar *opt* ',vals #2#))))
