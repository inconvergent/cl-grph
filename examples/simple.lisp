#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :grph)
(in-package :grph)

; the ! postfix indicates that the graph will be mutated, and that the symbol
; (g) will be updated to refer to the mutated version of the graph. but the
; operations are not destructive. that is, any previous reference to the graph
; will remain unchanged.

; the ! postfixed macros are convenient, but there are also corresponding
; functions add, del, prop which will return the mutated graph. that is

; (add! g 1 2) corresponds to doing
; (setf g (add g 1 2))

(defun get-sample-graph (&aux (g (grph)))

  ; add edges
  (add! g 0 1)
  (add! g 0 3)
  (add! g 2 3)
  (add! g 5 6)
  (add! g 9 0)
  (add! g 9 6)
  (add! g 9 7)

  ; add an edges with property
  (add! g 3 4 '(:a)) ; corresponds to '((:a t))
  (add! g 4 3 '((:a "43") (:xx 888)))
  (add! g 6 0 '((:a "60")))
  (add! g 8 9 '(:b))
  (add! g 7 8 '(:b :some-value))
  ; this wil be ignored since (7 8) is created above
  (add! g 7 8 '(:nothing-happens :hh)) ; returns nil
  g)


(defun main ()
  (let ((g (get-sample-graph)))
    (print (@edges g))
    (print (@verts g) )
    (print (@out g 3) )
    (print (@in g 3) )

    (print (@mem g 7 8))
    (print (@prop g (list 7 8)))

    (print (del! g 7 8))
    (print (@mem g 7 8))

    (print (del! g 2 3))
    (print (@in g 3))

    (print (@prop g '(4 3)))
    (print (@prop g '(7 8)))
    (print (@prop g '(4 3) :a))
    (print (@verts g))

    ; iteration macros
    (itr-edges (g e) (format t "~&edge: ~a: ~a~%" e (@prop g e)))
    (itr-verts (g v) (format t "~%~a, adjacent: " v)
      (itr-adj (g v i) (format t "~a " i)))))

(main)

