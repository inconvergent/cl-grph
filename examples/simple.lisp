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
  (print :create)
  (print ; print all output for the sake of demonstration
    (veq:lst
      (add! g 0 1) ; add edge '(0 1). returns '(0 1)
      (add! g 0 3)
      (add! g 2 3)
      (add! g 5 6)
      (add! g 9 0)
      (add! g 9 6)
      (add! g 9 7)
      ; add edges with property
      (add! g 3 4 :a) ; edge with property: (3 :a 4)
      (add! g 4 3 '(:a :xx)) ; edge with two properties
      (add! g 6 0 '(:a :b))
      (add! g 8 9 :b)
      (add! g 7 8 '(:b :some-value)) ; returns '(7 8)
      ; this wil be ignored since (7 8) is created above
      (add! g 7 8 '(:nothing-happens :hh)))) ; returns nil
  g)


(defun main ()
  (let* ((g (get-sample-graph)) (h g))
    ; (veq:vpr ...) prints all all values inside
    (veq:vpr g)
    (veq:vpr (@edges g)) ; list all edges
    (veq:vpr (@verts g)) ; list all verts
    (veq:vpr (@out g 3)) ; outbound verts of 3
    (veq:vpr (@in g 3)) ; inbounds verts of 3

    (veq:vpr (@mem g 7 8)) ; does edge (7 8) exist?
    (veq:vpr (@prop g '(7 8))) ; get props of edge (7 8)

    (veq:vpr (del! g 7 8)) ; delete edge (7 8)
    (veq:vpr g)
    (veq:vpr (@mem g 7 8))

    (veq:vpr (del! g 2 3))
    (veq:vpr (@in g 3)) ; inbound verts of 3

    (veq:vpr (@prop g '(4 3)))
    (veq:vpr (@prop g '(7 8)))
    (veq:vpr (@prop g '(4 3) :a)) ; t if (4 :a 3) exists
    (veq:vpr (@verts g))
    (print g)
    (print h) ; grph is immutable, so h still has the initial graph

    ; iteration macros
    (itr-edges (g e) (format t "~&edge: ~a: ~a~%" e (@prop g e)))
    (itr-verts (g v) (format t "~%~a, adjacent: " v)
      (itr-adj (g v i) (format t "~a " i)))))

(main)

