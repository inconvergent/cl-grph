#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

; load with parallel reducers:
(let ((*features* (cons :grph-parallel *features*)))
  (ql:quickload :grph))
(setf lparallel:*kernel* (lparallel:make-kernel 8))

; or grph load with serial query reducers
; (ql:quickload :grph)

(in-package :grph)

(defun main ()

  ; create a graph with a set of edges with properties
  (let ((g (ingest-facts (grph)
             '((0 :A 1) (0 :C 1) (1 :A 3) (1 :A 2) (1 :A 0) (1 :C 0)
               (2 :A 1) (3 :C 7) (3 :B 5) (3 :C 5) (3 :B 4) (3 :A 1)
               (4 :B 3) (4 :B 5) (4 :E 5) (5 :B 3) (5 :C 3) (5 :B 4)
               (5 :E 4) (7 :C 3) (9 :_ 7) (7 :_ 3)))))

    (itr-edges (g e) (format t "~&edge: ~a: ~s~%" e (@prop g e)))

    ; get every edge (?x ?y) with prop :a and :c
    (print (qry g :select (?x ?y)
                  :where (and (?x :a ?y) (?x :c ?y))))
    ; '((1 0) (0 1))


    ; get every edge (?x ?y) with prop :c, except incident edges of 1 with prop
    ; :a and incident edges of 3 with prop :a
    (print (qry g :select (?x ?y)
                  :where (and (?x :c ?y)
                              (not (or (?x :a 1) (?x :a 3))))))
    ; '((7 3) (5 3))

    ; get every vert ?r such that
    ;   ?r -:a-> ?a -:b-> 5;
    ;   ?r -:c-> 0; or
    ;   ?r -:e-> 5
    (print (qry g :select ?r
                  :where (or-join ?r (and (?r :a ?a) (?a :b 5))
                                     (?r :c 0)
                                     (?r :e 5))))
    ; '((4) (1))

    ; every edge (?x ?y) with prop :e or :a, when ?x index is less than ?y index
    (print (qry g :select (?x ?y)
                  :when (< ?x ?y)
                  :where (or (?x :e ?y) (?x :a ?y))))
    ; '((0 1) (1 2) (1 3) (4 5))

    ; get every vert ?y where
    ;   5 -:b-> ?y; or
    ;   5 -:a-> ?y
    (let ((?x 5))
      (print (qry g :in ?x ; bind ?x to the "outside" value: 5
                    :select ?y
                    :where (or (?x :b ?y)
                               (?x :a ?y)))))
    ; '((3) (4))

    ; collect (list ...) for every edge (?x ?y)
    ; with prop :a or :b, ignore edges with prop :c
    (mapc #'print (qry g :select (?x ?y)
                         :where (and (or (?x :a ?y) (?x :b ?y))
                                     (not (?x :c ?y)))
                         :collect (list (+ ?x ?y) 88 ?x ?y)))
    ; '((7 88 3 4) (7 88 4 3) (9 88 4 5) (9 88 5 4)
    ;   (4 88 3 1) (3 88 2 1) (4 88 1 3) (3 88 1 2))

    ; print every 3-path ?a--?b--?c.
    (qry g :select (?a ?b ?c)
           :where (and (?a _ ?b) (?b _ ?c))
           :when (/= ?a ?b ?c) ; ignore self-intersecting paths
           :then (print (list :3-path ?a ?b ?c))) ; returns nil
    ; (:3-PATH 9 7 3)
    ; (:3-PATH 7 3 5)
    ; ...
    ; (:3-PATH 0 1 3)
    ; (:3-PATH 0 1 2)
    ))

(main)

