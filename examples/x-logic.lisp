#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :grph)
(let ((*features* (cons :veq-reader-macros *features*)))
          (ql:quickload :weird))

(in-package :grph)

(defun make-edge-set ()
  (let ((wer (weir:make))
        (res (list)))
    (loop repeat 10
          do (weir:2add-vert! wer (rnd:2in-square 1f0)))
    (weir:add-edge! wer 0 1) (weir:add-edge! wer 0 4)
    (weir:add-edge! wer 1 5) (weir:add-edge! wer 0 2)
    (weir:add-edge! wer 1 2) (weir:add-edge! wer 1 3)
    (weir:add-edge! wer 3 4) (weir:add-edge! wer 4 6)
    (weir:add-edge! wer 3 4) (weir:add-edge! wer 4 5)
    (weir:add-edge! wer 3 5) (weir:add-edge! wer 3 7)

    (weir:set-vert-prop wer 1 :name)
    (weir:set-vert-prop wer 0 :name)
    (weir:set-vert-prop wer 6 :name)
    (weir:set-vert-prop wer 3 :name)

    (weir:set-edge-prop wer '(0 1) :a)
    (weir:set-edge-prop wer '(1 2) :a)
    (weir:set-edge-prop wer '(1 3) :a)

    (weir:set-edge-prop wer '(3 4) :b)
    (weir:set-edge-prop wer '(3 5) :b)
    (weir:set-edge-prop wer '(4 5) :b)

    (weir:set-edge-prop wer '(3 5) :c)
    (weir:set-edge-prop wer '(3 7) :c)
    (weir:set-edge-prop wer '(0 1) :c)

    (weir:set-edge-prop wer '(4 5) :e)

    (weir:itr-edges (wer e :verts (a b))
      (loop for p in (mapcar #'car (weir:get-edge-props wer e ))
        do (push `(,a ,p ,b) res)
           (push `(,b ,p ,a) res)))
    (setf res (sort (copy-tree res) #'< :key #'car))))

(defun main ()
  (let ((facts (make-edge-set)))
    (print facts)

    ))

(main)
