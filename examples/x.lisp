#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

; (load "logic-utils")
(let ((*features* (cons :veq-reader-macros *features*)))
          (ql:quickload :weird))
(ql:quickload :grph)
(rnd:set-rnd-state 1)


; ; (let* ((g (print (grph)))
; ;        (g2 g))
; ;   (setf g2 (add g2 7 2))
; ;   (setf g2 (add g2 8 3))
; ;   (setf g2 (add g2 6 7))
; ;   (print (setf g2 (add g2 8 3)))
; ;   (itr-edges (g2 a b) (print (list a b)))
; ;   (print (setf g2 (del g2 8 3)))
; ;   )

; ; (define-setf-expander xset (s &environment env)
; ;    "Set the last element in a list to the given value."
; ;    (mvb (dummies vals newval setter getter) (get-setf-expansion s env)
; ;      (let ((store (gensym)))
; ;        (values dummies vals
; ;                `(,store)
; ;                `(progn (setf ,getter (s ,store)) ,store)
; ;                `(xset ,getter)))))

(defun main ()
  (print :new)

  (let* ((n 1000000)
        (n2 n)
        (k (/ n 2))
        (grph (grph:grph))
        (graph (graph:make)))
    (print (grph::props grph))
    (time
      (loop repeat n
            for i = (rnd:rndi k)
            for j = (rnd:rndi k)
            if (/= i j)
            do (setf grph (grph:add grph i j (intern (grph::mkstr (rnd:rndi 1000)))))
               (grph:del! grph i (rnd:rndi k))
            finally (print grph)))
    (time
      (print (loop repeat n
            if (grph:@mem grph (rnd:rndi k) (rnd:rndi k))
            summing 1)))


  ; (print :old)
  ; (time
  ;   (loop repeat n
  ;         do (graph:add graph (rnd:rndi k) (rnd:rndi k))
  ;            (graph:del graph (rnd:rndi k) (rnd:rndi k))
  ;         finally (print graph)))
  ; (time
  ;   (loop repeat n2
  ;         do (graph:mem graph (rnd:rndi k) (rnd:rndi k))))

   ; (grph:prop! grph '(1 2) :name)
   ; (grph:prop! grph '(1 2) :name :fuck)
   ; (grph:prop! grph '(1 2) :thing :aa)
   ; (grph:prop! grph '(1 2) :other :sss)
   ; (grph:prop! grph '(8 2) :other :sss)
   ; (print grph)
   ; (print (grph::grph-props grph))
   ; (veq:vpr (grph:get-prop grph '(1 2) :name))
   ; (grph:prop! grph '(1 2) :name)
   ; (grph:prop! grph '(1 2) :name nil)
   ; (print grph)
   ; (veq:vpr (grph:get-prop grph '(1 2) :name))
   ; (veq:vpr (grph:get-prop grph '(1 7) :name))
  )

  )

(main)

