#!/bin/bash

set -e
touch ./grph.asd
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (ql:quickload :grph :verbose nil)
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
     --eval '(handler-case (asdf:test-system :grph)
                           (error (c) (print c) (sb-ext:quit :unix-status 3)))'

touch ./grph.asd
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (let ((*features* `(:grph-parallel ,@*features*)))
                             (ql:quickload :grph :verbose nil))
                           (error (c) (print c) (sb-ext:quit :unix-status 4)))'\
     --eval '(setf lparallel:*kernel* (lparallel:make-kernel 6))'\
     --eval '(handler-case (asdf:test-system :grph)
                           (error (c) (print c) (sb-ext:quit :unix-status 5)))'

