#!/bin/bash

set -e
touch ./grph.asd
sbcl --quit \
     --eval '(ql:quickload :prove)'\
     --eval '(ql:quickload :lparallel)'\
     --eval '(setf lparallel:*kernel* (lparallel:make-kernel 6))'\
     --eval '(handler-case (ql:quickload :grph :verbose nil)
                           (error (c) (format t "STAGE1FAIL: ~a" c)
                                      (uiop:quit 2)))'\
     --eval '(handler-case (asdf:test-system :grph)
                           (error (c) (format t "STAGE2FAIL: ~a" c)
                                      (uiop:quit 3)))'

