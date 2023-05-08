#!/bin/bash

set -e
touch ./grph.asd
sbcl --quit \
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (ql:quickload :grph :verbose nil)
                           (error (c) (format t "STAGE1FAIL: ~a" c)
                                      (sb-ext:quit :unix-status 2)))'\
     --eval '(handler-case (asdf:test-system :grph)
                           (error (c) (format t "STAGE2FAIL: ~a" c)
                                      (sb-ext:quit :unix-status 3)))'

touch ./grph.asd
sbcl --quit \
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (let ((*features* `(:grph-parallel ,@*features*)))
                             (ql:quickload :grph :verbose nil))
                           (error (c) (format t "STAGE3FAIL: ~a" c)
                                      (sb-ext:quit :unix-status 4)))'\
     --eval '(setf lparallel:*kernel* (lparallel:make-kernel 6))'\
     --eval '(handler-case (asdf:test-system :grph)
                           (error (c) (format t "STAGE4FAIL ~a" c)
                                      (sb-ext:quit :unix-status 5)))'

touch ./grph.asd

