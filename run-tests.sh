#!/bin/bash

set -e
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (ql:quickload :grph :verbose nil)
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
     --eval '(handler-case (asdf:test-system :grph)
                           (error (c) (print c) (sb-ext:quit :unix-status 3)))'

