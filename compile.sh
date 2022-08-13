#!/bin/bash

set -e
touch ./grph.asd
time sbcl --quit \
           --eval '(load "~/quicklisp/setup.lisp")'\
           --eval '(load "grph.asd")'\
           --eval '(handler-case (time (ql:quickload :grph :verbose t))
                     (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.tmp 2>&1
