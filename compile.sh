#!/bin/bash

set -e
touch ./grph.asd
time sbcl --quit \
           --eval '(load "grph.asd")'\
           --eval '(handler-case (time (ql:quickload :grph :verbose t))
                     (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.log 2>&1
