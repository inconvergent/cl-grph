#!/bin/bash

sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :grph )'\
     --eval '(handler-case (grph:ext-symbols? :grph :pretty)
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >DOCS.md.tmp 2>&1

cat << EOF > DOCS.md
# grph DOCUMENTATION

### Explanation

#### Context

#### Abbreviations

\`dsb\` is short for \`destructuring-bind\`.

\`mvb\` is short for \`multiple-value-bind\`.

\`mvc\` is short for \`multiple-value-call\`.


### Symbols

EOF

tail -n +8 DOCS.md.tmp >> DOCS.md
sed -i 's/[[:space:]]*$//g' DOCS.md
sed -i 's:/data/x/grph/::g' DOCS.md
rm  DOCS.md.tmp
