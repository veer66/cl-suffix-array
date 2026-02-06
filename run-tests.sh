#!/bin/bash

sbcl --disable-debugger \
     --eval "(require :asdf)" \
     --eval "(asdf:load-system :cl-suffix-array)" \
     --eval "(cl-suffix-array-test:run-tests)" \
     --quit
