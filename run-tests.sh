#!/bin/bash
# run-tests.sh - Run tests for cl-suffix-array

set -e

cd "$(dirname "$0")"

# Run tests using sbcl
sbcl --load package.lisp --load cl-suffix-array.lisp --load cl-suffix-array-test.lisp --eval "(cl-suffix-array-test:run-tests)" --quit
