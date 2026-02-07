#!/bin/bash
# run-tests.sh - Run tests for cl-suffix-array

set -e

cd "$(dirname "$0")"

# Run tests using sbcl
sbcl --load package.lisp --load cl-suffix-array.lisp --load tests/cl-suffix-array-tests.lisp --eval "(cl-suffix-array-tests:run-simple-tests)" --quit
