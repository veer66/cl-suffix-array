# cl-suffix-array

A Common Lisp library for building and using suffix arrays.

## Installation

To load this system, make sure it is in a directory that ASDF knows about (e.g., `~/common-lisp/`) and then run:

```lisp
(asdf:load-system :cl-suffix-array)
```

## Usage

```lisp
(cl-suffix-array:build-suffix-array "banana")
;; => (5 3 1 0 4 2)
```

## License

This project is licensed under the MIT License.
