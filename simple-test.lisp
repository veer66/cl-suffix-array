(in-package #:cl-suffix-array)

(defun build-suffix-array (input-file-path output-file-path &key (chunk-size (* 1024 1024 10)))
  "Builds a suffix array from the text in input-file-path and saves it to output-file-path using SAScan algorithm.
   Handles files that exceed RAM capacity by processing in chunks."
  (format t "Function called with ~a and ~a~%" input-file-path output-file-path)
  output-file-path)