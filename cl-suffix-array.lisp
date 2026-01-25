(in-package #:cl-suffix-array)

;;;; cl-suffix-array.lisp

(defun build-suffix-array (text)
  "Builds a suffix array for the given text.
This is a placeholder implementation."
  (declare (type string text))
  ;; Placeholder: returns a sorted list of suffixes' starting indices.
  (let ((len (length text)))
    (sort (loop for i from 0 below len collect i)
          #'(lambda (i j)
              (string< (subseq text i) (subseq text j))))))
