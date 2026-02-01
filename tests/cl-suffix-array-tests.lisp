(defpackage #:cl-suffix-array-tests
  (:use #:cl)
  (:import-from #:fiveam
		#:def-suite
		#:finishes
		#:in-suite
		#:is
		#:is-every
		#:signals
		#:test))

(in-package #:cl-suffix-array-tests)

(defparameter *test-text* "banana")
(defparameter *test-file* "test-input-full.txt")
(defparameter *output-file* "output-sa-test.txt")

(defun create-test-file (content filename)
  "Create a test file with the given content."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (write-string content out)))

(test basic-functionality
  (create-test-file *test-text* *test-file*)
  (let ((result (cl-suffix-array:build-suffix-array *test-file* *output-file*)))
    ;; Check that the function returns the output file path
    (is (equal result *output-file*) "Function returned correct output file path")
    (is (probe-file *output-file*) "Output file exists")
    (is (plusp (file-length (open *output-file* :element-type 'character)))
	"Output file is not empty")))
