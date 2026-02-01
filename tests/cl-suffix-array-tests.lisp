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

(defun create-test-file (content filename)
  "Create a test file with the given content."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (write-string content out)))

(test basic-functionality
  (create-test-file "banana" "banana.txt")
  (let ((result (cl-suffix-array:build-suffix-array "banana.txt" "banana-out.txt")))
    ;; Check that the function returns the output file path
    (is (equal result "banana-out.txt") "Function returned correct output file path")
    (is (probe-file "banana-out.txt") "Output file exists")
    (is (plusp (file-length (open "banana-out.txt" :element-type 'character)))
	"Output file is not empty")))

(test file-size-handling
  (create-test-file "abc" "small-test.txt")
  (let ((result (cl-suffix-array:build-suffix-array "small-test.txt" "small-out.txt")))
    (is (equal result "small-out.txt")
	"Function returned correct output file path for small file")
    (is (probe-file "small-out.txt")
	"Small output file exists")))
