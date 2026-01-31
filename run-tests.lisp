#!/usr/bin/env sbcl

;; Load the system
(load "package.lisp")
(load "cl-suffix-array.lisp")

;; Define test function
(defun run-simple-tests ()
  (format t "Running simple tests for cl-suffix-array...~%")
  
  ;; Test 1: Basic functionality
  (format t "Test 1: Basic functionality~%")
  (with-open-file (out "test-basic.txt" :direction :output :if-exists :supersede)
    (write-string "banana" out))
  
  (let ((result (cl-suffix-array:build-suffix-array "test-basic.txt" "output-basic.txt")))
    (if (and (equal result "output-basic.txt") (probe-file "output-basic.txt"))
        (format t "  PASS: Basic functionality~%")
        (format t "  FAIL: Basic functionality~%")))
  
  ;; Test 2: UTF-8 handling
  (format t "Test 2: UTF-8 handling~%")
  (with-open-file (out "test-utf8.txt" :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string "héllo 世界" out))
  
  (let ((result (cl-suffix-array:build-suffix-array "test-utf8.txt" "output-utf8.txt")))
    (if (and (equal result "output-utf8.txt") (probe-file "output-utf8.txt"))
        (format t "  PASS: UTF-8 handling~%")
        (format t "  FAIL: UTF-8 handling~%")))
  
  ;; Test 3: Larger file
  (format t "Test 3: Larger file handling~%")
  (with-open-file (out "test-large.txt" :direction :output :if-exists :supersede)
    (loop for i from 1 to 50 do (format out "line~a~%" i)))
  
  (let ((result (cl-suffix-array:build-suffix-array "test-large.txt" "output-large.txt" :chunk-size 256)))
    (if (and (equal result "output-large.txt") (probe-file "output-large.txt"))
        (format t "  PASS: Larger file handling~%")
        (format t "  FAIL: Larger file handling~%")))
  
  ;; Cleanup
  (format t "Cleaning up test files...~%")
  (dolist (file '("test-basic.txt" "output-basic.txt" 
                  "test-utf8.txt" "output-utf8.txt"
                  "test-large.txt" "output-large.txt"))
    (when (probe-file file)
      (delete-file file)))
  
  (format t "All tests completed.~%"))

;; Run the tests
(run-simple-tests)
(quit)