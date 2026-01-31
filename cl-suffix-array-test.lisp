(defpackage #:cl-suffix-array-test
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package #:cl-suffix-array-test)

(def-suite :cl-suffix-array
  :description "Test suite for cl-suffix-array")
(in-suite :cl-suffix-array)

;; Test data
(defparameter *test-text* "banana")
(defparameter *test-file* "test-input-full.txt")
(defparameter *output-file* "output-sa-test.txt")

;; Helper function to create test file
(defun create-test-file (content filename)
  "Create a test file with the given content."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (write-string content out)))

(test basic-functionality
  "Test basic functionality of build-suffix-array"
  (create-test-file *test-text* *test-file*)

  ;; Call the function
  (let ((result (cl-suffix-array:build-suffix-array *test-file* *output-file*)))
    ;; Check that the function returns the output file path
    (is (equal result *output-file*))

    ;; Check that the output file exists
    (is (probe-file *output-file*))

    ;; For a simple test, we can check that the file is not empty
    (is (plusp (file-length (open *output-file* :element-type 'character))))))

(test file-size-handling
  "Test that the function handles file size correctly"
  (create-test-file "abc" "small-test.txt")

  (let ((result (cl-suffix-array:build-suffix-array "small-test.txt" "small-out.txt")))
    (is (equal result "small-out.txt"))
    (is (probe-file "small-out.txt"))))

(test utf8-handling
  "Test that the function handles UTF-8 text"
  (create-test-file "héllo 世界" "utf8-test.txt")

  (let ((result (cl-suffix-array:build-suffix-array "utf8-test.txt" "utf8-out.txt")))
    (is (equal result "utf8-out.txt"))
    (is (probe-file "utf8-out.txt"))))

(test large-file-simulation
  "Test with a larger simulated file"
  ;; Create a larger test file
  (let ((large-content (make-string 100 :initial-element #\a)))
    (setf (char large-content 50) #\b)  ; Add some variation
    (create-test-file large-content "large-test.txt"))

  (let ((result (cl-suffix-array:build-suffix-array "large-test.txt" "large-out.txt" :chunk-size 1024)))
    (is (equal result "large-out.txt"))
    (is (probe-file "large-out.txt"))))

;; Cleanup after tests
(defmacro with-test-files ((&rest files) &body body)
  "Execute body and cleanup specified test files afterwards."
  `(unwind-protect
        (progn ,@body)
     (dolist (file ',files)
       (when (probe-file file)
         (delete-file file)))))

;; Integration test
(test integration-test
  "Integration test with cleanup"
  (with-test-files (test-file output-file "small-test.txt" "small-out.txt"
                            "utf8-test.txt" "utf8-out.txt"
                            "large-test.txt" "large-out.txt")
    (create-test-file *test-text* *test-file*)

    (let ((result (cl-suffix-array:build-suffix-array *test-file* *output-file*)))
      (is (equal result *output-file*))
      (is (probe-file *output-file*)))))

(defun run-tests ()
  "Run all tests in the cl-suffix-array suite."
  (run! :cl-suffix-array))
