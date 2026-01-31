(defpackage #:cl-suffix-array-test
  (:use #:cl)
  (:export #:run-tests #:run-simple-tests))

(in-package #:cl-suffix-array-test)

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

;; Simple test functions (always available)
(defun test-basic-functionality ()
  "Test basic functionality of build-suffix-array"
  (create-test-file *test-text* *test-file*)

  ;; Call the function
  (let ((result (cl-suffix-array:build-suffix-array *test-file* *output-file*)))
    ;; Check that the function returns the output file path
    (when (equal result *output-file*)
      (format t "  PASS: Function returned correct output file path~%"))
    
    ;; Check that the output file exists
    (when (probe-file *output-file*)
      (format t "  PASS: Output file exists~%"))
    
    ;; For a simple test, we can check that the file is not empty
    (when (plusp (file-length (open *output-file* :element-type 'character)))
      (format t "  PASS: Output file is not empty~%"))))

(defun test-file-size-handling ()
  "Test that the function handles file size correctly"
  (create-test-file "abc" "small-test.txt")

  (let ((result (cl-suffix-array:build-suffix-array "small-test.txt" "small-out.txt")))
    (if (and (equal result "small-out.txt") (probe-file "small-out.txt"))
        (format t "  PASS: Small file handling~%")
        (format t "  FAIL: Small file handling~%"))))

(defun test-utf8-handling ()
  "Test that the function handles UTF-8 text"
  (create-test-file "héllo 世界" "utf8-test.txt")

  (let ((result (cl-suffix-array:build-suffix-array "utf8-test.txt" "utf8-out.txt")))
    (if (and (equal result "utf8-out.txt") (probe-file "utf8-out.txt"))
        (format t "  PASS: UTF-8 handling~%")
        (format t "  FAIL: UTF-8 handling~%"))))

(defun test-large-file-simulation ()
  "Test with a larger simulated file"
  ;; Create a larger test file
  (let ((large-content (make-string 100 :initial-element #\a)))
    (setf (char large-content 50) #\b)  ; Add some variation
    (create-test-file large-content "large-test.txt"))

  (let ((result (cl-suffix-array:build-suffix-array "large-test.txt" "large-out.txt" :chunk-size 1024)))
    (if (and (equal result "large-out.txt") (probe-file "large-out.txt"))
        (format t "  PASS: Large file simulation~%")
        (format t "  FAIL: Large file simulation~%"))))

;; Cleanup after tests
(defmacro with-test-files ((&rest files) &body body)
  "Execute body and cleanup specified test files afterwards."
  (let ((file-var (gensym "FILE")))
    `(unwind-protect
          (progn ,@body)
       (dolist (,file-var ',files)
         (when (probe-file ,file-var)
           (delete-file ,file-var))))))

;; Integration test
(defun test-integration ()
  "Integration test with cleanup"
  (let ((test-file "integration-test-input.txt")
        (output-file "integration-test-output.txt"))
    (create-test-file *test-text* test-file)

    (let ((result (cl-suffix-array:build-suffix-array test-file output-file)))
      (if (and (equal result output-file) (probe-file output-file))
          (format t "  PASS: Integration test~%")
          (format t "  FAIL: Integration test~%")))

    ;; Cleanup specific files for this test
    (dolist (file (list test-file output-file "small-test.txt" "small-out.txt"
                        "utf8-test.txt" "utf8-out.txt"
                        "large-test.txt" "large-out.txt"))
      (when (probe-file file)
        (delete-file file)))))

;; FiveAM tests (only if FiveAM is available)
(eval-when (:load-toplevel :execute)
  (when (find-package :fiveam)
    (pushnew :fiveam *features*)
    (use-package :fiveam)))

#+fiveam
(progn
  (def-suite :cl-suffix-array
    :description "Test suite for cl-suffix-array")
  (in-suite :cl-suffix-array)

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

  ;; Integration test
  (test integration-test
    "Integration test with cleanup"
    (with-test-files (test-file output-file "small-test.txt" "small-out.txt"
                              "utf8-test.txt" "utf8-out.txt"
                              "large-test.txt" "large-out.txt")
      (create-test-file *test-text* *test-file*)

      (let ((result (cl-suffix-array:build-suffix-array *test-file* *output-file*)))
        (is (equal result *output-file*))
        (is (probe-file *output-file*))))))

(defun run-tests ()
  "Run all tests in the cl-suffix-array suite (FiveAM if available, otherwise simple tests)."
  #+fiveam
  (run! :cl-suffix-array)
  #-fiveam
  (run-simple-tests))

;; Simple test runner for environments without FiveAM
(defun run-simple-tests ()
  "Run simple tests without FiveAM framework."
  (format t "Running simple tests for cl-suffix-array...~%")

  ;; Test 1: Basic functionality
  (format t "Test 1: Basic functionality~%")
  (test-basic-functionality)

  ;; Test 2: File size handling
  (format t "Test 2: File size handling~%")
  (test-file-size-handling)

  ;; Test 3: UTF-8 handling
  (format t "Test 3: UTF-8 handling~%")
  (test-utf8-handling)

  ;; Test 4: Large file simulation
  (format t "Test 4: Large file simulation~%")
  (test-large-file-simulation)

  ;; Test 5: Integration test
  (format t "Test 5: Integration test~%")
  (test-integration)

  ;; Test 6: Suffix array
  (format t "Test 6: Suffix array~%")
  (with-open-file (out "test-object.txt" :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string "the quick brown fox jumps over the lazy dog" out))

  (cl-suffix-array:build-suffix-array "test-object.txt" "output-object.txt")

  (let* ((obj (cl-suffix-array:open-suffix-array "test-object.txt" "output-object.txt"))
         (found1 (cl-suffix-array:contains obj "fox"))
         (found2 (cl-suffix-array:contains obj "jumps"))
         (found3 (cl-suffix-array:contains obj "xyzzy")))
    (if (and found1 found2 (not found3))
        (format t "  PASS: Suffix array~%")
        (format t "  FAIL: Suffix array (found1=~a, found2=~a, found3=~a)~%" found1 found2 found3)))

  ;; Test 7: Suffix array with Unicode
  (format t "Test 7: Suffix array with Unicode~%")
  (with-open-file (out "test-unicode.txt" :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string "Hello 世界, this is a test with 中文 characters" out))

  (cl-suffix-array:build-suffix-array "test-unicode.txt" "output-unicode.txt")

  (let* ((obj (cl-suffix-array:open-suffix-array "test-unicode.txt" "output-unicode.txt"))
         (found1 (cl-suffix-array:contains obj "世界"))
         (found2 (cl-suffix-array:contains obj "中文"))
         (found3 (cl-suffix-array:contains obj "xyz")))
    (if (and found1 found2 (not found3))
        (format t "  PASS: Suffix array with Unicode~%")
        (format t "  FAIL: Suffix array with Unicode (found1=~a, found2=~a, found3=~a)~%" found1 found2 found3)))

  ;; Cleanup
  (format t "Cleaning up test files...~%")
  (dolist (file '("test-object.txt" "output-object.txt"
                  "test-unicode.txt" "output-unicode.txt"))
    (when (probe-file file)
      (delete-file file)))

  (format t "All tests completed.~%"))