(defpackage #:cl-suffix-array-tests
  (:use #:cl)
  (:export #:run-simple-tests))

(in-package #:cl-suffix-array-tests)

(defun create-test-file (content filename)
  "Create a test file with the given content."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (write-string content out)))

(defmacro with-test-files ((&rest files) &body body)
  "Execute body and cleanup specified test files afterwards."
  (let ((file-var (gensym "FILE")))
    `(unwind-protect
          (progn ,@body)
       (dolist (,file-var ',files)
         (when (probe-file ,file-var)
           (delete-file ,file-var))))))

(defun test-basic-functionality ()
  "Test basic functionality of build-suffix-array."
  (create-test-file "banana" "banana.txt")
  (let ((result (cl-suffix-array:build-suffix-array "banana.txt" "banana-out.txt")))
    (assert (equal result "banana-out.txt") nil "Function returned correct output file path")
    (assert (probe-file "banana-out.txt") nil "Output file exists")
    (assert (plusp (file-length (open "banana-out.txt" :element-type 'character))) nil "Output file is not empty")))

(defun test-file-size-handling ()
  "Test handling of small files."
  (create-test-file "abc" "small-test.txt")
  (let ((result (cl-suffix-array:build-suffix-array "small-test.txt" "small-out.txt")))
    (assert (equal result "small-out.txt") nil "Function returned correct output file path for small file")
    (assert (probe-file "small-out.txt") nil "Small output file exists")))

(defun test-utf8-handling ()
  "Test UTF-8 handling."
  (create-test-file "héllo 世界" "utf8-test.txt")
  (let ((result (cl-suffix-array:build-suffix-array "utf8-test.txt" "utf8-out.txt")))
    (assert (equal result "utf8-out.txt") nil "Function returned correct output file path for UTF-8 file")
    (assert (probe-file "utf8-out.txt") nil "UTF-8 output file exists")
    (assert (plusp (file-length (open "utf8-out.txt" :element-type 'character))) nil "UTF-8 output file is not empty")))

(defun test-integration-test ()
  "Test integration of build-suffix-array."
  (create-test-file "integration test content" "integration-test-input.txt")
  (let ((result (cl-suffix-array:build-suffix-array "integration-test-input.txt" "integration-test-output.txt")))
    (assert (equal result "integration-test-output.txt") nil "Function returned correct output file path for integration test")
    (assert (probe-file "integration-test-output.txt") nil "Integration output file exists")
    (assert (plusp (file-length (open "integration-test-output.txt" :element-type 'character))) nil "Integration output file is not empty")))

(defun test-suffix-array-functionality ()
  "Test suffix array functionality for pattern matching."
  (with-test-files ("test-object.txt" "output-object.txt")
    (with-open-file (out "test-object.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string "the quick brown fox jumps over the lazy dog" out))

    (cl-suffix-array:build-suffix-array "test-object.txt" "output-object.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-object.txt" "output-object.txt"))
           (found1 (cl-suffix-array:contains obj "fox"))
           (found2 (cl-suffix-array:contains obj "jumps"))
           (found3 (cl-suffix-array:contains obj "xyzzy")))
      (assert (and found1 found2 (not found3)) nil "Suffix array correctly finds existing patterns and rejects non-existing ones"))))

(defun test-suffix-array-unicode ()
  "Test suffix array with Unicode patterns."
  (with-test-files ("test-unicode.txt" "output-unicode.txt")
    (with-open-file (out "test-unicode.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string "Hello 世界, this is a test with 中文 characters" out))

    (cl-suffix-array:build-suffix-array "test-unicode.txt" "output-unicode.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-unicode.txt" "output-unicode.txt"))
           (found1 (cl-suffix-array:contains obj "世界"))
           (found2 (cl-suffix-array:contains obj "中文"))
           (found3 (cl-suffix-array:contains obj "xyz")))
      (assert (and found1 found2 (not found3)) nil "Suffix array correctly handles Unicode patterns"))))

(defun test-suffix-array-multiline ()
  "Test suffix array with multiline text."
  (with-test-files ("test-multiline.txt" "output-multiline.txt")
    (with-open-file (out "test-multiline.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "Line 1: Hello World~%Line 2: This is a test~%Line 3: With multiple lines~%Line 4: And more content~%"))

    (cl-suffix-array:build-suffix-array "test-multiline.txt" "output-multiline.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-multiline.txt" "output-multiline.txt"))
           (found1 (cl-suffix-array:contains obj "World"))
           (found2 (cl-suffix-array:contains obj "multiple lines"))
           (found3 (cl-suffix-array:contains obj "Line 2:"))
           (found4 (cl-suffix-array:contains obj "xyz")))
      (assert (and found1 found2 found3 (not found4)) nil "Suffix array correctly handles multiline text"))))

(defun test-find-pattern-function ()
  "Test find-pattern function."
  (with-test-files ("test-find.txt" "output-find.txt")
    (with-open-file (out "test-find.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string "banana bandana" out))

    (cl-suffix-array:build-suffix-array "test-find.txt" "output-find.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-find.txt" "output-find.txt"))
           (matches1 (cl-suffix-array:find-pattern obj "ana"))
           (matches2 (cl-suffix-array:find-pattern obj "ban"))
           (matches3 (cl-suffix-array:find-pattern obj "xyz")))
      (assert (= (length matches1) 3) nil "Should find 3 occurrences of 'ana'")
      (assert (= (length matches2) 2) nil "Should find 2 occurrences of 'ban'")
      (assert (= (length matches3) 0) nil "Should find 0 occurrences of 'xyz'"))))

(defun test-find-lines-with-pattern-function ()
  "Test find-lines-with-pattern function."
  (with-test-files ("test-lines.txt" "output-lines.txt")
    (with-open-file (out "test-lines.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "Line 1: Hello world~%Line 2: This is a test~%Line 3: Another line with world~%Line 4: Final line~%"))

    (cl-suffix-array:build-suffix-array "test-lines.txt" "output-lines.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-lines.txt" "output-lines.txt"))
           (lines1 (cl-suffix-array:find-lines-with-pattern obj "world"))
           (lines2 (cl-suffix-array:find-lines-with-pattern obj "test"))
           (lines3 (cl-suffix-array:find-lines-with-pattern obj "xyz")))
      (assert (= (length lines1) 2) nil "Should find 2 lines with 'world'")
      (assert (= (length lines2) 1) nil "Should find 1 line with 'test'")
      (assert (= (length lines3) 0) nil "Should find 0 lines with 'xyz'"))))

(defun run-simple-tests ()
  "Run all tests and report results. Returns T if all tests passed, NIL otherwise."
  (let ((failed-tests '())
        (total-tests 0))
    (labels ((run-test (name test-fn)
               (incf total-tests)
               (handler-case
                   (funcall test-fn)
                 (error (e)
                   (push name failed-tests)
                   (format *error-output* "FAILED: ~a~%  ~a~%" name e)))))
      (run-test "basic-functionality" #'test-basic-functionality)
      (run-test "file-size-handling" #'test-file-size-handling)
      (run-test "utf8-handling" #'test-utf8-handling)
      (run-test "integration-test" #'test-integration-test)
      (run-test "suffix-array-functionality" #'test-suffix-array-functionality)
      (run-test "suffix-array-unicode" #'test-suffix-array-unicode)
      (run-test "suffix-array-multiline" #'test-suffix-array-multiline)
      (run-test "find-pattern-function" #'test-find-pattern-function)
      (run-test "find-lines-with-pattern-function" #'test-find-lines-with-pattern-function))

    ;; Cleanup
    (dolist (file '("banana.txt" "banana-out.txt" "small-test.txt" "small-out.txt"
                    "utf8-test.txt" "utf8-out.txt" "integration-test-input.txt"
                    "integration-test-output.txt" "test-object.txt" "output-object.txt"
                    "test-unicode.txt" "output-unicode.txt" "test-multiline.txt"
                    "output-multiline.txt" "test-find.txt" "output-find.txt"
                    "test-lines.txt" "output-lines.txt" "temp-psascan"))
      (when (probe-file file) (delete-file file)))
    (when (probe-file "./temp-psascan")
      (ignore-errors (delete-file "./temp-psascan")))

    (format t "~&Test Results: ~a/~a tests passed~%" (- total-tests (length failed-tests)) total-tests)
    (if (endp failed-tests)
        (format t "All tests passed!~%")
        (format t "Failed tests: ~a~%" failed-tests))
    (endp failed-tests)))
