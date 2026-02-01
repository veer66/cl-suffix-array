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

(test utf8-handling
  (create-test-file "héllo 世界" "utf8-test.txt")
  (let ((result (cl-suffix-array:build-suffix-array "utf8-test.txt" "utf8-out.txt")))
    (is (equal result "utf8-out.txt")
	"Function returned correct output file path for UTF-8 file")
    (is (probe-file "utf8-out.txt")
	"UTF-8 output file exists")
    (is (plusp (file-length (open "utf8-out.txt" :element-type 'character)))
	"UTF-8 output file is not empty")))

(defmacro with-test-files ((&rest files) &body body)
  "Execute body and cleanup specified test files afterwards."
  (let ((file-var (gensym "FILE")))
    `(unwind-protect
          (progn ,@body)
       (dolist (,file-var ',files)
         (when (probe-file ,file-var)
           (delete-file ,file-var))))))

(test integration-test
  (with-test-files ("integration-test-input.txt" "integration-test-output.txt")
    (let ((test-file "integration-test-input.txt")
          (output-file "integration-test-output.txt"))
      (create-test-file "integration test content" test-file)
      (let ((result (cl-suffix-array:build-suffix-array test-file output-file)))
        (is (equal result output-file)
            "Function returned correct output file path for integration test")
        (is (probe-file output-file)
            "Integration output file exists")
        (is (plusp (file-length (open output-file :element-type 'character)))
            "Integration output file is not empty")))))

(test suffix-array-functionality
  (with-test-files ("test-object.txt" "output-object.txt")
    (with-open-file (out "test-object.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string "the quick brown fox jumps over the lazy dog" out))

    (cl-suffix-array:build-suffix-array "test-object.txt" "output-object.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-object.txt" "output-object.txt"))
           (found1 (cl-suffix-array:contains obj "fox"))
           (found2 (cl-suffix-array:contains obj "jumps"))
           (found3 (cl-suffix-array:contains obj "xyzzy")))
      (is (and found1 found2 (not found3))
          "Suffix array correctly finds existing patterns and rejects non-existing ones"))))

(test suffix-array-unicode
  (with-test-files ("test-unicode.txt" "output-unicode.txt")
    (with-open-file (out "test-unicode.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string "Hello 世界, this is a test with 中文 characters" out))

    (cl-suffix-array:build-suffix-array "test-unicode.txt" "output-unicode.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-unicode.txt" "output-unicode.txt"))
           (found1 (cl-suffix-array:contains obj "世界"))
           (found2 (cl-suffix-array:contains obj "中文"))
           (found3 (cl-suffix-array:contains obj "xyz")))
      (is (and found1 found2 (not found3))
          "Suffix array correctly handles Unicode patterns"))))

(test suffix-array-multiline
  (with-test-files ("test-multiline.txt" "output-multiline.txt")
    (with-open-file (out "test-multiline.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "Line 1: Hello World~%Line 2: This is a test~%Line 3: With multiple lines~%Line 4: And more content~%"))

    (cl-suffix-array:build-suffix-array "test-multiline.txt" "output-multiline.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-multiline.txt" "output-multiline.txt"))
           (found1 (cl-suffix-array:contains obj "World"))
           (found2 (cl-suffix-array:contains obj "multiple lines"))
           (found3 (cl-suffix-array:contains obj "Line 2:"))
           (found4 (cl-suffix-array:contains obj "xyz")))
      (is (and found1 found2 found3 (not found4))
          "Suffix array correctly handles multiline text"))))

(test find-pattern-function
  (with-test-files ("test-find.txt" "output-find.txt")
    (with-open-file (out "test-find.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string "banana bandana" out))

    (cl-suffix-array:build-suffix-array "test-find.txt" "output-find.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-find.txt" "output-find.txt"))
           (matches1 (cl-suffix-array:find-pattern obj "ana"))
           (matches2 (cl-suffix-array:find-pattern obj "ban"))
           (matches3 (cl-suffix-array:find-pattern obj "xyz")))
      (is (and (= (length matches1) 3)  ; Should find "ana" at positions (1,4), (3,6), (11,14)
               (= (length matches2) 2)    ; Should find "ban" at positions (0,3), (7,10)
               (= (length matches3) 0))   ; Should find nothing for "xyz"
          "Find pattern function correctly identifies pattern occurrences"))))

(test find-lines-with-pattern-function
  (with-test-files ("test-lines.txt" "output-lines.txt")
    (with-open-file (out "test-lines.txt" :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "Line 1: Hello world~%Line 2: This is a test~%Line 3: Another line with world~%Line 4: Final line~%"))

    (cl-suffix-array:build-suffix-array "test-lines.txt" "output-lines.txt")

    (let* ((obj (cl-suffix-array:open-suffix-array "test-lines.txt" "output-lines.txt"))
           (lines1 (cl-suffix-array:find-lines-with-pattern obj "world"))
           (lines2 (cl-suffix-array:find-lines-with-pattern obj "test"))
           (lines3 (cl-suffix-array:find-lines-with-pattern obj "xyz")))
      (is (and (= (length lines1) 2)    ; Should find "world" in 2 lines
               (= (length lines2) 1)      ; Should find "test" in 1 line
               (= (length lines3) 0))     ; Should find nothing for "xyz"
          "Find lines with pattern function correctly identifies lines containing patterns"))))
