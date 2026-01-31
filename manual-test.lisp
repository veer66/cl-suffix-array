(in-package #:cl-user)
(require :asdf)
(asdf:load-system :cl-suffix-array)

(defun test-build-suffix-array ()
  "Simple manual test for the build-suffix-array function."
  (format t "Testing build-suffix-array function...~%")

  ;; Create a test file
  (with-open-file (out "manual-test.txt"
                       :direction :output
                       :if-exists :supersede
                       :external-format :utf-8)
    (write-string "banana" out))

  ;; Call the function
  (let ((result (cl-suffix-array:build-suffix-array "manual-test.txt" "manual-output.txt")))
    (format t "Function returned: ~a~%" result)

    ;; Check if output file was created
    (if (probe-file "manual-output.txt")
        (format t "SUCCESS: Output file created~%")
        (format t "FAILURE: Output file not created~%"))

    ;; Display contents of output file
    (format t "Contents of output file:~%")
    (with-open-file (in "manual-output.txt" :external-format :utf-8)
      (loop for line = (read-line in nil nil)
            while line do
            (format t "~a~%" line)))

    ;; Cleanup
    (when (probe-file "manual-test.txt") (delete-file "manual-test.txt"))
    (when (probe-file "manual-output.txt") (delete-file "manual-output.txt")))

  (format t "Test completed.~%"))

;; Run the test
(test-build-suffix-array)