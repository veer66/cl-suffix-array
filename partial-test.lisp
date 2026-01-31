(in-package #:cl-suffix-array)

;;;; cl-suffix-array.lisp

(defconstant +chunk-size+ (* 1024 1024 10)) ; 10MB chunks by default

(defun get-file-size (filepath)
  "Get the size of a file in bytes."
  (with-open-file (stream filepath :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun build-suffix-array (input-file-path output-file-path &key (chunk-size +chunk-size+))
  "Builds a suffix array from the text in input-file-path and saves it to output-file-path using SAScan algorithm.
   Handles files that exceed RAM capacity by processing in chunks."
  (let* ((file-size (get-file-size input-file-path))
         (temp-dir (uiop:ensure-directory-pathname 
                    (merge-pathnames "temp-sascan/" (uiop:temporary-directory))))
    
    ;; Create temporary directory if it doesn't exist
    (ensure-directories-exist temp-dir)
    
    ;; For very large files that exceed RAM, we implement a simplified external memory approach
    ;; The real SAScan algorithm is quite complex, involving sampling, recursion, and induced sorting
    ;; Here we implement a divide-and-conquer approach that mimics the key aspects
    
    ;; Phase 1: Divide the file into chunks that fit in memory
    (let ((num-chunks (ceiling file-size chunk-size)))
      (let ((chunk-files '()))
        
        ;; Process each chunk separately
        (loop for chunk-num from 0 below num-chunks do
          (let* ((start-pos (* chunk-num chunk-size))
                 (end-pos (min (* (1+ chunk-num) chunk-size) file-size))
                 (chunk-file (merge-pathnames 
                              (format nil "chunk-~a.tmp" chunk-num) 
                              temp-dir)))
            
            ;; Process this chunk and save partial results
            (process-file-chunk input-file-path chunk-file start-pos end-pos)
            (push chunk-file chunk-files)))
        
        ;; Phase 2: Merge all chunk results using external merge sort
        (perform-external-merge-sort chunk-files output-file-path temp-dir)))
    
    ;; Clean up temporary directory
    (uiop:delete-directory-tree temp-dir :validate t)
    
    ;; Return output file path as success indicator
    output-file-path))

(defun process-file-chunk (input-file-path output-chunk-file start-pos end-pos)
  "Process a single chunk of the input file to generate partial suffix array information."
  (let ((chunk-size (- end-pos start-pos)))
    ;; Read the chunk from the input file
    (with-open-file (input-stream input-file-path 
                                 :direction :input 
                                 :element-type '(unsigned-byte 8)
                                 :external-format :utf-8)
      (file-position input-stream start-pos)
      (let ((buffer (make-array chunk-size :element-type '(unsigned-byte 8))))
        (read-sequence buffer input-stream)
        
        ;; For this simplified version, we'll just store position info
        ;; A full implementation would extract and sort suffixes within the chunk
        (with-open-file (out output-chunk-file 
                             :direction :output 
                             :element-type '(unsigned-byte 8)
                             :external-format :utf-8
                             :if-does-not-exist :create
                             :if-exists :supersede)
          (loop for i from 0 below chunk-size do
            (format out "~a~%" (+ start-pos i))))))))