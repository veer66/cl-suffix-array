(defpackage #:cl-suffix-array
  (:use #:cl)
  (:export #:build-suffix-array
           #:build-suffix-array-external
           #:contains
           #:find-pattern
           #:find-lines-with-pattern
           #:suffix-array
           #:open-suffix-array
           #:suffix-array-original-text-pathname
           #:suffix-array-suffix-array-pathname
           #:read-text
           #:get-file-size
           #:process-block
           #:perform-psascan-merge
           #:process-text-block
           #:process-all-blocks
           #:sufsort))
