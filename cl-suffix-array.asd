(defsystem #:cl-suffix-array
  :description "A library for creating and using suffix arrays."
  :author "Vee Satayamas <veerpub@pm.me>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:uiop)
  :serial t
  :components ((:file "package")
               (:file "cl-suffix-array")
               (:file "cl-suffix-array-test")))
