(defsystem "cl-suffix-array"
  :description "A library for creating and using suffix arrays."
  :author "Vee Satayamas <veerpub@pm.me>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:uiop)
  :serial t
  :components ((:file "package")
               (:file "cl-suffix-array"))
  :in-order-to ((test-op (test-op "cl-suffix-array/tests"))))

(defsystem "cl-suffix-array/tests"
    :depends-on ("cl-suffix-array" "fiveam")
    :components ((:module "tests"
		  :components ((:file "cl-suffix-array-tests"))))
    :perform (test-op (o c)
		      (symbol-call :fiveam '#:run!)))
