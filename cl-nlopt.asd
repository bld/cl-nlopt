(asdf:defsystem :cl-nlopt
  :author "Ben Diedrich"
  :license "MIT"
  :description "Common Lisp CFFI interface to NLOPT"
  :depends-on ("cffi" "cmu-infix")
  :serial t
  :components
  ((:file "package")
   (:file "swig")
   (:file "nlopt")
   (:file "test-slsqp")))
