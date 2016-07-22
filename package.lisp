(defpackage :cl-nlopt
  (:use :cl :cffi))

(in-package :cl-nlopt)

(define-foreign-library libnlopt
  (t (:default "libnlopt-0")))

(use-foreign-library libnlopt)
