(defpackage :cl-nlopt
  (:use :cl :cffi))

(in-package :cl-nlopt)

;; Define & use NLOPT shared library

(define-foreign-library libnlopt
  (t (:default "libnlopt")))

(use-foreign-library libnlopt)
