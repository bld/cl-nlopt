(in-package :cl-nlopt)

;; Load cmu-infix readtable
(named-readtables:in-readtable cmu-infix:syntax)

(defcstruct rosenbrock_data
  (a :double)
  (b :double))

(defcallback rosenbrock_func :double ((n :unsigned-int) (x :pointer) (grad :pointer) (data :pointer))
  (with-foreign-slots ((a b) data (:struct rosenbrock_data))
    (let ((x0 (mem-aref x :double 0))
          (x1 (mem-aref x :double 1)))
      (when (not (null-pointer-p grad))
        (setf (mem-aref grad :double 0) #I(-4*b*x0*(x1 - x0^^2) - 2*(a - x0))
              (mem-aref grad :double 1) #I(2*b*(x1 - x0^^2))))
      #I((a - x0)^^2 + b*(x1 - x0^^2)^^2))))

(defcallback rosenbrock_constraint :double ((n :unsigned-int) (x :pointer) (grad :pointer) (data :pointer))
  (let ((x0 (mem-aref x :double 0))
        (x1 (mem-aref x :double 1)))
    ;; Set constraint gradient
    (when (not (null-pointer-p grad))
      (setf (mem-aref grad :double 0) #I(2*x0)
            (mem-aref grad :double 1) #I(2*x1)))
    ;; Return constraint value
    #I(x0^^2 + x1^^2 - 1d0)))
    
(defun test-slsqp (&key (n 2) (alg :nlopt_ld_slsqp) (constr_tol 1d-8) (rel_tol 1d-4) (x0 #(0d0 0d0)))
  (let ((opt (nlopt_create (foreign-enum-value 'nlopt_algorithm alg) n))
        (data-type '(:struct rosenbrock_data)))
    (with-foreign-objects ((rdata data-type)
                           (x :double 2)
                           (min_f :double))
      ;; Set Rosenbrock function constants
      (with-foreign-slots ((a b) rdata (:struct rosenbrock_data))
        (setf a 1d0 b 100d0))
      ;; Set objective function
      (nlopt_set_min_objective opt (get-callback 'rosenbrock_func) rdata)
      ;; Set inequality constraint
      (nlopt_add_inequality_constraint opt (get-callback 'rosenbrock_constraint) (null-pointer) constr_tol)
      ;; Set initial guess
      (dotimes (i 2)
        (setf (mem-aref x :double i) (aref x0 i)))
      ;; Set relative tolerance
      (nlopt_set_xtol_rel opt rel_tol)
      ;; Optimize
      (print (nlopt_optimize opt x min_f))
      (format t "Found minimum at (f ~a ~a) = ~a ~&" (mem-aref x :double 0) (mem-aref x :double 1) (mem-ref min_f :double))
      ;; Deallocate opt
      (nlopt_destroy opt))))
