(in-package :cl-nlopt)

(defvar counts 0)

(defcallback myfunc :double ((n :unsigned-int) (x :pointer) (grad :pointer) (my_func_data :pointer))
  (incf counts)
  (let ((sqrtx1 (sqrt (mem-aref x :double 1))))
    ;; Set objective gradient when it isn't null
    (when (not (null-pointer-p grad))
      (setf (mem-aref grad :double 0) 0.0d0
	    (mem-aref grad :double 1) (/ 0.5d0 sqrtx1)))
    ;; Return objective function value
    sqrtx1))

(defcstruct my_constraint_data
  (a :double)
  (b :double))

(defcallback myconstraint :double ((n :unsigned-int) (x :pointer) (grad :pointer) (data :pointer))
  (with-foreign-slots ((a b) data (:struct my_constraint_data))
    (let ((x0 (mem-aref x :double 0))
          (x1 (mem-aref x :double 1)))
      ;; Set constraint gradient when it isn't null
      (when (not (null-pointer-p grad))
        (setf (mem-aref grad :double 0) (* 3 a (expt (+ (* a x0) b) 2))
	      (mem-aref grad :double 1) -1d0))
      ;; Return constraint value
      (- (expt (+ (* a x0) b) 3) x1))))

(defun test (&optional (alg :NLOPT_LD_MMA))
  (let ((opt (nlopt_create (foreign-enum-value 'nlopt_algorithm alg) 2))
	(data-type '(:struct my_constraint_data)))
    (with-foreign-objects ((lb :double 2)
			   (data0 data-type)
			   (data1 data-type)
			   (x :double 2)
			   (minf :double))
      (setf (mem-aref lb :double 0) most-negative-double-float)
      (setf (mem-aref lb :double 1) 0d0)
      (nlopt_set_lower_bounds opt lb)
      (nlopt_set_min_objective opt (get-callback 'myfunc) (null-pointer))
      (with-foreign-slots ((a b) data0 (:struct my_constraint_data))
	(setf a 2d0 b 0d0))
      (with-foreign-slots ((a b) data1 (:struct my_constraint_data))
	(setf a -1d0 b 1d0))
      (nlopt_add_inequality_constraint opt (get-callback 'myconstraint) data0 1d-8)
      (nlopt_add_inequality_constraint opt (get-callback 'myconstraint) data1 1d-8)
      ;;(nlopt_set_xtol_rel opt 1d-4)
      (setf (mem-aref x :double 0) 1.234d0
	    (mem-aref x :double 1) 5.678d0)
      (setf counts 0)
      (nlopt_optimize opt x minf)
      (format t "found minimum in ~a steps~&" counts)
      (format t "found minimum at (f ~a ~a) = ~a~&" (mem-aref x :double 0) (mem-aref x :double 1) (mem-ref minf :double))
      (nlopt_destroy opt))))
