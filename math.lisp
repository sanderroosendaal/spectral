;; basics
(defun add-fn (a b) (array-op #'+ b a))
(defun sub-fn (a b) (array-op #'- b a))
(defun mul-fn (a b) (array-op #'* b a))
(defun div-fn (a b) (array-op #'/ b a))


(register-op '+ #'add-fn 2)
(register-op '- #'sub-fn 2)
(register-op '* #'mul-fn 2)
(register-op '% #'div-fn 2)

;; change-sign, abs, intg, frac, rnd
(defun complex-fn (a b) (array-op #'complex a b))
(defun chs (a) (array-fn (lambda (x) (- 0 x)) a))
(defun abs-fn (a) (array-fn #'abs a))
(defun intg-fn (a) (array-fn #'floor a))
(defun frac-fn (a) (array-fn (lambda (x) (mod x 1)) a))
(defun rnd-fn (a) (array-fn #'round a))

(register-op 'complex #'complex-fn 2)
(register-op 'chs #'chs 1)
(register-op 'abs #'abs-fn 1)
(register-op 'intg #'intg-fn 1)
(register-op 'frac #'frac-fn 1)
(register-op 'rnd #'rnd-fn 1)

;; max, min
(defun max-fn (a b) (array-op #'max a b))
(defun min-fn (a b) (array-op #'min a b))

(register-op 'max #'max-fn 2)
(register-op 'min #'min-fn 2)

;; power, roots, logarithms
(defun sqr-fn (a) (array-fn (lambda (x) (* x x)) a))
(defun sqrt-fn (a) (array-fn #'sqrt a))
(defun exp-fn (a) (array-fn #'exp a))
(defun log-fn (a) (array-fn #'log a))
(defun 10^x (a) (array-fn (lambda (x) (expt 10 x)) a))
(defun 10log-fn (a) (array-fn (lambda (x) (log x 10)) a))
(defun y^x-fn (a b) (array-op (lambda (x y) (expt x y)) b a))
(defun 1/x-fn (a) (array-fn (lambda (x) (coerce (/ x) 'double-float)) a))

(register-op 'square #'sqr-fn 1)
(register-op 'sqrt #'sqrt-fn 1)
(register-op 'exp #'exp-fn 1)
(register-op 'ln #'log-fn 1)
(register-op '10^x #'10^x 1)
(register-op 'log #'10log-fn 1)
(register-op 'y^x #'y^x-fn 2)
(register-op '1/x #'1/x-fn 1)

;; trigonometry 
(defun sin-fn (a) (array-fn #'sin a))
(defun cos-fn (a) (array-fn #'cos a))
(defun tan-fn (a) (array-fn #'tan a))
(defun asin-fn (a) (array-fn #'asin a))
(defun acos-fn (a) (array-fn #'acos a))
(defun atan-fn (a) (array-fn #'atan a))
(defun sinh-fn (a) (array-fn #'sinh a))
(defun cosh-fn (a) (array-fn #'cosh a))
(defun tanh-fn (a) (array-fn #'tanh a))
(defun asinh-fn (a) (array-fn #'asinh a))
(defun acosh-fn (a) (array-fn #'acosh a))
(defun atanh-fn (a) (array-fn #'atanh a))


(register-op 'sin #'sin-fn 1)
(register-op 'cos #'cos-fn 1)
(register-op 'tan #'tan-fn 1)
(register-op 'asin #'asin-fn 1)
(register-op 'acos #'acos-fn 1)
(register-op 'atan #'atan-fn 1)
(register-op 'sinh #'sinh-fn 1)
(register-op 'cosh #'cosh-fn 1)
(register-op 'tanh #'tanh-fn 1)
(register-op 'asinh #'asinh-fn 1)
(register-op 'acosh #'acosh-fn 1)
(register-op 'atanh #'atanh-fn 1)

;; miscellaneous
(defun fact-fn (a)
  (labels ((fact (n)
	     (cond
	       ((= n 0) 1)
	       ((= n 1) 1)
	       ((> n 1) (* n (fact (1- n)))))))
    (array-fn #'fact a)))

(register-op '! #'fact-fn 1)

;; coordinates 0 ->P and ->R
;; for now, takes a [x y] coordinate vector and returns [r theta] in radians
(defun ->P (a)
  "Converts a = [x, y] in Rectangular coordinates to Polar coordinates [r, theta]"
  (cond
    ((not (listp a))
     (error "->P takes a vector [x y] of length 2"))
    ((/= (length a) 2)
     (error "->P takes a vector [x y] of length 2"))
    ((= (first a) 0)
     (if (> (second a) 0)
	 (/ pi 2)
	 (- 0 (/ pi 2))))
    ((= (second a) 0)
     (if (> (first a) 0)
	 0
	 pi))
    (t
      (let ((x (first a))
	    (y (second a)))
	(list
	 (sqrt (+ (* x x) (* y y)))
	 (atan (/ y x)))))))

(defun ->R (a)
  "Converts a = [r, theta] in Polar coordinates to Rectangular coordinates [x, y]"
  (let ((r (first a))
	(theta (second a)))
    (list (* r (cos theta)) (* r (sin theta)))))

(register-op '->P #'->P 1)
(register-op '->R #'->R 1)
