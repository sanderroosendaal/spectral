;; Macro for unary ops: (def-unary-op <spectral-name> <fn-name> <operator> &optional docstring)
(defmacro def-unary-op (op-name fn-name operator &optional docstring)
  (let ((doc (or docstring (string-capitalize (string op-name)))))
    `(progn
       (defun ,fn-name (a)
         ,doc
         (array-fn ,operator a))
       (register-op ',op-name #',fn-name 1))))

;; basics
(defun add-fn (a b)
  "Addition"
  (array-op #'+ b a))

(defun sub-fn (a b)
  "Subtraction"
  (array-op #'- b a))

(defun mul-fn (a b)
  "Multiplication"
  (array-op #'* b a))

(defun div-fn (a b)
  "Division"
  (array-op #'/ b a))


(register-op '+ #'add-fn 2)
(register-op '- #'sub-fn 2)
(register-op '* #'mul-fn 2)
(register-op '% #'div-fn 2)

;; change-sign, abs, intg, frac, rnd
(defun complex-fn (a b)
  "Creates complex number a+ib"
  (array-op #'complex a b))

(def-unary-op chs chs (lambda (x) (- 0 x)) "Change sign a -> -a")
(def-unary-op abs abs-fn #'abs "Absolute value, also works for complex numbers")
(def-unary-op re realpart-fn #'realpart "Get the real part of complex number")
(def-unary-op im imagpart-fn #'imagpart "Get the imaginary part of complex number")
(def-unary-op intg intg-fn #'floor "Floor")
(def-unary-op frac frac-fn (lambda (x) (mod x 1)) "Returns the fractional part 1.23 -> .23")
(def-unary-op rnd rnd-fn #'round "Round")

(register-op 'complex #'complex-fn 2)

;; max, min
(defun max-fn (a b)
  "Maximum"
  (array-op #'max a b))

(defun min-fn (a b)
  "Minimum"
  (array-op #'min a b))

(register-op 'max #'max-fn 2)
(register-op 'min #'min-fn 2)

;; power, roots, logarithms
(def-unary-op square sqr-fn (lambda (x) (* x x)) "Square a^2")
(def-unary-op sqrt sqrt-fn #'sqrt "Square root")
(def-unary-op exp exp-fn #'exp "Exponential: e^a")
(def-unary-op ln log-fn #'log "Natural logarithm ln a")
(def-unary-op 10^x 10^x (lambda (x) (expt 10 x)) "10^a")
(def-unary-op log 10log-fn (lambda (x) (log x 10)) "10log a")
(def-unary-op 1/x 1/x-fn (lambda (x) (coerce (/ x) 'double-float)) "Reciprocal: 1/a")

(defun y^x-fn (a b)
  "y^x first on stack is exponent, second is base: y^x 3 2 --> 8"
  (array-op (lambda (x y) (expt x y)) b a))

(register-op 'y^x #'y^x-fn 2)

;; trigonometry
(def-unary-op sin sin-fn #'sin)
(def-unary-op cos cos-fn #'cos)
(def-unary-op tan tan-fn #'tan)
(def-unary-op asin asin-fn #'asin)
(def-unary-op acos acos-fn #'acos)
(def-unary-op atan atan-fn #'atan)
(def-unary-op sinh sinh-fn #'sinh)
(def-unary-op cosh cosh-fn #'cosh)
(def-unary-op tanh tanh-fn #'tanh)
(def-unary-op asinh asinh-fn #'asinh)
(def-unary-op acosh acosh-fn #'acosh)
(def-unary-op atanh atanh-fn #'atanh)

;; miscellaneous
(defun fact-fn (a)
  "Factorial ! 8 -> 40320"
  (labels ((fact (n)
	     (cond
	       ((= n 0) 1)
	       ((= n 1) 1)
	       ((> n 1) (* n (fact (1- n)))))))
    (array-fn #'fact a)))

(register-op '! #'fact-fn 1)


;; coordinates 0 ->P and ->R
;; for now, takes a [x y] coordinate vector and returns [r theta] in radians (as vector)
(defun ->P (a)
  "Converts a = [x, y] in Rectangular coordinates to Polar coordinates [r, theta]"
  (let ((a (if (arrayp a) (coerce a 'list) a)))
    (cond
      ((not (listp a))
       (spectral-error "->P takes a vector [x y] of length 2"))
      ((/= (length a) 2)
       (spectral-error "->P takes a vector [x y] of length 2"))
      ((= (first a) 0)
       (coerce (list (abs (second a))
                     (if (> (second a) 0) (/ pi 2) (- (/ pi 2))))
               'vector))
      ((= (second a) 0)
       (coerce (list (abs (first a))
                     (if (> (first a) 0) 0 pi))
               'vector))
      (t
       (let ((x (first a))
             (y (second a)))
         (coerce (list (sqrt (+ (* x x) (* y y))) (atan (/ y x)))
                 'vector))))))

(defun ->R (a)
  "Converts a = [r, theta] in Polar coordinates to Rectangular coordinates [x, y]"
  (let ((a (if (arrayp a) (coerce a 'list) a)))
    (when (or (not (listp a)) (/= (length a) 2))
      (spectral-error "->R takes a vector [r theta] of length 2"))
    (let ((r (first a))
          (theta (second a)))
      (coerce (list (* r (cos theta)) (* r (sin theta))) 'vector))))

(register-op '->P #'->P 1)
(register-op '->R #'->R 1)
