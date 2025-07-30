;; basics - we have 1 = True, 0 = False
(defun greater-fn (a b) (array-op (lambda (b a) (if (> a b) 1 0)) a b))
(defun greater-equal-fn (a b) (array-op (lambda (b a) (if (>= a b) 1 0)) a b))
(defun smaller-fn (a b) (array-op (lambda (b a) (if (< a b) 1 0)) a b))
(defun smaller-equal-fn (a b) (array-op (lambda (b a) (if (<= a b) 1 0)) a b))
(defun eql-fn (a b) (array-op (lambda (b a) (if (= a b) 1 0)) a b))
(defun not-eql-fn (a b) (array-op (lambda (b a) (if (= a b) 0 1)) a b))

(register-op '> #'greater-fn 2)
(register-op '>= #'greater-equal-fn 2)
(register-op '< #'smaller-fn 2)
(register-op '<= #'smaller-equal-fn 2)
(register-op 'eq #'eql-fn 2)
(register-op 'neq #'not-eql-fn 2)
