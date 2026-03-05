;; basics - we have 1 = True, 0 = False
(defmacro def-filter (name pred &optional invert docstring)
  "Define a filter op: (name a b) -> array of 0/1 where (pred a b) holds.
   Does not register; use (register-op 'sym #'name 2) separately."
  (let ((doc (or docstring (format nil "~A threshold array -> 0/1 mask" pred))))
    `(defun ,name (a b)
       ,doc
       (array-op (lambda (b a) (if (,pred a b) ,(if invert 0 1) ,(if invert 1 0))) a b))))

(def-filter greater-fn > nil "Greater than, > 5 [1 2 3 4 5 6 7] -> [0 0 0 0 0 1 1]")
(def-filter greater-equal-fn >= nil "Greater than or equal, >= 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 1 1]")
(def-filter smaller-fn < nil "Smaller than, < 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 0 0]")
(def-filter smaller-equal-fn <= nil "Smaller than or equal, <= 5 [1 2 3 4 5 6 7] -> [1 1 1 1 1 0 0]")
(def-filter eql-fn = nil "Equal, eq 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 0 0]")
(def-filter not-eql-fn = t "Not Equal, neq 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 1 1]")

(register-op '> #'greater-fn 2)
(register-op '>= #'greater-equal-fn 2)
(register-op '< #'smaller-fn 2)
(register-op '<= #'smaller-equal-fn 2)
(register-op 'eq #'eql-fn 2)
(register-op 'neq #'not-eql-fn 2)
