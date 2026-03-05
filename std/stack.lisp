;; Stack manipulation functions
(defun push-stack (value)
  (if (and value (or (not (stringp value)) (not (string= value ""))))
      (progn
  (push value *stack*)
  (first *stack*))))

(defun pop-stack ()
  "Pops the top element off the stack"
  (if (null *stack*)
      (spectral-error "Stack underflow: cannot pop from an empty stack")
      (pop *stack*)))

(defun peek-stack ()
  (first *stack*))


(defun dup ()
  "Duplicate the top element of the stack."
  (let ((top (pop-stack)))
    (push-stack top)
    (push-stack top)
    (peek-stack)))

(defun swap ()
  "Swap top and second element of the stack"
  (let ((top (pop-stack))
  (second (pop-stack)))
    (push-stack top)
    (push-stack second)))

(defun pretty-print-array (array &optional (stream *standard-output*))
  "Pretty-print an n-dimensional ARRAY with square brackets, showing
a maximum of *print-limit-per-dim* items per dimension (elided with ...)."
  (labels ((row-major-index (indices)
       (let ((dims (array-dimensions array)))
         (reduce (lambda (acc idx-dim)
       (+ (* acc (second idx-dim)) (first idx-dim)))
           (mapcar #'list indices dims)
           :initial-value 0)))
     (get-element (indices)
       (row-major-aref array (row-major-index indices)))
     (print-sub (indices)
       (let ((dim (length indices))
       (rank (array-rank array)))
         (if (= dim rank)
       ;; Base case: we've reached a scalar element
       (princ (get-element indices) stream)
       ;; Recursive case: print subarrays
       (progn
         (princ "[" stream)
         (let* ((dim-size (array-dimension array dim))
          (limit (min *print-limit-per-dim* dim-size)))
           (dotimes (i limit)
       (when (> i 0)
         (princ " " stream))
       (print-sub (append indices (list i))))
           (when (< limit dim-size)
       (princ " ..." stream)))
         (princ "]" stream))))))
    (print-sub '())
    (terpri stream)))



(defun pretty-print-stack-item (item)
  "Print a single stack item (number, string, list, or array) to *standard-output*."
  (cond
    ((null item) "")
    ((numberp item) (format t "~A~%" item))
    ((stringp item) (format t "~A~%" item))
    ((and (listp item) (numberp (first item)))
     (if (< (length item) *print-limit-per-dim*)
   (format t "[ ~{~A~^ ~} ]~%" item)
   (format t "[ ~{~A~^ ~} ... ]~%" (subseq item 0 *print-limit-per-dim*))))
    ((listp item)
     (if (< (length item) *print-limit-per-dim*)
   (progn
     (format t "[~%")
     (loop for v in item
     for i from 0
     do (progn
          (when (> i 0) (format t ""))
          (pretty-print-stack-item v)))
     (format t "]~%"))
   (let ((new-item (subseq item 0 *print-limit-per-dim*)))
     (format t "[~%")
     (loop for v in new-item
     for i from 0
     do (progn
          (when (> i 0) (format t ""))
          (pretty-print-stack-item v)))
     (format t " ... ]~%"))))
    ((arrayp item)
     (pretty-print-array item))
    (t (format t "~A~%" item))))

(defun pretty-print-stack ()
  "Print the top N items of the stack in a readable format (N = *peek-stack-limit*)."
  (if (null *stack*)
      (format t "Empty Stack")
      (let ((n (min (length *stack*) *peek-stack-limit*)))
  (loop for item in (reverse (subseq *stack* 0 n))
        do
     (format t "--~%")
     (pretty-print-stack-item item)))))

(register-stack-op 'dup #'dup 0)
(register-stack-op 'd #'dup 0)
(register-stack-op 'swap #'swap 0)
(register-stack-op 'pop #'pop-stack 0)
(register-stack-op 'peek #'pretty-print-stack 0)
