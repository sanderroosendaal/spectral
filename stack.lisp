;; Stack manipulation functions
(defun push-stack (value)
  (push value *stack*))

(defun pop-stack ()
  (if (null *stack*)
      (error "Stack underflow: cannot pop from an empty stack")
      (pop *stack*)))

(defun peek-stack ()
  (or
   (first *stack*)
   (progn
     (pop-stack)
     (peek-stack))))


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

;; could be better, but it's better than nothing
(defun pretty-print-stack-item (item)
  (cond
    ((null item) "")
    ((numberp item) (format t "~A~%" item))
    ((stringp item) (format t "~A~%" item))
    ((and (listp item) (numberp (first item)))
     (if (< (length item) 10)
	 (format t "[ ~{~A~^ ~} ]~%" item)
	 (format t "[ ~{~A~^ ~} ... ]~%" (subseq item 0 10))))
    ((listp item)
     (if (< (length item) 10)
	 (progn
	   (format t "[~%")
	   (loop for v in item
		 for i from 0
		 do (progn
		      (when (> i 0) (format t ""))
		      (pretty-print-stack-item v)))
	   (format t "]~%"))
	 (let ((new-item (subseq item 0 10)))
	   (format t "[~%")
	   (loop for v in new-item
		 for i from 0
		 do (progn
		      (when (> i 0) (format t ""))
		      (pretty-print-stack-item v)))
	   (format t " ... ]~%"))))
    (t (format t "~A~%" item))))

(defun pretty-print-stack ()
  "Print the top 5 items of the stack in a readable format."
  (if (null *stack*)
      (format t "Empty Stack")
      (let ((n (min (length *stack*) 5)))
	(loop for item in (reverse (subseq *stack* 0 n))
	      do
		 (format t "--~%")
		 (pretty-print-stack-item item)))))

(register-stack-op 'dup #'dup 0)
(register-stack-op 'swap #'swap 0)
(register-stack-op 'pop #'pop-stack 0)
(register-stack-op 'peek #'pretty-print-stack 0)
