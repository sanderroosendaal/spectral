(defparameter *stack* nil)

(defun push-stack (value)
  (push value *stack*))

(defun pop-stack ()
  (pop *stack*))

(defun peek-stack ()
  (first *stack*))

;; Array operations
(defun array-op (op a b)
  "Apply binary operation element-wise"
  (cond
    ((and (numberp a) (numberp b)) (funcall op a b))
    ((numberp a) (mapcar (lambda (x) (funcall op a x)) b))
    ((numberp b) (mapcar  (lambda (x) (funcall op x b)) a))
    (t (mapcar op a b))))

;; Built-in functions
(defun range-fn (n)
  (loop for i from 0 below n collect i))

(defun add-fn (a b) (array-op #'+ b a))

(defun sub-fn (a b) (array-op #'- b a))

(defun mul-fn (a b) (array-op #'* b a))

(defun div-fn (a b) (array-op #'/ b a))

;; trigonometry (element-wise)
(defun sin-fn (a) (if (numberp a) (sin a) (mapcar #'sin a)))

(defun cos-fn (a) (if (numberp a) (cos a) (mapcar #'cos a)))

(defun tan-fn (a) (if (numberp a) (tan a) (mapcar #'tan a)))

;; Constants
(defparameter *constants*
  `((pi . ,(coerce pi 'double-float))
    (e . ,(exp 1.0d0))))

;; Variables and functions storage
(defparameter *variables* (make-hash-table))

(defparameter *functions* (make-hash-table))

;; Symbol table
(defparameter *ops*
  (list (cons '+ #'add-fn)
	(cons '- #'sub-fn)
	(cons '* #'mul-fn)
	(cons '% #'div-fn)
	(cons 'range #'range-fn)
	(cons 'sin #'sin-fn)
	(cons 'cos #'cos-fn)
	(cons 'tan #'tan-fn)))

(defun execute-token (token)
  "Execute a single token"
  (cond
    ;; Numbers push themselves
    ((numberp token)
     (push-stack token))

    ;; Arrays push themselves
    ((and (listp token) (eq (first token) 'array))
     (push-stack (rest token)))

    ;; Constants
    ((assoc token *constants*)
     (push-stack (cdr (assoc token *constants*))))

    ;; Variables
    ((gethash token *variables*)
     (push-stack (gethash token *variables*)))

    ;; Functions - check if it's a user-defined function
    ((gethash token *functions*)
     (funcall (gethash token *functions*)))

    ;; File operations (special case - take filename from stack)
    ((eq token 'load)
     (let ((filename (pop-stack)))
       (push-stack (load-numbers filename))))

    ;; Unary operations
    ((member token '(range sin cos tan))
     (let ((a (pop-stack)))
       (push-stack (funcall (cdr (assoc token *ops*)) a))))

    ;; Binary operations
    ((member token '(+ - * %))
     (let ((a (pop-stack))
	   (b (pop-stack)))
       (push-stack (funcall (cdr (assoc token *ops*)) a b))))

    (t (error "Unknown token:: ~A" token))))

(defun parse-array (tokens)
  "Parse array lieral [1 2 3] from token stream"
  (let ((result '())
	(depth 0))
    (loop for token in tokens
	  when (eq token '[) do (incf depth)
	    when (eq token ']) do (decf depth)
	      when (and (> depth 0) (not (member token '([ ]))))
		do (push token result)
	  when (= depth 0) return (cons 'array (reverse result)))))

(defun tokenize (expr-string)
  "Simple tokenizer"
  (let ((tokens '()))
    (with-input-from-string (s expr-string)
      (loop for token = (read s nil nil)
	    while token
	    do (push token tokens)))
    (reverse tokens)))

(defun evaluate (expr-string)
  "Main evaluation function"
  (setf *stack* nil)
  (let ((tokens (tokenize expr-string)))
    ;; Check for assignment
    (if (and (>= (length tokens) 3) (eq (second tokens) '=))
	(handle-assignment (first tokens) (cddr tokens))
	;; Normal evaluation: process tokens right to left
	(progn (dolist (token (reverse tokens))
		 (unless (eq token '[)
		   (execute-token token)))
	       (peek-stack)))))

(defun handle-assignment (name expr-tokens)
  "Handle variable/function assignment"
  (let ((result (handler-case
		    ;; Try to evaluate the expression
		    (progn
		      (setf *stack* nil)
		      (dolist (token (reverse expr-tokens))
			(unless (eq token '[)
			  (execute-token token)))
		      (peek-stack))
		  ;; If stack underflow, it's a function definition
		  (error
		      (condition)
		    (declare (ignore condition))
		    ;; Store as function - simplified for now
		    (lambda ()
		      (setf *stack* nil)
		      (dolist
			  (token (reverse expr-tokens))
			(unless (eq token'[)
			  (execute-token token)))
		      (peek-stack))))))
    (if (functionp result)
	(setf (gethash name *functions*) result)
	(setf (gethash name *variables*) result))
    result))

;; Simple file I/O functions
(defun load-numbers (filename)
  "Load numbers from a text file (one per line)"
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
	  while line
	  collect (parse-number line))))

(defun parse-number (str)
  "Parse a string as a number"
  (handler-case (read-from-string str)
    (error () 0)))

(defun run-script (filename)
  "Execute a script file line by line"
   (with-open-file (stream filename :direction :input)
     (let ((results '()))
       (loop for line = (read-line stream nil nil)
	     while line
	     when (and (> (length line) 0)
		       (not (char= (char line 0) #\;))) ; Skip comments
		  (do (let ((result (evaluate (string-trim " " line))))
			(when result
			  (push (list line result) results)))))
       (reverse results))))

;; Add file operations
(setf *ops* (append *ops* (list (cons 'load #'load-numbers)
				(cons 'run #'run-script))))
