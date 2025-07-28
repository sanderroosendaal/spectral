(defparameter *stack* nil)

(defun push-stack (value)
  (push value *stack*))

(defun pop-stack ()
  (if (null *stack*)
      (error "Stack underflow: cannot pop from an empty stack")
      (pop *stack*)))

(defun peek-stack ()
  (first *stack*))

;; Array operations
(defun array-op (op a b)
  "Apply binary operation element-wise"
  (cond
    ((and (numberp a) (numberp b)) (funcall op a b))
    ((and (numberp a) (listp b)) (mapcar (lambda (x) (array-op op a x)) b))
    ((and (numberp b) (listp a)) (mapcar (lambda (x) (array-op op x b)) a))
    ((and (listp a) (listp b))
     (unless (= (length a) (length b))
       (error "Mismatched array lengths: ~S and ~S" a b))
     (mapcar #'(lambda (x y) (array-op op x y)) a b))
    (t (error "Invalid inputs: ~S and ~S" a b))))
     
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

(defun execute-token (token &optional (debug nil))
  "Execute a single token"
  (when debug
    (format t "Execute-token ~A, stack ~A%" token *stack*))
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
  "Parses the last well-formed bracketed matrix in TOKENS from right to left.
Returns (values tokens-before-matrix matrix).
Signals an error on invalid tokens or unmatched brackets."
  (labels
      ((scan-right (tokens)
	 (let ((stack '())
	       (matrix-tokens '())
	       (depth 0))
	   (loop for token in (reverse tokens)
		 do (cond
		      ((eql token '])
		       (incf depth)
		       (push token matrix-tokens))
		      ((eql token '[)
		       (decf depth)
		       (push token matrix-tokens)
		       (when (< depth 0)
			 (error "Unmatched [")))
		      ((or (numberp token)
			   (member token '(] [)))
		       (push token matrix-tokens))
		      (t (error "Invalid token ~S" token)))
		 while (> depth 0))
	   (if (/= depth 0)
	       (error "Unmatched brackets in matrix")
	       (let ((matrix-len (length matrix-tokens)))
		 (values
		  (subseq tokens 0 (- (length tokens) matrix-len))
		  (reverse matrix-tokens))))))
       
       (parse (tokens)
	 (cond
	   ((null tokens)
	    (values nil nil))
	   ((eql (first tokens) '[)
	    (multiple-value-bind (sublist rest)
		(parse-list (rest tokens))
	      (multiple-value-bind (tail result)
		  (parse rest)
		(values (cons sublist tail) result))))
	   (t (values nil tokens))))
       
       (parse-list (tokens)
	 (let ((result '()))
	   (loop
	     (cond
	       ((null tokens)
		(error "Unmatched ["))
	       ((eql (first tokens) '])
		(return (values (nreverse result) (rest tokens))))
	       ((numberp (first tokens))
		(push (first tokens) result)
		(setf tokens (rest tokens)))
	       ((eql (first tokens) '[)
		(multiple-value-bind (sublist rest)
		    (parse-list (rest tokens))
		  (push sublist result)
		  (setf tokens rest)))
	       (t (error "Invalid token ~S" (first tokens))))))))
    ;; Main logic:
    (multiple-value-bind (prefix matrix-tokens) (scan-right tokens)
      (multiple-value-bind (matrix leftover) (parse (reverse matrix-tokens))
	(when leftover
	  (error "Extra tokens after parsing: ~S" leftover))
	(values prefix (first matrix))))))



(defun tokenize (expr-string)
  "Simple tokenizer"
  (let ((tokens '()))
    (with-input-from-string (s expr-string)
      (loop for token = (read s nil nil)
	    while token
	    do (push token tokens)))
    (reverse tokens)))

(defun evaluate (expr-string &optional (debug nil))
  "Main evaluation function"
  ;;(setf *stack* nil)
  (let ((tokens (tokenize expr-string)))
    ;; Check for assignment
    (if (and (>= (length tokens) 3) (eq (second tokens) '=))
	(handle-assignment (first tokens) (cddr tokens))
	;; Normal evaluation: process tokens right to left
	(progn
	  (loop while tokens do
	    (let ((token (first (reverse tokens))))
	      (when debug
		(format t "Tokens ~A, Token: ~A, Stack: ~A~%" tokens token *stack*))
	      (cond
		(( eq token '])
		 (multiple-value-bind (rest array-literal) (parse-array tokens)
		   (push-stack array-literal)
		   (setf tokens rest)))
		(t
		 (execute-token token)
		 (setf tokens (reverse (cdr (reverse tokens))))))))
	  (peek-stack)))))

(defun handle-assignment (name expr-tokens &optional (debug nil))
  "Handle variable/function assignment"
  (let ((result
	  (handler-case
	      ;; Try to evaluate the expression
	      (progn
		(let ((*stack* nil))
		(dolist (token (reverse expr-tokens))
		  (unless (eq token '[)
		    (execute-token token debug)))
		(peek-stack)))
	    ;; If stack underflow, it's a function definition
	    (error
		(condition)
	      (declare (ignore condition))
	      ;; Store as function - simplified for now
	      (lambda ()
		(dolist
		    (token (reverse expr-tokens))
		  (unless (eq token'[)
		    (execute-token token debug)))
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
	       do (let ((result (evaluate (string-trim " " line))))
		    (when result
		      (push (list line result) results))))
       (reverse results))))

;; Add file operations
(setf *ops* (append *ops* (list (cons 'load #'load-numbers)
				(cons 'run #'run-script))))
