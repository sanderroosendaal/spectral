(require "asdf")

(defpackage :spectral
  (:use :cl)
  (:export :evaluate))

(in-package :spectral)

(defparameter *stack* nil)

;; Constants
(defparameter *constants*
  `((pi . ,(coerce pi 'double-float))
    (e . ,(exp 1.0d0))))

;; Variables and functions storage
(defparameter *variables* (make-hash-table))

(defparameter *functions* (make-hash-table))

(defparameter *ops* (make-hash-table))

(defparameter *stack-ops* (make-hash-table))
(defparameter *error-stream* t)

;; Symbol list
(defun register-op (name function arity)
  "Register a new operation with the given name, function, and arity."
  (setf (gethash name *ops*)
	(cons function arity)))

(defun register-stack-op (name function arity)
  "Registers a new stack operation with the given name, function and arity"
  (setf (gethash name *stack-ops*)
	(cons function arity)))
(load "std/stack.lisp")

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

(defun array-fn (op a)
  (cond
    ((numberp a) (funcall op a))
    ((listp a) (mapcar (lambda (x) (array-fn op x)) a))
    (t (error "Invalid input for array operation: ~S" a))))

(defun strip-token (token char)
  (let* ((name (symbol-name token)))
    (if (and (> (length name) 0)
	     (char= (char name 0) char))
	(intern (subseq name 1))
	token)))

(defun reduce-array (op a)
  (cond
    ((numberp a) (error "Invalid input for reduce: ~S" a))
    ((listp a) (reduce op a))
    (t (error "Invalid input for reduce: ~S" a))))

(defun scan (op initial lst)
  (let ((results nil)
	(acc initial))
    (dolist (item lst (nreverse results))
	   (setf acc (funcall op acc item))
      (push acc results))))

(defun scan1 (op lst)
  (when (null lst)
    (error "scan1 requires a non-empty list"))
  (let ((results (list (first lst)))
	(acc (first lst)))
    (dolist (item (rest lst) (nreverse results))
      (setf acc (funcall op acc item))
      (push acc results))))

(defun scan-array (op a)
  (format t "Entering scan-array with ~A on ~A~%" op a)
  (cond
    ((numberp a) (error "Invalid input for scan: ~A" a))
    ((listp a) (scan1 op a))
    (t (error "Invalid input for scan: ~A" a))))

(load "errors.lisp")

(defun execute-token (token &optional (filename nil) (line-number nil) (debug nil))
  "Execute a single token"
  (when debug
    (format t "Execute-token ~A, stack ~A%" token *stack*))
  (handler-case 
      (cond
	;; Numbers push themselves
	((numberp token)
	 (push-stack token))
	
	;; Arrays push themselves
	((and (listp token) (eq (first token) 'array))
	 (push-stack (rest token)))
	
	;; Strings push themselves
	((stringp token)
	 (push-stack token))
	
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
	
	;; Reduction
	((char= (char (symbol-name token) 0) #\/)
	 (let ((op (strip-token token #\/)))
	   (if (gethash op *ops*)
	       (let* ((a (pop-stack))
		      (op-fn (car (gethash op *ops*)))
		      (value (reduce-array op-fn a)))
		 (push-stack value))
	       (error "Unknown operation: ~A" op))))
	
	;; Scan - using & for now, as \ is escaping
	((char= (char (symbol-name token) 0) #\&)
	 (let ((op (strip-token token #\&)))
	   (if (gethash op *ops*)
	       (let* ((a (pop-stack))
		      (op-fn (car (gethash op *ops*)))
		      (value (scan-array op-fn a)))
		 (push-stack value))
	       (error "Unknown operation: ~A" op))))
	
	;; Stack operations
	((gethash token *stack-ops*)
	 (funcall (car (gethash token *stack-ops*))))
	
	;; Nullary operations
	((= (cdr (gethash token *ops*)) 0)
	 (let* ((op-fn (car (gethash token *ops*)))
		(values (multiple-value-list (funcall op-fn))))
	   (loop for value in values do (push-stack value))))
	
	;; Unary operations
	((= (cdr (gethash token *ops*)) 1)
	 (let* ((a (pop-stack))
		(op-fn (car (gethash token *ops*)))
		(values (multiple-value-list (funcall op-fn a))))
	   (loop for value in values do (push-stack value))))
	
	;; Binary operations
	((= (cdr (gethash token *ops*)) 2)
	 (let* ((a (pop-stack))
		(b (pop-stack))
		(op-fn (car (gethash token *ops*)))
		(values (multiple-value-list (funcall op-fn a b))))
	   (loop for value in values do (push-stack value))))

	(t (error "Unknown token: ~A" token)))
    (error (condition) (handle-error condition (format nil "Error executing token ~A" token) *error-stream* filename line-number))))

(defun parse-array (tokens &optional (filename nil) (line-number nil))
  "Parses the last well-formed bracketed matrix in TOKENS from right to left.
Returns (values tokens-before-matrix matrix).
Can only handle arrays of numerical values.
Signals an error on invalid tokens or unmatched brackets."
  (handler-case
      (labels
	  ((scan-right (tokens)
	     (let ((matrix-tokens '())
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
	    (values prefix (first matrix)))))
    (error (condition) (handle-error condition *error-stream* filename line-number))))

(defparameter *single-character-tokens*
  '(#\[ #\] #\+ #\- #\% #\* #\< #\> #\!))

(defparameter *square-brackets*
  '(#\] #\[))

(defun is-digit-p (c)
  (char<= #\0 c #\9))


(defun add-spaces-around-brackets (s)
    (with-output-to-string (out)
      (loop for char across s
	    do (if (member char *square-brackets*)
		   (format out " ~C " char)
		   (write-char char out)))))

(defun preprocess (s)
  (add-spaces-around-brackets s))

(defun tokenize (expr-string)
  "Simple tokenizer"
  (let ((tokens '())
	(expr-string (preprocess expr-string)))
    (with-input-from-string (s expr-string)
      (loop for token = (read s nil nil)
	    while token
	    do (push token tokens)))
    (reverse tokens)))

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

(defun evaluate (expr-string &optional (filename nil) (line-number) (debug nil))
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
		 (multiple-value-bind (rest array-literal) (parse-array tokens filename line-number)
		   (push-stack array-literal)
		   (setf tokens rest)))
		(t
		 (execute-token token filename line-number)
		 (setf tokens (reverse (cdr (reverse tokens))))))))
	  (peek-stack)))))

(defun spectral-repl ()
  (format t "Spectral REPL - type exit to exit")
  (loop 
    (format t "~&ΣpectraΛ > ")
    (finish-output)
    (let ((line (read-line *standard-input* nil :eof)))
      (cond
	((or (null line) (string= line "exit")) (return))
	(t (evaluate line)
	   (pretty-print-stack))))))

(load "std/arrays.lisp")
(load "std/math.lisp")
(load "std/io.lisp")
(load "std/filters.lisp")

