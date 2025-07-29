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

;; Symbol list
(defun register-op (name function arity)
  "Register a new operation with the given name, function, and arity."
  (setf (gethash name *ops*)
	(cons function arity)))

(defun register-stack-op (name function arity)
  "Registers a new stack operation with the given name, function and arity"
  (setf (gethash name *stack-ops*)
	(cons function arity)))

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

;; could be better, but it's better than nothing
(defun pretty-print-stack-item (item)
  (cond
    ((null item) "")
    ((numberp item) (format t "~A~%" item))
    ((stringp item) (format t "~A~%" item))
    ((and (listp item) (numberp (first item)))
     (format t "[ ~{~A~^ ~} ]~%" item))
    ((listp item)
     (format t "[~%")
     (loop for v in item
	   for i from 0
	   do (progn
		(when (> i 0) (format t ""))
		(pretty-print-stack-item v)))
     (format t "]~%"))
    (t (format t "~A~%" item))))

(defun pretty-print-stack ()
  (loop for item in (reverse *stack*)
	do (pretty-print-stack-item item)))

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

(register-stack-op 'dup #'dup 0)
(register-stack-op 'swap #'swap 0)

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

    (t (error "Unknown token:: ~A" token))))

(defun parse-array (tokens)
  "Parses the last well-formed bracketed matrix in TOKENS from right to left.
Returns (values tokens-before-matrix matrix).
Can only handle arrays of numerical values.
Signals an error on invalid tokens or unmatched brackets."
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
	(values prefix (first matrix))))))

(defun tokenize (expr-string)
  "Simple tokenizer"
  (let ((tokens '()))
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

(defun spectral-repl ()
  (format t "Spectral REPL - type exit to exit")
  (loop 
    (format t "~&> ")
    (finish-output)
    (let ((line (read-line *standard-input* nil :eof)))
      (cond
	((or (null line) (string= line "exit")) (return))
	(t (evaluate line)
	   (pretty-print-stack))))))

(load "arrays.lisp")
(load "math.lisp")
(load "io.lisp")

