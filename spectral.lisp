(require "asdf")

(defpackage :spectral
  (:use :cl)
  (:export :evaluate))

(ql:quickload :cl-ansi-text)

(in-package :spectral)

(defun in-slime-p ()
  (and (find-package :swank)
       (symbol-value (find-symbol "*EMACS-CONNECTION*" :swank))))

(defun colors-supported-p ()
  (not (string= (uiop:getenv "TERM") "dumb")))

(defun should-colorize-p ()
  (and (not (in-slime-p)) (colors-supported-p)))

(defparameter *spectral-colors*
  '("ff4500" "ff8c00" "ffd700" "32cd32" "00ced1" "1e90ff" "4b0082" "9400d3"))

(defun spectral-color-text (text)
  (let ((result "")
	(colors *spectral-colors*))
    (loop for char across text
	  for i from 0
	  do (let ((color (nth (mod i (length colors)) colors)))
	       (setf result
		     (concatenate 'string result
				  (cl-ansi-text:make-color-string color)
				  (string char)))))
    result))

(defparameter *stack* nil)

;; Constants
(defparameter *constants*
  `((pi . ,(coerce pi 'double-float))
    (e . ,(exp 1.0d0))
    (epsilon . ,double-float-epsilon)))

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
(defun array-op-list (op a b)
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

(defun array-fn-list (op a)
  (cond
    ((numberp a) (funcall op a))
    ((listp a) (mapcar (lambda (x) (array-fn op x)) a))
    (t (error "Invalid input for array operation: ~S" a))))

(defun array-op (op a b)
  "Apply binary operation element-wise on n-dimensional arrays."
  (cond
    ((and (numberp a) (numberp b)) (funcall op a b))
    ((and (numberp a) (arrayp b))
     (let* ((dimensions (array-dimensions b))
	    (result-array (make-array dimensions)))
       (dotimes (i (array-total-size b))
	 (setf (row-major-aref result-array i)
	       (funcall op a (row-major-aref b i))))
       result-array))
    ((and (numberp b) (arrayp a))
     (let* ((dimensions (array-dimensions a))
	    (result-array (make-array dimensions)))
       (dotimes (i (array-total-size a))
	 (setf (row-major-aref result-array i)
	       (funcall op (row-major-aref a i) b)))
       result-array))
    ((and (arrayp a) (arrayp b))
     (let* ((dimensions (array-dimensions a))
	    (result-array (make-array dimensions)))
       (unless (equal dimensions (array-dimensions b))
	 (error "Mismatched array dimensions: arrays must have the same dimensions"))
       (dotimes (i (array-total-size a))
	 (setf (row-major-aref result-array i)
	       (funcall op (row-major-aref a i) (row-major-aref b i))))
       result-array))))
    

(defun array-fn (op a)
  "Apply a unary operation element-wise on an n-dimensional array."
  (cond
    ((numberp a) (funcall op a))
    ((arrayp a)
     (let* ((dimensions (array-dimensions a))
	    (result-array (make-array dimensions)))
       (dotimes (i (array-total-size a))
	 (setf (row-major-aref result-array i)
	       (funcall op (row-major-aref a i))))
       result-array))
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
    ((and (arrayp a) (> (length (array-dimensions a)) 1))
     (let* ((dims (array-dimensions a))
	    (rest-dims (subseq dims 1))
	    (result (make-array rest-dims)))
       (dotimes (i (array-total-size result))
	 (let* ((idx (array-row-major-index-to-subscript rest-dims i))
		(values (loop for i below (first dims)
			      collect (apply #'aref a (cons i idx))))
		(v (reduce op values)))
	   (setf (row-major-aref result i) v)))
       result))
    ((arrayp a) 
     (let* ((result (row-major-aref a 0))
	    (ntot (array-total-size a)))
       (loop for i from 1 to (1- ntot) do
	 (setf result (funcall op result (row-major-aref a i))))
       result))
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


(defun check-rectangular (elements)
  (when (every #'listp elements)
    ;; Only check if all elements are lists (i.e., nested arrays)
    (let ((first-len (length (first elements))))
      (unless (every (lambda (row) (= (length row) first-len)) elements)
        (error "Ragged array: all rows must have the same length"))
      (when (and (plusp first-len) (listp (caar elements)))
        (dolist (row elements)
          (check-rectangular row))))))

(defun compute-dimensions (elements)
  (if (every #'listp elements)
      (list (length elements)
            (if (plusp (length elements))
                (length (first elements))
                0))
      (length elements)))

(defun eval-node (element &optional (debug nil))
  "Execute AST element"
  (let ((typ (first element))
	(val (cdr element)))
    (if debug
	(format t "Entering eval-node with typ ~A, val ~A~%" typ val))
    (cond
      ;; Numbers push themselves
      ((equal typ :number)
       (push-stack (car val)))

      ;; Strings push themselves
      ((equal typ :string)
       (push-stack (car val)))

      ;; Constants
      ((equal typ :constant)
       (push-stack (cdr (assoc (car val) *constants*))))

      ;;Variables
      ((equal typ :variable)
       (push-stack (gethash (car val) *variables*)))

      ;; Assignment
      ((equal typ :assignment)
       (let* ((name (first val))
	      (exprs (car (rest val)))
	      (result
		(handler-case
		    ;; try to evaluate the expression
		    (progn
		      (let ((*stack* nil))
			(dolist (element (reverse exprs))
			  (eval-node element))
			(peek-stack)))
		  (error
		      (condition)
		    (declare (ignore condition))
		    (lambda ()
		      (dolist
			  (element (reverse exprs))
			(eval-node element))
		      (peek-stack))))))
	 (if (functionp result)
	     (setf (gethash name *functions*) result)
	     (setf (gethash name *variables*) result))))

      ;; Conditional
      ((equal typ :if-then-else)
       (let* ((condition (pop-stack)))
	 (if (is-true condition)
	     (eval-node (first val))
	     (eval-node (second val)))))

      ;; Groups
      ((equal typ :group)
       (loop for item in (reverse val) do (eval-node item)))
      
      ;; Arrays
      ((equal typ :array)
       (let* ((evaluated-elements (mapcar (lambda (part)
					    (eval-node part nil)
					    (let ((val (pop-stack)))
					      (if (arrayp val)
						  (coerce val 'list)
						  val))) val))
	      (dims (compute-dimensions evaluated-elements)))
	 (when (every #'listp evaluated-elements)
	   (check-rectangular evaluated-elements))
	 (format t "Dims ~A elements ~A~%" dims evaluated-elements)
	 (push-stack
          (make-array (if (listp dims) dims (list dims))
			:initial-contents evaluated-elements))))

      ;; User defined functions
      ((equal typ :function)
       (push-stack (funcall (gethash (car val) *functions*))))

      ;; Reduction

      ;; Scan

      ;; Stack operation
      ((equal typ :stack)
       (funcall (car (gethash val *stack-ops*))))

      ;; STD function operations
      ((equal typ :op)
       (let ((op-fn (car val))
	     (arity (cadr val)))
	 (cond
	   ;; Nullary operations
	   ((= arity 0)
	    (let ((values (multiple-value-list (funcall op-fn))))
	      (loop for value in values do (push-stack value))
	      (first *stack*)))
	   

           ;; Unary operations
	   ((= arity 1)
	    (let* ((a (pop-stack))
		   (values (multiple-value-list (funcall op-fn a))))
	      (loop for value in values do (push-stack value)))
	    (first *stack*))
    
	   ;; Binary operations
	   ((= arity 2)
	    (let* ((a (pop-stack))
		   (b (pop-stack))
		   (values (multiple-value-list (funcall op-fn a b))))
	      (loop for value in values do (push-stack value))
	      (first *stack*)))

	   (t (error "Functions of arity ~A are not implemented" arity)))))

      ;; unrecognized
      (t (error "Unexpected expressions: ~A" element)))))

(defun execute-token (token &optional (debug nil))
  "Execute a single token"
  (when debug
    (format t "Execute-token ~A, stack ~A~%" token *stack*)
    (format t "Number? ~A~%" (numberp token)))
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
       (loop for value in values do (push-stack value))
       (first *stack*)))
    
    ;; Unary operations
    ((= (cdr (gethash token *ops*)) 1)
     (let* ((a (pop-stack))
	    (op-fn (car (gethash token *ops*)))
	    (values (multiple-value-list (funcall op-fn a))))
       (loop for value in values do (push-stack value))
       (first *stack*)))
    
    ;; Binary operations
    ((= (cdr (gethash token *ops*)) 2)
     (let* ((a (pop-stack))
	    (b (pop-stack))
	    (op-fn (car (gethash token *ops*)))
	    (values (multiple-value-list (funcall op-fn a b))))
       (loop for value in values do (push-stack value))
       (first *stack*)))
    
    (t (error "Unknown token: ~A" token))))

;; Need to make array processing smarter. It should handle for example
;; [0 1 pi]  --> [0 1 3.14xxx]
;; [range 9] --> [[0 1 2 3 4 5 6 7 8]] etc
;; temp before we move to arrays in parse-array
(defun list-to-n-dimensional-array (nested-lists)
  "Convert a nested list into a fully rectangular N-dimensionl array."
  (labels ((shape (lst)
	     (if (listp lst)
		 (cons (length lst) (shape (first lst)))
		 nil))
	   (flatten (lst)
	     (if (listp lst)
		 (mapcan #'flatten lst)
		 (list lst))))
    (let* ((dims (shape nested-lists))
	   (flat (flatten nested-lists))
	   (array (make-array dims)))
      (loop for idx from 0 below (length flat)
	    do (setf (row-major-aref array idx) (nth idx flat)))
      array)))

(defun parse-group (tokens &optional (filename nil) (line-number nil))
  "Parses the last well-formed parenthesized group in TOKENS from right to left.
Returns (values tokens-before-group group).
Group is a (potentially nested) list of token groups.
Signals an error on invalied tokens or unmatched parentheses."
  (handler-case
      (let* ((rest (reverse (rest (reverse tokens))))
	     (group (first (reverse tokens)))
	     (first-op (first (reverse group)))
	     (rest-group (reverse (rest (reverse group)))))
	(concatenate 'list rest (list rest-group first-op)))	
    (error (condition) (handle-error condition *error-stream* filename line-number))))

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
	    (values prefix (list-to-n-dimensional-array (first matrix))))))
    (error (condition) (handle-error condition *error-stream* filename line-number))))

(defparameter *single-character-tokens*
  '(#\+ #\% #\* #\< #\> #\!))

(defparameter *square-brackets*
  '(#\] #\[))

(defun is-digit-p (c)
  (char<= #\0 c #\9))

(defun remove-comments (string)
  (let ((in-quote nil)
	(result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (dotimes (i (length string))
      (let ((char (char string i)))
	(cond
	  ((char= char #\")
	   (setf in-quote (not in-quote))
	   (vector-push-extend char result))
	  ((and (not in-quote) (char= char #\;))
	   (return-from remove-comments (coerce result 'string)))
	  (t
	   (vector-push-extend char result)))))
    (coerce result 'string)))


(defun add-spaces-around-brackets (s)
    (with-output-to-string (out)
      (loop for char across s
	    do (if (member char *square-brackets*)
		   (format out " ~C " char)
		   (write-char char out)))))

(defun add-spaces-after-single-char-tokens (s)
  (let ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (dotimes (i (length s))
      (vector-push-extend (char s i) result)
      (when (and (< i (1- (length s)))
		 (find (char s i) *single-character-tokens*)
		 (or
		  (digit-char-p (char s (1+ i)))
		  (find (char s (1+ i)) *single-character-tokens*)))
	(vector-push-extend #\Space result)))
    (coerce result 'string)))


(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)

(defun parse-pipes (s)
  (with-output-to-string (out)
    (labels ((process (start end)
	       ;; Transform "a|b|c" to "((a)(b)(c))"
	       (let ((parts (split-sequence:split-sequence #\| (subseq s start end))))
		 (format out "(~{(~a)~})" parts)))
	     (walk (i)
	       (loop while (< i (length s)) do
		 (let ((ch (char s i)))
		   (cond
		     ((char= ch #\()
		      (let ((start i)
			    (depth 1)
			    (j (1+ i)))
			;; Search for matching closing paren
			(loop while (and (< j (length s)) (> depth 0)) do
			  (let ((c (char s j)))
			    (cond
			      ((char= c #\() (incf depth))
			      ((char= c #\)) (decf depth))))
			  (incf j))
			(if (and (= depth 0)
				 (find #\| s :start (1+ start) :end (1- j)))
			    ;; Parenthesized expression with | found
			    (progn
			      (process (1+ start) (1- j))
			      (setf i j))
			    ;; Not an alternation or unbalanced
			    (progn
			      (write-string (subseq s start j) out)
			      (setf i j)))))
		     (t (write-char ch out)
			(incf i)))))))
	     (walk 0))))


(defmacro preprocess-s (string &body expressions)
  "Execute a equence of expressions on a string, threading the
result through each expression using 's' as the placeholder for the current value."
  (let ((result-var (gensym "RESULT")))
    `(let ((,result-var ,string))
       ,@(mapcar (lambda (expr)
		   `(setf ,result-var ,(substitute result-var 's expr)))
		 expressions)
       ,result-var)))

(defun preprocess (s)
  (preprocess-s s
    (remove-comments s)
    (parse-pipes s)
    (add-spaces-around-brackets s)
    (add-spaces-after-single-char-tokens s)))

(defun tokenize (expr-string)
  "Simple tokenizer"
  (let ((tokens '())
	(expr-string (preprocess expr-string)))
    (with-input-from-string (s expr-string)
      (loop for token = (read s nil nil)
	    while token
	    do (push token tokens)))
    (reverse tokens)))


(defun is-true (value)
  (cond
    ((numberp value)
     (/= value 0))
    ((arrayp value)
     (> (length value) 0))
    ((listp value)
     (> (length value) 0))
    (t t)))

(defun handle-if (tokens &optional (filename nil) (line-number nil))
  (handler-case
      (let ((then-else (second (reverse tokens))))
	(unless (and (listp then-else)
		     (>= (length then-else) 2))
	  (error "Could not find correct then-else clause for IF"))
	(let* ((condition (pop-stack))
	       (next-token (if (is-true condition) (first then-else) (second then-else)))
	       (rest (subseq tokens 0 (- (length tokens) 2))))
	  (concatenate 'list rest (list next-token))))
    (error (condition) (handle-error condition *error-stream* filename line-number))))

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

(defun parse-user-function-call (tokens)
  (let ((rest (cdr tokens)))
    (values
     rest
     `(:function ,(car tokens)))))

 
(defun parse-array-2 (tokens)
  (let ((rest (cdr tokens))
	(elements '()))
    (loop until (equal (first tokens) '])
	  do (multiple-value-bind (rest2 tok) (parse-expression rest)
	       (setf rest rest2)
	       (setf tokens rest2)
	       (push tok elements)))
    (values
     (cdr rest)
     `(:array ,@(nreverse elements)))))

(defun parse-stack-ops (tokens)
  (let ((fun (gethash (car tokens) *stack-ops*))
	(rest (cdr tokens)))
    (values
     rest
     `(:stack ,(car fun) ,(cdr fun)))))


(defun parse-ops (tokens)
  (let ((fun (gethash (car tokens) *ops*))
	(rest (cdr tokens)))
    (values
     rest
     `(:op ,(car fun) ,(cdr fun)))))

(defun parse-assignment (tokens)
  (let* ((name (first tokens))
	 (expr-tokens (cddr tokens))
	 (ast (parse expr-tokens)))
    (values
     nil
     `(:assignment ,name ,ast))))

(defun parse-if (tokens)
  (let ((then-else (first tokens))
	(rest (cddr tokens)))
    (unless (and (listp then-else)
		 (>= (length then-else) 2))
      (error "Could not find correct then-else clause for IF"))
    (let ((groups (parse-group-2 then-else)))
      (format t "Groups ~A~%" groups)
      (values
       rest
       `(:if-then-else ,groups)))))

(defun parse-group-2 (group)
  (let ((result '()))
    (loop until (null group) do
      (multiple-value-bind (rest tok) (parse-expression group)
	(setf group rest)
	(push tok result)))
    (reverse result)))

(defun parse-expression (tokens)
  (let ((token (first tokens))
	(stoken (second tokens)))
    (cond
      ;; If - conditional
      ((eq stoken 'if)
       (parse-if tokens))

      ;; Assignment
      ((eq stoken '=)
       (parse-assignment tokens))
      
      ;; Numbers
      ((numberp token)
       (values 
	(cdr tokens)
	`(:number ,token)))

      ;; Strings
      ((stringp token)
       (values
	(cdr tokens)       
       `(:string ,token)))

      ;; Constants
      ((assoc token *constants*)
       (values
	(cdr tokens)
       `(:constant ,token)))

      ;; Variables
      ((gethash token *variables*)
       (values
       (cdr tokens)
       `(:variable ,token)))

      ;; Groups
      ((listp token)
       (values
	(cdr tokens)
	`(:group
	  ,(parse-group-2 token))))

      ;; User defined Functions
      ((gethash token *functions*)
       (parse-user-function-call tokens))

      ;; Array
      ((equal token '[)
       (parse-array-2 tokens))

      ;; Reduction
      ((char= (char (symbol-name token) 0) #\/)
       (values 
	(cdr tokens)
       `(:reduce ,(parse-expression token))))

      ;; Scan
      ((char= (char (symbol-name token) 0) #\&)
       (values
	(cdr tokens)
	`(:scan ,(parse-expression token))))

      ;; Stack operations
      ((gethash token *stack-ops*)
       (parse-stack-ops tokens))

      ;; STD function operations
      ((gethash token *ops*)
       (parse-ops tokens))
      
      ;; unrecognized
      (t (error "Unexpected token: ~A" token)))))

(defun parse (tokens)
  (let ((result '()))
    (loop until (null tokens)
	  do
	     (multiple-value-bind
		   (rtokens expr)
		 (parse-expression tokens)
	       (setf tokens rtokens)
	       (push expr result)))
    (nreverse result)))

(defun evaluate-2 (expr-string &optional (debug nil))
  (let ((tokens (tokenize expr-string)))
    (let ((ast (parse tokens)))
      (loop for node in (reverse ast) do
	(eval-node node debug))
      (peek-stack))))

(defun evaluate (expr-string &optional (filename nil) (line-number nil) (debug nil))
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
		((null token)
		 (setf tokens (reverse (rest (reverse tokens)))))
		((eq token 'if)
		 (setf tokens
		       (handle-if tokens filename line-number)))
		((listp token) ;; group, currently discarded
		 (setf tokens (parse-group tokens)))
		(( eq token '])
		 (multiple-value-bind (rest array-literal) (parse-array tokens filename line-number)
		   (push-stack array-literal)
		   (setf tokens rest)))
	      (t
		(execute-token token debug)
		(setf tokens (reverse (cdr (reverse tokens))))))))
	  (peek-stack)))))


(defun spectral-repl ()
  (if (should-colorize-p)
      (format t (cl-ansi-text:green (format nil "Spectral REPL - type exit to exit")))
      (format t "Spectral REPL - type exit to exit"))
  (loop
    (if (should-colorize-p)
	(format t "~&~A ~A~A" (spectral-color-text (format nil "ΣpectraΛ")) (format nil "~C[0m" #\Escape) " > ")
	(format t "~&ΣpectraΛ > "))
    (finish-output)
    (let ((line (read-line *standard-input* nil :eof)))
      (cond
	((or (null line) (string= line "exit")) (return))
	(t (evaluate-2 line)
	   (pretty-print-stack))))))

(load "std/arrays.lisp")
(load "std/math.lisp")
(load "std/io.lisp")
(load "std/filters.lisp")
(load "std/signal_processing.lisp")
(load "std/plotting.lisp")
