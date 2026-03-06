(require "asdf")

;; Load all dependencies before defpackage (named-readtables must be early for SLIME)
(load (merge-pathnames "std/deps.lisp"
                       (make-pathname :defaults (or *load-truename* (truename "spectral.lisp"))
                                     :name nil :type nil)))

(defpackage :spectral
  (:use :cl)
  (:export :evaluate :reset-spectral-state))

(in-package :spectral)

;;; Pipeline: tokenize -> parse -> eval-node. AST nodes use (:type ...).
;;; Stack: ops pop args in reverse order (b then a), so "% 3 15" => 15/3.

;; Resolve paths relative to spectral.lisp (so std/*.lisp load correctly from any CWD)
(defvar *spectral-root*
  (make-pathname :defaults (or *load-truename* (truename "spectral.lisp"))
                :name nil :type nil))

;; Forward declarations for mutually recursive / forward-referenced functions
#+sbcl
(declaim (ftype (function (t) boolean) is-true)
         (ftype (function (list) list) parse parse-group)
         (ftype (function (list) (values list t)) parse-expression))

(defun in-slime-p ()
  (let ((pkg (find-package :swank)))
    (and pkg
   (let ((sym (find-symbol "*EMACS-CONNECTION*" pkg)))
     (and sym (boundp sym) (symbol-value sym))))))

(defun colors-supported-p ()
  (not (string= (or (uiop:getenv "TERM") "") "dumb")))

(defun should-colorize-p ()
  (and (not (in-slime-p)) (colors-supported-p)))

(defparameter *spectral-colors*
  '("ff4500" "ff8c00" "ffd700" "32cd32" "00ced1" "1e90ff" "4b0082" "9400d3"))

(defun spectral-color-text (text)
  (let ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
  (colors *spectral-colors*))
    (loop for char across text
    for i from 0
    do (let ((color (nth (mod i (length colors)) colors)))
         (map nil (lambda (c) (vector-push-extend c result))
        (cl-ansi-text:make-color-string color))
         (vector-push-extend char result)))
    (coerce result 'string)))

(defparameter *stack* nil)

;; Constants
(defparameter *constants*
  `((pi . ,(coerce pi 'double-float))
    (e . ,(exp 1.0d0))
    (epsilon . ,double-float-epsilon)))

;; Variables and functions storage
(defparameter *variables* (make-hash-table))

(defparameter *functions* (make-hash-table))

;; Operation registries:
;; *ops*       - STD functions (+, -, *, %, sin, ...) via register-op
;; *stack-ops* - dup, swap, pop, peek
;; *reduce-ops* - /+, /*, /max, /min (parser treats token starting with /)
;; *scan-ops*   - &+, &* (parser treats token starting with &)
(defparameter *ops* (make-hash-table))
(defparameter *stack-ops* (make-hash-table))
(defparameter *reduce-ops* (make-hash-table))
(defparameter *scan-ops* (make-hash-table))
(defparameter *error-stream* t)

;; Magic numbers (used by std libs)
(defparameter *print-limit-per-dim* 10 "Max elements shown per dimension when pretty-printing arrays.")
(defparameter *peek-stack-limit* 5 "Max stack items shown by peek.")
(defparameter *max-ndims* 8 "Max array dimensions supported by .sdat format.")
(defparameter *npy-alignment* 16 "NPY header alignment (bytes).")

(defun spectral-error (format-control &rest format-args)
  "Signal a user-facing Spectral error with consistent formatting."
  (error (make-condition 'simple-error
                        :format-control format-control
                        :format-arguments format-args)))

;; Script context for error reporting (bound by run-script)
(defparameter *script-filename* nil)
(defparameter *script-line* nil)

(defun reset-spectral-state ()
  "Clear stack, variables, and functions. Used for fresh test runs."
  (setf *stack* nil)
  (clrhash *variables*)
  (clrhash *functions*))

;; Symbol list
(defun register-op (name function arity)
  "Register a new operation with the given name, function, and arity."
  (setf (gethash name *ops*)
  (cons function arity)))

(defun register-stack-op (name function arity)
  "Registers a new stack operation with the given name, function and arity"
  (setf (gethash name *stack-ops*)
  (cons function arity)))

;; Array operations (defined before std libs that use them)
(defun array-op (op a b)
  "Apply binary operation element-wise on n-dimensional arrays.
   List inputs are coerced to vectors."
  (declare (optimize (speed 3) (safety 1)))
  (when (listp a) (setf a (coerce a 'vector)))
  (when (listp b) (setf b (coerce b 'vector)))
  (cond
    ((and (numberp a) (numberp b)) (funcall op a b))
    ((and (numberp a) (arrayp b))
     (let* ((dimensions (array-dimensions b))
      (result-array (make-array dimensions))
      (total (array-total-size b)))
       (dotimes (i total)
   (setf (row-major-aref result-array i)
         (funcall op a (row-major-aref b i))))
       result-array))
    ((and (numberp b) (arrayp a))
     (let* ((dimensions (array-dimensions a))
      (result-array (make-array dimensions))
      (total (array-total-size a)))
       (dotimes (i total)
   (setf (row-major-aref result-array i)
         (funcall op (row-major-aref a i) b)))
       result-array))
    ((and (arrayp a) (arrayp b))
     (let* ((dimensions (array-dimensions a))
      (result-array (make-array dimensions))
      (total (array-total-size a)))
       (unless (equal dimensions (array-dimensions b))
   (spectral-error "Mismatched array dimensions: arrays must have the same dimensions"))
       (dotimes (i total)
   (setf (row-major-aref result-array i)
         (funcall op (row-major-aref a i) (row-major-aref b i))))
       result-array))))


(defun array-fn (op a)
  "Apply a unary operation element-wise on an n-dimensional array.
   List inputs are coerced to vectors."
  (declare (optimize (speed 3) (safety 1)))
  (when (listp a) (setf a (coerce a 'vector)))
  (cond
    ((numberp a) (funcall op a))
    ((arrayp a)
     (let* ((dimensions (array-dimensions a))
      (result-array (make-array dimensions))
      (total (array-total-size a)))
       (dotimes (i total)
   (setf (row-major-aref result-array i)
         (funcall op (row-major-aref a i))))
       result-array))
    (t (spectral-error "Invalid input for array operation: ~S" a))))

(load (merge-pathnames "std/stack.lisp" *spectral-root*))
(load (merge-pathnames "std/arrays.lisp" *spectral-root*))
(load (merge-pathnames "std/math.lisp" *spectral-root*))
(load (merge-pathnames "std/io.lisp" *spectral-root*))
(load (merge-pathnames "std/filters.lisp" *spectral-root*))
(load (merge-pathnames "std/signal_processing.lisp" *spectral-root*))
(load (merge-pathnames "std/plotting.lisp" *spectral-root*))

;; Reduction and scan operators
(setf (gethash '/+ *reduce-ops*) #'+)
(setf (gethash '/* *reduce-ops*) #'*)
(setf (gethash '/max *reduce-ops*) #'max)
(setf (gethash '/min *reduce-ops*) #'min)
(setf (gethash '&+ *scan-ops*) #'+)
(setf (gethash '&* *scan-ops*) #'*)

(defparameter *parallel-reduce-threshold* 10000
  "Use parallel reduction when array has more than this many elements.")

;; 1D: fold left. 2D: reduce along first axis, return (dims-1) shape.
;; Uses lparallel for large arrays (>= *parallel-reduce-threshold* elements).
(defun reduce-array (op a)
  (cond
    ((numberp a)
     (spectral-error "Reduction (e.g. /+) expects an array, got ~S. Use /+ [1 2 3] to sum values." a))
    ((and (arrayp a) (> (length (array-dimensions a)) 1))
     (let* ((dims (array-dimensions a))
            (rest-dims (subseq dims 1))
            (col-size (reduce #'* rest-dims))
            (ncols (array-total-size (make-array rest-dims)))
            (result (make-array rest-dims))
            (use-parallel (>= (* (first dims) ncols) *parallel-reduce-threshold*)))
       (if use-parallel
           (lparallel:pdotimes (i ncols result)
             (let ((v (loop with acc = (row-major-aref a i)
                           for k from 1 below (first dims)
                           do (setf acc (funcall op acc (row-major-aref a (+ i (* k col-size)))))
                           finally (return acc))))
               (setf (row-major-aref result i) v)))
           (dotimes (i ncols)
             (let ((v (loop with acc = (row-major-aref a i)
                           for k from 1 below (first dims)
                           do (setf acc (funcall op acc (row-major-aref a (+ i (* k col-size)))))
                           finally (return acc))))
               (setf (row-major-aref result i) v))))
       result))
    ((arrayp a)
     (let* ((ntot (array-total-size a))
            (use-parallel (>= ntot *parallel-reduce-threshold*)))
       (if use-parallel
           ;; Parallel: preduce needs initial-value for + and *; max/min use first of each chunk.
           (cond ((eq op #'+) (lparallel:preduce op a :initial-value 0))
                 ((eq op #'*) (lparallel:preduce op a :initial-value 1))
                 (t (lparallel:preduce op a)))
           (let ((result (row-major-aref a 0)))
             (loop for i from 1 to (1- ntot) do
                   (setf result (funcall op result (row-major-aref a i))))
             result))))
    (t (spectral-error "Reduction expects an array, got ~S." a))))

(defun scan (op initial lst)
  (let ((results nil)
  (acc initial))
    (dolist (item lst (nreverse results))
     (setf acc (funcall op acc item))
      (push acc results))))

;; Inclusive prefix scan: first element = itself, no initial value.
(defun scan1 (op lst)
  (when (null lst)
    (spectral-error "scan1 requires a non-empty list"))
  (let ((results (list (first lst)))
  (acc (first lst)))
    (dolist (item (rest lst) (nreverse results))
      (setf acc (funcall op acc item))
      (push acc results))))

(defun scan-array (op a)
  (cond
    ((numberp a)
     (spectral-error "Scan (e.g. &+) expects an array, got ~S. Use &+ [1 2 3] for cumulative sum." a))
    ((listp a) (scan1 op a))
    ((arrayp a)
     (let ((lst (coerce a 'list)))
       (coerce (scan1 op lst) 'vector)))
    (t (spectral-error "Scan expects an array, got ~S." a))))

(defun check-rectangular (elements)
  (when (every #'listp elements)
    ;; Only check if all elements are lists (i.e., nested arrays)
    (let ((first-len (length (first elements))))
      (unless (every (lambda (row) (= (length row) first-len)) elements)
        (spectral-error "Ragged array: all rows must have the same length"))
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

;; element = (car element) = :type, (cdr element) = val.
;; For :reduce/:scan, val = (op-symbol operand-ast). Eval operand first, then apply.
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
       (eval-node (first (car val)))
       (eval-node (second (car val))))))

      ;; Groups
      ((equal typ :group)
       (loop for item in (reverse (car val)) do (eval-node item)))
      
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
   (push-stack
          (make-array (if (listp dims) dims (list dims))
      :initial-contents evaluated-elements))))

      ;; User defined functions
      ((equal typ :function)
       (funcall (gethash (car val) *functions*)))

      ;; Reduction
      ((equal typ :reduce)
       (eval-node (second val) debug)
       (let* ((operand (pop-stack))
        (op-sym (first val))
        (op-fn (gethash op-sym *reduce-ops*)))
   (when (null op-fn)
     (spectral-error "Unknown reduction operator: ~A" op-sym))
   (push-stack (reduce-array op-fn operand))))

      ;; Scan
      ((equal typ :scan)
       (eval-node (second val) debug)
       (let* ((operand (pop-stack))
        (op-sym (first val))
        (op-fn (gethash op-sym *scan-ops*)))
   (when (null op-fn)
     (spectral-error "Unknown scan operator: ~A" op-sym))
   (push-stack (scan-array op-fn operand))))

      ;; Stack operation
      ((equal typ :stack)
       (funcall (car val)))

      ;; STD function operations
      ;; Stack order: pop a then b means b was pushed last (rightmost in source).
      ;; Op-fn receives (b a) to match left-to-right reading: "+ 3 5" => 3+5.
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
        (loop for value in values do (push-stack value))
        (first *stack*)))
    
     ;; Binary operations
     ((= arity 2)
      (let* ((a (pop-stack))
       (b (pop-stack))
       (values (multiple-value-list (funcall op-fn a b))))
        (loop for value in values do
    (push-stack value))
        (first *stack*)))

     ;; Ternary operations (e.g. write-hdf5 filename path data)
     ((= arity 3)
      (let* ((a (pop-stack))
       (b (pop-stack))
       (c (pop-stack))
       (values (multiple-value-list (funcall op-fn a b c))))
        (loop for value in values do (push-stack value))
        (first *stack*)))

     (t (spectral-error "Functions of arity ~A are not implemented" arity)))))

      ;; unrecognized
      (t (spectral-error "Unexpected expressions: ~A" element)))))

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
  "Execute a sequence of expressions on a string, threading the
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

;; Preprocess strips comments, expands |, adds spaces. Read produces symbols/numbers.
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

(defun parse-user-function-call (tokens)
  (let ((rest (cdr tokens)))
    (values
     rest
     `(:function ,(car tokens)))))

 
(defun parse-array (tokens)
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
      (spectral-error "Could not find correct then-else clause for IF"))
    (let ((groups (parse-group then-else)))
      (values
       rest
       `(:if-then-else ,groups)))))

(defun parse-group (group)
  (let ((result '()))
    (loop until (null group) do
      (multiple-value-bind (rest tok) (parse-expression group)
  (setf group rest)
  (push tok result)))
    (reverse result)))

;; Returns (values rest-tokens ast). Consumes one expression from tokens.
;; Reduction/scan: consume token (e.g. /+), parse operand from (cdr tokens).
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
    ,(parse-group token))))

      ;; User defined Functions
      ((gethash token *functions*)
       (parse-user-function-call tokens))

      ;; Array
      ((equal token '[)
       (parse-array tokens))

      ;; Reduction
      ((char= (char (symbol-name token) 0) #\/)
       (multiple-value-bind (rest operand-ast) (parse-expression (cdr tokens))
   (values rest `(:reduce ,token ,operand-ast))))

      ;; Scan
      ((char= (char (symbol-name token) 0) #\&)
       (multiple-value-bind (rest operand-ast) (parse-expression (cdr tokens))
   (values rest `(:scan ,token ,operand-ast))))

      ;; Stack operations
      ((gethash token *stack-ops*)
       (parse-stack-ops tokens))

      ;; STD function operations
      ((gethash token *ops*)
       (parse-ops tokens))
      
      ;; unrecognized
      (t (spectral-error "Unexpected token: ~A" token)))))

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

(defun evaluate (expr-string &optional (debug nil))
  "Parse and evaluate a Spectral expression. Returns the top-of-stack value."
  (let ((tokens (tokenize expr-string)))
    (let ((ast (parse tokens)))
      (loop for node in (reverse ast) do
  (eval-node node debug))
      (peek-stack))))


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
  (t (handler-case
         (progn (evaluate line)
          (pretty-print-stack))
       (error (e)
         (format *error-stream* "~&Error: ~A~%" e)
         (finish-output *error-stream*))))))))
