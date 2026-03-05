#!/usr/bin/env sbcl -–script
(load "~/.sbclrc")

;; Robust Lisp documentation extractor
;; Usage: sbcl --script doc-extractor.lisp spectral.lisp std/*.lisp > documentation.md
;; On Windows PowerShell, use: ... 2>$null | Out-File -FilePath documentation.md -Encoding utf8
(let ((*standard-output* *error-output*))
  (ql:quickload :cl-ansi-text)   ; spectral.lisp references this
  (ql:quickload :cl-ppcre)
  (ql:quickload :split-sequence)
  (ql:quickload :cl-csv)
  (ql:quickload :array-operations)
  (handler-case (ql:quickload :magicl) (error () nil))
  (ql:quickload :cffi)
  (handler-case (load "std/fftw-ffi.lisp") (error () nil)))

(defvar *functions* (make-hash-table :test 'equal))
(defvar *registered-ops* '())

;; Structure to hold function information
(defstruct func-info
  name
  docstring
  parameters
  file)

;; Structure to hold registered operation information 
(defstruct op-info
  name
  function
  arity
  file)

(defun extract-docstring (form)
  "Extract docstring from a defun form if present."
  (when (and (listp form)
       (eq (first form) 'defun)
       (>= (length form) 4))
    (let ((body (nthcdr 3 form)))
      (when (and (stringp (first body))
     (rest body)) ; Make sure it’s not just a string return value
  (first body)))))

(defun extract-parameters (form)
  "Extract parameter list from a defun form."
  (when (and (listp form)
       (eq (first form) 'defun)
       (>= (length form) 3))
    (third form)))

(defun process-defun (form filename)
  "Process a defun form and store its information."
  (when (and (listp form) (eq (first form) 'defun))
    (let* ((name (second form))
     (params (extract-parameters form))
     (docstring (extract-docstring form))
     (name-str (string-downcase (symbol-name name))))
      (setf (gethash name-str *functions*)
      (make-func-info :name name-str
          :docstring docstring
          :parameters params
          :file filename)))))

(defun strip-hash-quote (s)
  (subseq s 2))

(defun process-register-op (form filename)
  "Process a register-op form and store its information."
  (when (and (listp form)
       (or (eq (first form) 'register-op) (eq (first form) 'register-stack-op))
       (= (length form) 4))
    (let ((name (second form))
    (function (third form))
    (arity (fourth form)))
      ;; Handle quoted symbols and (function name)
      (when (and (listp name) (eq (first name) 'quote))
  (setf name (second name)))
      (when (listp function)
  (cond ((eq (first function) 'quote)
         (setf function (second function)))
        ((and (eq (first function) 'function) (= (length function) 2))
         (setf function (second function)))))
      ;; Convert to strings and push
      (let ((name-str (cond ((stringp name) name)
          ((symbolp name) (string-downcase (symbol-name name)))
          (t (format nil "~A" name))))
      (function-str (if (symbolp function)
            (string-downcase (symbol-name function))
            (string-downcase (format nil "~A" function))))
      (arity-val (if (numberp arity) arity
         (if (and (listp arity) (eq (first arity) 'quote))
             (second arity) arity))))
  (push (make-op-info :name name-str :function function-str
          :arity arity-val :file filename)
        *registered-ops*)))))

(defun proper-list-p (x)
  (loop for tail = x then (cdr tail)
        until (null tail)
        when (atom tail) return nil
        finally (return t)))

(defun walk-form (form filename)
  "Walk through a form and process defuns and register-op calls."
  (handler-case
      (cond
  ((null form) nil)
  ((atom form) nil)
  ((eq (first form) 'defun)
   (process-defun form filename))
  ((or (eq (first form) 'register-op) (eq (first form) 'register-stack-op))
   (process-register-op form filename))
  ;; Skip recursing into quasiquote - contains comma structs that break dolist
  ((and (consp form) (eq (first form) 'sb-int:quasiquote)) nil)
  ((and (consp form) (eq (first form) 'quote)) nil)
  (t
   (when (proper-list-p form)
     (dolist (subform form)
       (when (proper-list-p subform)
         (walk-form subform filename))))))
    (type-error () nil)))  ; skip forms that cause type errors (e.g. comma in quasiquote)

(defun read-all-forms (filename)
  "Read all forms from a file safely."
  (handler-case
      (with-open-file (stream filename :direction :input)
  (let ((forms '())
        (*read-eval* nil)) ; Security: disable #. reader macro
    (loop
      (let ((form (handler-case
          (read stream nil :eof)
        (error (c)
          (format *error-output* "Warning: Error reading form in ~A: ~A~%"
            filename c)
          nil))))
        (if (eq form :eof)
      (return forms)
      (if form
          (push form forms)))))))
    (error (c)
      (format *error-output* "Error reading file ~A: ~A~%" filename c)
      nil)))

(defun process-file (filename)
  "Process a single Lisp file."
  (format *error-output* "Processing ~A…~%" filename)
  (let ((forms (read-all-forms filename)))
    (dolist (form forms)
      (walk-form form filename))))

(defun format-parameters (params)
  "Format parameter list for display."
  (if params
      (format nil "~A" params)
      "()"))

(defun escape-markdown (text)
  "Escape special markdown characters in text."
  (when text
    (let ((result text))
      ;; Escape basic markdown characters
      (setf result (substitute #\Space #\Newline result))
      (setf result (substitute #\Space #\Tab result))
      ;; Remove excessive whitespace
      (string-trim " " result))))

(defun group-and-sort-ops (ops)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (o ops)
      (push o (gethash (op-info-file o) table)))
    (mapcar (lambda (file)
        (cons file
        (sort (gethash file table) #'string< :key #'op-info-name)))
      (sort (loop for k being the hash-keys of table collect k) #'string<))))

(defun generate-markdown ()
  "Generate markdown documentation."
  (format t "# API Documentation~%~%")
  (format t "Generated from Lisp source files.~%~%")

  ;; Built-in operators (reduction and scan)
  (format t "## Built-in Operators~%~%")
  (format t "These operators are part of the core language (parser-level) and are not~%")
  (format t "registered via `register-op`.~%~%")
  (format t "### Reduction~%~%")
  (format t "Reduce an array to a single value (or along the first axis for 2D arrays).~%~%")
  (format t "| Operator | Operation | Example |~%")
  (format t "|----------|-----------|---------|~%")
  (format t "| `/+` | Sum | `/+ [1 2 3 4 5]` -> 15 |~%")
  (format t "| `/*` | Product | `/* [1 2 3 4 5]` -> 120 |~%")
  (format t "| `/max` | Maximum | `/max [3 1 4 1 5]` -> 5 |~%")
  (format t "| `/min` | Minimum | `/min [3 1 4 1 5]` -> 1 |~%")
  (format t "~%For 2D arrays, reduction is along the first axis: `/+ [[1 2][3 4]]` -> #(4 6).~%")
  (format t "~%Scalar operands produce a clear error; use array syntax, e.g. `/+ [1 2 3]`.~%~%")
  (format t "---~%~%")
  (format t "### Scan~%~%")
  (format t "Prefix scan (inclusive): cumulative result at each position.~%~%")
  (format t "| Operator | Operation | Example |~%")
  (format t "|----------|-----------|---------|~%")
  (format t "| `&+` | Cumulative sum | `&+ [1 2 3 4 5]` -> #(1 3 6 10 15) |~%")
  (format t "| `&*` | Cumulative product | `&* [1 2 3 4 5]` -> #(1 2 6 24 120) |~%")
  (format t "~%Scalar operands produce a clear error; use array syntax, e.g. `&+ [1 2 3]`.~%~%")
  (format t "---~%~%")
  (format t "### REPL Error Handling~%~%")
  (format t "The interactive REPL catches evaluation errors, prints the message, and continues.~%")
  (format t "The session stays alive instead of exiting on error.~%~%")
  (format t "---~%~%")

  ;; Sort operations by name for consistent output
  (let ((sorted-ops (group-and-sort-ops *registered-ops*)))
    (if sorted-ops
  (progn
    (format t "## Registered Operations~%~%")
    (dolist (file-lst sorted-ops)
      (format t "### ~A ~%~%" (car file-lst))
      (format t "---~%")
      (dolist (op (cdr file-lst))
        (format t "#### `~A`~%~%" (op-info-name op))
        (format t "- **Function:** `~A`~%" (op-info-function op))
        (format t "- **Arity:** ~A~%" (op-info-arity op))
        (format t "- **Source:** ~A~%~%" (op-info-file op))

        ;; Look up function documentation
        (let ((func-info (gethash (op-info-function op) *functions*)))
    (when func-info
      (when (func-info-parameters func-info)
        (format t "**Parameters:** `~A`~%~%" 
          (format-parameters (func-info-parameters func-info))))
      (when (func-info-docstring func-info)
        (format t "**Description:**~%~%~A~%~%" 
          (escape-markdown (func-info-docstring func-info))))))
        (format t "---~%~%"))))
  (format t "No registered operations found.~%~%")))

    ;; Also list all functions with documentation
    (let ((documented-funcs '()))
      (maphash (lambda (name info)
     (when (func-info-docstring info)
       (push info documented-funcs)))
         *functions*)

      (when documented-funcs
  (setf documented-funcs (sort documented-funcs #'string< :key #'func-info-name))
  (format t "## All Documented Functions~%~%")
  (dolist (func documented-funcs)
    (format t "### `~A`~%~%" (func-info-name func))
    (format t "**Parameters:** `~A`~%~%" 
      (format-parameters (func-info-parameters func)))
    (format t "**Source:** ~A~%~%" (func-info-file func))
    (format t "~A~%~%" (escape-markdown (func-info-docstring func)))
    (format t "---~%~%")))))

(defun main ()
  "Main entry point."
  (let ((files (rest *posix-argv*)))
    (if files
  (progn
    (dolist (file files)
      (when (probe-file file)
        (process-file file)))
    (generate-markdown))
  (progn
    (format *error-output* "Usage: ~A file1.lisp file2.lisp …~%"
      (first *posix-argv*))
    (sb-ext:exit :code 1)))))

;; Run main function
(main)
