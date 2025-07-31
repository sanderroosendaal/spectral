#!/usr/bin/env sbcl -–script
(load "~/.sbclrc")

;; Robust Lisp documentation extractor
;; Usage: sbcl -–script doc-extractor.lisp *.lisp > documentation.md
(ql:quickload :cl-csv)

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
      ;; Handle quoted symbols and strings
      (when (and (listp name) (eq (first name) 'quote))
	(setf name (second name)))
      (when (and (listp function) (eq (first function) 'quote))
	(setf function (second function)))
      ;; Convert to strings
      (let ((name-str (cond ((stringp name) name)
			    ((symbolp name) (string-downcase (symbol-name name)))
			    (t (format nil "~A" name))))
	    (function-str (cond ((stringp function) function) ; Handle #\ escape)
				((symbolp function) (strip-hash-quote (string-downcase (symbol-name function))))
				(t (strip-hash-quote (string-downcase (format nil "~A" function))) )))
	    (arity-val (cond ((numberp arity) arity)
			     ((and (listp arity) (eq (first arity) 'quote))
			      (second arity))
			     (t arity))))
	(push (make-op-info :name name-str
			    :function function-str
			    :arity arity-val
			    :file filename)
	      *registered-ops*)))))

(defun walk-form (form filename)
  "Walk through a form and process defuns and register-op calls."
  (cond
    ((null form) nil)
    ((atom form) nil)
    ((eq (first form) 'defun)
     (process-defun form filename))
    ((or (eq (first form) 'register-op) (eq (first form) 'register-stack-op))
     (process-register-op form filename))
    (t
     ;; Recursively process sublists
     (dolist (subform form)
       (when (listp subform)
	 (walk-form subform filename))))))

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
		    (sort (gethash file table) #'string< :key #'op-info-function)))
	    (sort (loop for k being the hash-keys of table collect k) #'string<))))

(defun generate-markdown ()
  "Generate markdown documentation."
  (format t "# API Documentation~%~%")
  (format t "Generated from Lisp source files.~%~%")

  ;; Sort operations by name for consistent output
  (let ((sorted-ops (group-and-sort-ops *registered-ops*)))
    (if sorted-ops
	(progn
	  (format t "## Registered Operations~%~%")
	  (dolist (file-lst sorted-ops)
	    (format t "### ~A ~%~%" (car file-lst))
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
			    (escape-markdown (func-info-docstring func-info))))))))

	    (format t "---~%~%")))
	(format t "No registered operations found.~%~%"))

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
