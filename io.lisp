;; Simple file I/O functions
(ql:quickload :cl-csv)

(defun parse-number (str)
  "Parse a string as a number"
  (handler-case (read-from-string str)
    (error () 0)))

(defun parse-number-safe (string)
  "Safely parse a string to number"
  (handler-case
      (let ((trimmed (string-trim '(#\Space #\Tab) string)))
	(cond
	  ((string= trimmed "") nil)
	  ((every #'digit-char-p trimmed) (parse-integer trimmed))
	  ((or (find #\. trimmed) (find #\e trimmed) (find #\E trimmed))
	   (read-from-string trimmed))
	  (t (parse-integer trimmed))))
    (error () nil)))

(defun load-numbers (filename)
  "Load numbers from a text file (one per line)"
  (with-open-file (stream filename :direction :input)
    (let ((result '()))
      (loop for line = (read-line stream nil nil)
	    while line
	    do (push (parse-number line) result))
      (nreverse result))))

(defun is-header-row (row)
  "Check if a row contains headers"
  (some (lambda (cell)
	  (null (parse-number-safe cell)))
	row))

(defun load-csv (filename &key (has-headers :auto))
  "Loads a table from a CSV file, comma separated, row oriented"
  (let* ((all-rows (cl-csv:read-csv (pathname filename)))
	 (first-row (first all-rows))
	 (headers nil)
	 (data-rows nil))
    (cond
      ;; auto-detect headers
      ((eq has-headers :auto)
       (if (is-header-row first-row)
	   (setf headers first-row
		 data-rows (rest all-rows))
	   (setf data-rows all-rows)))
      ;; explicitly has headers
      (has-headers
       (setf headers first-row
	     data-rows (rest all-rows)))
      ;; No headers
      (t (setf data-rows all-rows)))

    ;; Parse numbers in data rows
    (let ((parsed-data
	    (mapcar (lambda (row)
		      (mapcar #'parse-number-safe row))
		    data-rows)))
      (if headers
	  (values headers parsed-data)
	  parsed-data))))

(defun run-script (filename)
  "Execute a script file line by line"
   (with-open-file (stream filename :direction :input)
     (loop for line = (read-line stream nil nil)
	   while line
	   when (and (> (length line) 0)
		     (not (char= (char line 0) #\;))) ; Skip comments
	     do (evaluate (string-trim " " line)))
     (peek-stack)))


;; Add file operations
(register-op 'load #'load-numbers 1)
(register-op 'load-csv #'load-csv 1)
(register-op 'run #'run-script 1)

