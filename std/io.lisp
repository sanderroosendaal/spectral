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
      (coerce
       (nreverse result) 'vector))))

(defun write-numbers (filename data)
  "Write numbers (vector) to a text file (one per line)"
  (with-open-file (stream filename :direction :output
				   :if-exists :supersede :element-type 'character)
    (loop for i from 0 to (1- (length data))
	  do
	     (format stream "~A~%" (aref data i)))
    (terpri stream))
  data)

(defun is-header-row (row)
  "Check if a row contains headers"
  (some (lambda (cell)
	  (null (parse-number-safe cell)))
	row))

(defun write-csv-field (value stream delimiter)
  "Write a single field to CSV stream, properly escaping if necessary."
  (let ((string-value (princ-to-string value)))
    (cond
      ;; If field contains delimiter, newline, or quote, wrap in quotes and escape internal quotes
      ((or (find delimiter string-value)
           (find #\Newline string-value)
           (find #\" string-value))
       (write-char #\" stream)
       (loop for char across string-value do
         (when (char= char #\")
           (write-char #\" stream)) ; Escape quotes by doubling them
         (write-char char stream))
       (write-char #\" stream))
      ;; Otherwise, write the value as-is
      (t
       (write-string string-value stream)))))

(defun write-csv (matrix filename &optional (delimiter #\,))
  "Saves a 2D lisp array to a CSV file, comma separated, row oriented"
  (unless (arrayp matrix)
    (error "Expected a 2D array, got ~A" (type-of matrix)))
  (unless (= (array-rank matrix) 2)
    (error "Expected a 2D array, got ~A" (array-rank matrix)))
  (with-open-file (stream filename :direction :output :if-exists :supersede
				   :element-type 'character)
    (let ((rows (first (array-dimensions matrix)))
	  (cols (second (array-dimensions matrix))))
      (dotimes (row rows)
	(dotimes (col cols)
	  (let ((value (aref matrix row col)))
	    (write-csv-field value stream delimiter)
	    (when (< col (1- cols))
	      (write-char delimiter stream))))
	(terpri stream))))
  matrix)


(defun load-csv (filename)
  "Loads a table from a CSV file, comma separated, row oriented"
  (let* ((all-rows (cl-csv:read-csv (pathname filename)))
	 (first-row (first all-rows))
	 (headers nil)
	 (data-rows nil))
    (if (is-header-row first-row)
	(setf headers (coerce first-row 'vector)
	      data-rows (rest all-rows))
	(setf data-rows all-rows))

    ;; Parse numbers in data rows
    (let ((parsed-data
	    (coerce
	     (mapcar (lambda (row)
		       (mapcar #'parse-number-safe row))
		     data-rows) 'vector)))
      (if headers
	  (values headers parsed-data)
	  parsed-data))))

(defun run-script (filename)
  "Execute a script file line by line"
  (with-open-file (stream filename :direction :input)
    (let ((line-count 1))
      (loop for line = (read-line stream nil nil)
	    while line
	    when (and (> (length line) 0)
		      (not (char= (char line 0) #\;))) ; Skip comments
	      do (evaluate (string-trim " " line)) ;; -- need to add filename line-count)
	    (incf line-count)))
     (peek-stack)))


;; Add file operations
(register-op 'load #'load-numbers 1)
(register-op 'write #'write-numbers 2)
(register-op 'load-csv #'load-csv 1)
(register-op 'write-csv #'write-csv 2)
(register-op 'run #'run-script 1)

