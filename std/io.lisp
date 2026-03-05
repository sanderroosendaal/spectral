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


;;; Binary array I/O (.sdat format - MDA-inspired, zero deps)
;;; Header: 4b type, 4b bytes/elem, 4b ndims, 4b×ndims dims (little-endian)
;;; Type codes: -1 float64, -2 float32, -3 int32, -4 int16

(defconstant +sdat-float64+ -1)
(defconstant +sdat-float32+ -2)
(defconstant +sdat-int32+ -3)
(defconstant +sdat-int16+ -4)

(defun write-uint32-le (n stream)
  (dotimes (i 4)
    (write-byte (ldb (byte 8 (* i 8)) (logand n #xFFFFFFFF)) stream)))

(defun read-uint32-le (stream)
  (let ((n 0))
    (dotimes (i 4 n)
      (setf n (logior n (ash (read-byte stream) (* i 8)))))))

(defun write-int32-le (n stream)
  (write-uint32-le (ldb (byte 32 0) n) stream))

(defun read-int32-le (stream)
  (let ((u (read-uint32-le stream)))
    (if (logbitp 31 u) (logior u #x-100000000) u)))

(defun array-element-type-to-sdat-code (arr)
  (let ((et (array-element-type arr)))
    (cond ((subtypep et 'double-float) +sdat-float64+)
          ((subtypep et 'single-float) +sdat-float32+)
          ((subtypep et '(signed-byte 32)) +sdat-int32+)
          ((subtypep et '(signed-byte 16)) +sdat-int16+)
          (t +sdat-float64+)))) ; coerce to float64

(defun sdat-bytes-per-element (typecode)
  (case typecode
    ((-1) 8)   ; float64
    ((-2) 4)   ; float32
    ((-3) 4)   ; int32
    ((-4) 2)   ; int16
    (t (error "Unknown .sdat type code: ~A" typecode))))

(defun write-binary (filename data)
  "Write array to .sdat binary file. Supports float64, float32, int32, int16."
  (unless (arrayp data)
    (error "Expected an array, got ~A" (type-of data)))
  (let* ((dims (array-dimensions data))
         (ndims (length dims))
         (typecode (array-element-type-to-sdat-code data))
         (bpe (sdat-bytes-per-element typecode)))
    (when (> ndims 8)
      (error ".sdat supports at most 8 dimensions, got ~A" ndims))
    (with-open-file (s filename :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      (write-int32-le typecode s)
      (write-uint32-le bpe s)
      (write-uint32-le ndims s)
      (dolist (d dims)
        (write-uint32-le (ldb (byte 32 0) d) s))
      ;; Write row-major data (coerce to appropriate type for typecode)
      (flet ((write-elem (x)
               (case typecode
                 ((-1) (write-float64-le (coerce x 'double-float) s))
                 ((-2) (write-float32-le (coerce x 'single-float) s))
                 ((-3) (write-int32-le (round (coerce x 'double-float)) s))
                 ((-4) (write-int16-le (round (coerce x 'double-float)) s))
                 (t (write-float64-le (coerce x 'double-float) s)))))
        (dotimes (i (array-total-size data))
          (write-elem (row-major-aref data i))))))
  data)

(defun write-float64-le (x stream)
  #+sbcl
  (let ((bits (sb-kernel:double-float-bits x)))
    (dotimes (j 8)
      (write-byte (ldb (byte 8 (* j 8)) bits) stream)))
  #-sbcl
  (let ((bits (double-to-ieee754-bits x)))
    (dotimes (j 8)
      (write-byte (ldb (byte 8 (* j 8)) bits) stream))))

(defun double-to-ieee754-bits (x)
  (multiple-value-bind (sig exp sign) (integer-decode-float x)
    (let* ((sign-bit (ash (if (minusp sign) 1 0) 63))
           (mant (logand sig #xFFFFFFFFFFFFF))
           (ieee-exp (+ exp 1023)))
      (when (or (< ieee-exp 0) (>= ieee-exp 2047))
        (return-from double-to-ieee754-bits 0))
      (logior sign-bit (ash ieee-exp 52) mant))))

(defun write-float32-le (x stream)
  (let ((bits (single-to-ieee754-bits x)))
    (dotimes (j 4)
      (write-byte (ldb (byte 8 (* j 8)) bits) stream))))

(defun single-to-ieee754-bits (x)
  (multiple-value-bind (sig exp sign) (integer-decode-float (float x 1.0d0))
    (let* ((sign-bit (ash (if (minusp sign) 1 0) 31))
           (mant (logand sig #x7FFFFF))
           (ieee-exp (+ exp 127)))
      (when (or (< ieee-exp 0) (>= ieee-exp 256))
        (return-from single-to-ieee754-bits 0))
      (logior sign-bit (ash ieee-exp 23) mant))))

(defun write-int16-le (n stream)
  (let ((u (ldb (byte 16 0) (round n))))
    (write-byte (ldb (byte 8 0) u) stream)
    (write-byte (ldb (byte 8 8) u) stream)))

(defun ieee754-bits-to-double (bits)
  (let ((sign (if (logbitp 63 bits) -1 1))
        (exp (ldb (byte 11 52) bits))
        (mant (ldb (byte 52 0) bits)))
    (cond
      ((= exp 0)
       (if (= mant 0) (* sign 0.0d0)
           (* sign (scale-float (float mant 1.0d0) -1074))))
      ((= exp 2047)
       (if (= mant 0) (* sign most-positive-double-float)
           (* sign most-positive-double-float)))
      (t
       (* sign (scale-float (1+ (scale-float (float mant 1.0d0) -52))
                            (- exp 1023)))))))

(defun read-float64-le (stream)
  (let ((bits 0))
    (dotimes (i 8)
      (setf bits (logior bits (ash (read-byte stream) (* i 8)))))
    (ieee754-bits-to-double bits)))

(defun ieee754-bits-to-single (bits)
  (let ((sign (if (logbitp 31 bits) -1 1))
        (exp (ldb (byte 8 23) bits))
        (mant (ldb (byte 23 0) bits)))
    (if (or (= exp 0) (= exp 255))
        (float 0 1.0f0)
        (coerce (* sign (scale-float (1+ (scale-float (float mant 1.0f0) -23))
                                     (- exp 127)))
                'single-float))))

(defun read-float32-le (stream)
  (let ((bits 0))
    (dotimes (i 4)
      (setf bits (logior bits (ash (read-byte stream) (* i 8)))))
    (ieee754-bits-to-single bits)))

(defun read-int16-le (stream)
  (let ((low (read-byte stream))
        (high (read-byte stream)))
    (let ((u (logior low (ash high 8))))
      (if (logbitp 15 u) (logior u #x-10000) u))))

(defun load-binary (filename)
  "Load array from .sdat binary file. Returns double-float array."
  (with-open-file (s filename :direction :input :element-type '(unsigned-byte 8))
    (let* ((typecode (read-int32-le s))
           (_bpe (read-uint32-le s))
           (ndims (read-uint32-le s)))
      (declare (ignore _bpe))
      (when (> ndims 8)
        (error "Invalid .sdat: ndims ~A > 8" ndims))
      (let ((dims (loop repeat ndims collect (read-uint32-le s))))
        (let ((total (reduce #'* dims))
              (et (case typecode
                    ((-1) 'double-float)
                    ((-2) 'single-float)
                    ((-3) '(signed-byte 32))
                    ((-4) '(signed-byte 16))
                    (t (error "Unknown .sdat type code: ~A" typecode)))))
          (let ((arr (make-array dims :element-type et)))
            (dotimes (i total)
              (setf (row-major-aref arr i)
                    (case typecode
                      ((-1) (read-float64-le s))
                      ((-2) (read-float32-le s))
                      ((-3) (read-int32-le s))
                      ((-4) (read-int16-le s))
                      (t (error "Unknown type ~A" typecode)))))
            ;; Coerce to double-float for Spectral uniformity
            (if (subtypep et 'double-float)
                arr
                (let ((out (make-array dims :element-type 'double-float)))
                  (dotimes (i total)
                    (setf (row-major-aref out i)
                          (coerce (row-major-aref arr i) 'double-float)))
                  out))))))))


;; Add file operations
(register-op 'load #'load-numbers 1)
(register-op 'write #'write-numbers 2)
(register-op 'load-csv #'load-csv 1)
(register-op 'write-csv #'write-csv 2)
(register-op 'run #'run-script 1)
(register-op 'load-binary #'load-binary 1)
(register-op 'write-binary #'write-binary 2)

