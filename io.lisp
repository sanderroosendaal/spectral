;; Simple file I/O functions
(defun parse-number (str)
  "Parse a string as a number"
  (handler-case (read-from-string str)
    (error () 0)))

(defun load-numbers (filename)
  "Load numbers from a text file (one per line)"
  (with-open-file (stream filename :direction :input)
    (let ((result '()))
      (loop for line = (read-line stream nil nil)
	    while line
	    do (push (parse-number line) result))
      (nreverse result))))

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
(register-op 'run #'run-script 1)

