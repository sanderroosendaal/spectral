(defun handle-error (error &optional (message "An error occurred") (stream t) (file-name nil) (line-number nil) (column-number nil) (function-name nil))
  "Handle errors by printing a message and returning nil."
  (let ((error-message
	  (cond
	    ((and file-name line-number column-number)
	     (format stream "~A at ~A, line ~D, column ~D: " function-name file-name line-number column-number))
	    ((and function-name file-name line-number)
	     (format stream "~A at ~A, line ~D: " function-name file-name line-number))
	    ((and file-name column-number)
	     (format stream "~A at ~A, column ~D: " function-name file-name column-number))
	    ((and function-name line-number)
	     (format stream "~A at line ~D: " function-name line-number))
	    ((and function-name column-number)
	     (format stream "~A at column ~D: " function-name column-number))
	    ((and file-name line-number)
	     (format stream "Error in file ~A, line ~D: " file-name line-number))
	    (line-number
	     (format stream "Line ~D: " line-number)))))
     (setf error-message (format stream "~A ~A: ~A" error-message message error)))
  nil)
