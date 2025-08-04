(defun array-to-magicl-fast (arr)
  (let* ((dims (array-dimensions arr))
	 (flat-data (map 'vector
			 (lambda (x) (coerce x 'double-float))
			 (make-array (array-total-size arr) :displaced-to arr))))
    (magicl:from-list (coerce flat-data 'list) dims)))

(defun magicl-matrix-to-array (magicl-matrix)
  (let* ((dims (magicl:shape magicl-matrix))
	 (result (make-array dims :element-type 'double-float)))
    (dotimes (i (first dims))
      (dotimes (j (second dims))
	(setf (aref result i j)
	      (magicl:tref magicl-matrix i j))))
    result))

(defun matrix-multiply (a b)
  "BLAS/LAPACK Required: Matrix Multiply two matrices A and B."
  (let* ((magicl-a (array-to-magicl-fast a))
	 (magicl-b (array-to-magicl-fast b))
	 (result-magicl (magicl:@ magicl-a magicl-b)))
    (magicl-matrix-to-array result-magicl)))

(register-op 'mmult #'matrix-multiply 2)
(register-op '@ #'matrix-multiply 2)
