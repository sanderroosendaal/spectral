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

(defmacro with-magicl (a op)
  `(let* ((magicl-a (array-to-magicl-fast ,a))
	  (result (funcall ,op magicl-a)))
     (magicl-matrix-to-array result)))

(defun matrix-multiply (a b)
  "BLAS/LAPACK Required: Matrix Multiply two matrices A and B."
  (let* ((magicl-a (array-to-magicl-fast a))
	 (magicl-b (array-to-magicl-fast b))
	 (result-magicl (magicl:@ magicl-a magicl-b)))
    (magicl-matrix-to-array result-magicl)))

(defun det-fn (a)
  "BLAS/LAPACK Required: Determinant of matrix"
  (with-magicl a #'magicl:det))

(defun trace-fn (a)
  "BLAS/LAPACK Required: Trace (sum of diagnoal elements) of matrix"
  (with-magicl a #'magicl:trace))

(defun triu (a)
  "BLAS/LAPACK Required: Upper triangular part of matrix"
  (with-magicl a #'magicl:upper-triangular))

(defun tril (a)
  "BLAS/LAPACK Required: Lower triangular part of matrix"
  (with-magicl a #'magicl:lower-triangular))

(defun dagger (a)
  "BLAS/LAPACK Required: Conjugate transport of matrix"
  (with-magicl a #'magicl:conjugate-transpose))

(defun matrix-inv (a)
  "BLAS/LAPACK Required: Matrix inverse"
  (with-magicl a #'magicl:inv))

(defun eig-fn (a)
  "BLAS/LAPACK Required: Returns eigenvalues and eigenvectors of matrix"
  (with-magicl a #'magicl:eig))

(register-op 'mmult #'matrix-multiply 2)
(register-op '@ #'matrix-multiply 2)
(register-op 'det #'det-fn 1)
(register-op 'trace #'trace-fn 1)
(register-op 'triu #'triu 1)
(register-op 'tril #'tril 1)
(register-op 'dagger #'dagger 1)
(register-op 'conjugate-transpose #'dagger 1)
(register-op 'inv #'matrix-inv 1)
(register-op 'eig #'eig-fn 1)
