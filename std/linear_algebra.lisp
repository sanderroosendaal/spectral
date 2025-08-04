;; Helper functions

(defun array-to-magicl-fast (arr)
  (let* ((dims (array-dimensions arr))
	 (flat-data (map 'vector
			 (lambda (x) (coerce x 'double-float))
			 (make-array (array-total-size arr) :displaced-to arr))))
    (magicl:from-list (coerce flat-data 'list) dims)))

(defun magicl-matrix-to-array (magicl-matrix)
  (let* ((dims (magicl:shape magicl-matrix))
	 (result (make-array dims)))
    (dotimes (i (first dims))
      (dotimes (j (second dims))
	(setf (aref result i j)
	      (magicl:tref magicl-matrix i j))))
    result))

(defun list-to-vector (lst)
  (make-array (list (length lst)) :initial-contents lst))

(defun matrix-multiply (a b)
  "BLAS/LAPACK Required: Matrix Multiply two matrices A and B."
  (let* ((magicl-a (array-to-magicl-fast a))
	 (magicl-b (array-to-magicl-fast b))
	 (result-magicl (magicl:@ magicl-a magicl-b)))
    (magicl-matrix-to-array result-magicl)))

(defun det-fn (a)
  "BLAS/LAPACK Required: Determinant of matrix"
  (unless (and (arrayp a) (= (length (array-dimensions a)) 2)
	       (= (first (array-dimensions a))
		  (second (array-dimensions a))))
    (error "Input must be a square matrix"))
  (let* ((magicl-a (array-to-magicl-fast a)))
      (magicl:det magicl-a)))

(defun trace-fn (a)
  "BLAS/LAPACK Required: Trace (sum of diagnoal elements) of matrix"
  (unless (and (arrayp a) (= (length (array-dimensions a)) 2)
	       (= (first (array-dimensions a))
		  (second (array-dimensions a))))
    (error "Input must be a square matrix"))
  (let* ((magicl-a (array-to-magicl-fast a)))
    (magicl:trace magicl-a)))

(defun triu (a)
  "BLAS/LAPACK Required: Upper triangular part of matrix"
  (unless (and (arrayp a) (= (length (array-dimensions a)) 2)
	       (= (first (array-dimensions a))
		  (second (array-dimensions a))))
    (error "Input must be a square matrix"))
  (let* ((magicl-a (array-to-magicl-fast a)))
    (magicl:upper-triangular magicl-a)))

(defun tril (a)
  "BLAS/LAPACK Required: Lower triangular part of matrix"
  (unless (and (arrayp a) (= (length (array-dimensions a)) 2)
	       (= (first (array-dimensions a))
		  (second (array-dimensions a))))
    (error "Input must be a square matrix"))
  (let* ((magicl-a (array-to-magicl-fast a)))
    (magicl:lower-triangular magicl-a)))
  

(defun dagger (a)
  "BLAS/LAPACK Required: Conjugate transport of matrix"
  (unless (and (arrayp a) (= (length (array-dimensions a)) 2))
    (error "Input must be a matrix"))
  (let* ((magicl-a (array-to-magicl-fast a)))
    (magicl:conjugate-transpose magicl-a)))

(defun matrix-inv (a)
  "BLAS/LAPACK Required: Matrix inverse"
  (let* ((magicl-a (array-to-magicl-fast a))
	 (result-magicl (magicl:inv magicl-a)))
    (magicl-matrix-to-array result-magicl)))

(defun eig-fn (a)
  "BLAS/LAPACK Required: Returns eigenvalues and eigenvectors of matrix"
  (unless (and (arrayp a) (= (length (array-dimensions a)) 2)
	       (= (first (array-dimensions a))
		  (second (array-dimensions a))))
    (error "Input must be a square matrix"))
  (let* ((magicl-a (array-to-magicl-fast a)))
    (multiple-value-bind (eigenvalues eigenvectors)
	(magicl:eig magicl-a)			  
      (values
       (list-to-vector eigenvalues)
       (magicl-matrix-to-array eigenvectors)))))

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
