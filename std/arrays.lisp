; Array functions
(defun range-fn (n)
  "Generates a range from 0 to n-1: range 9: [0 1 2 3 4 5 6 7 8 9]"
  (let ((arr (make-array n :element-type 'number)))
    (dotimes (i n arr)
      (setf (aref arr i) i))))

(defun meshgrid-y (m n)
  "Generates a mesh in X direction of dimensions MxN"
  (let ((array (make-array (list m n) :element-type 'number)))
    (dotimes (i (* m n) array)
      (setf (row-major-aref array i) (mod i n)))
    array))

(defun meshgrid-x (m n)
  "Generates a mesh in Y direction of dimensions MxN"
  (let ((array (make-array (list m n) :element-type 'number)))
    (dotimes (i (* m n) array)
      (setf (row-major-aref array i) (floor (/ i n))))
    array))

;; table operations
;; rotate, transpose, reshape, mean, std, median
;; max min
;; reductions
(defun rotate (array)
  "Rotate clockwise, i.e. last element becomes first element"
  (cond
    ((null array)
     (error "Got empty input"))
    ((numberp array) array)
    ((stringp array) array)
    ((= (length (array-dimensions array)) 1)
     (let* ((len (length array))
	    (last (aref array (1- len))))
       (loop for i downfrom (1- len) above 0
	     do (setf (aref array i) (aref array (1- i))))
       (setf (aref array 0) last)
       array))
    ((arrayp array)
     (let* ((size (array-total-size array))
	    (dim0 (array-dimension array 1))
	    (new-array (make-array (array-dimensions array))))
       (loop for i from 0 below dim0 do
	 (setf (row-major-aref new-array i)
	       (row-major-aref array (+ i  (- size dim0)))))
       (loop for i from dim0 below size do
	 (setf (row-major-aref new-array i)
	       (row-major-aref array (- i dim0))))
       new-array))
    (t array)))

(defun transpose (matrix)
  "Transpose a matrix (list of lists)"
  (let* ((rows (array-dimension matrix 0))
	 (cols (array-dimension matrix 1))
	 (result (make-array (list cols rows)
			     :element-type (array-element-type matrix))))
    (dotimes (i rows)
      (dotimes (j cols)
	(setf (aref result j i) (aref matrix i j))))
    result))

;; shape, length
(defun shape-fn (array)
  "Return the shape of an array"
  (array-dimensions array))

(defun count-elements (array)
  "Count the number of elements in an array"
  (array-total-size array))

(defun rank-fn (array)
  (array-rank array))

(defun flatten (array)
  "Flatten a nested list structure into a single list."
  (let* ((size (array-total-size array))
	 (flat (make-array size :element-type (array-element-type array))))
    (dotimes (i size flat)
      (setf (aref flat i) (row-major-aref array i)))))


(defun reshape (shape array)
  "Reshape an array into a multi-dimensional array based on the given shape.
   The shape is a list of dimensions, e.g. (2 3) for a 2x3 matrix."
  (let* ((size (array-total-size array))
	 (new-dims (coerce shape 'list))
	 (result (make-array new-dims)))
    (if (/= (array-total-size result) size)
	(error "Non-matching shape ~A" new-dims)
	(dotimes (i size)
	  (setf (row-major-aref result i) (row-major-aref array i))))
    result))

(defun pick (index array)
  "Pick an element from an array based on the index.
   If index is a number, it returns the nth element.
   If index is a list, it traverses the array according to the indices in the list."
  (cond
    ((numberp index)
     (let ((v (aref array index)))
       (if v v (error "Invalid index ~A for ~A" index array))))
    ((arrayp index)
     (let ((array-c
	     (apply #'aref array (coerce index 'list))))
       (if array-c array-c (error "Invalid index ~A for ~A" index array))))
    (t (error "Invalid inputs to pick: ~A, ~A" index array))))

(defun array-slice (n arr)
  "Pick nth row from array"
  (make-array (array-dimension arr 1)
	      :displaced-to arr
	      :displaced-index-offset (* n (array-dimension arr 1))))

(defun take (index array)
  "Take the first N elements from an array."
  (cond
    ((numberp index)
     (handler-case
	 (let* ((dims (array-dimensions array))
		(rank (length dims)))
	   (unless (and (>= (first dims) index) (> rank 0))
	     (error "Invalid dimensions or too few elements to take."))
	   (let* ((new-dims (cons index (rest dims)))
		  (result (make-array new-dims
				      :element-type (array-element-type array))))
	     (dotimes (i (array-total-size result) result)
	       (setf (row-major-aref result i)
		     (row-major-aref array i)))))
       (error
	   (condition)
	 (declare (ignore condition))
	 (error "Invalid index: take ~A ~A" index array))))
     (t (error "Invalid inputs to take: ~A, ~A" index array))))

(defun drop (index array)
  "Drop the first N elements from an array."
  (cond
    ((numberp index)
     (let* ((dims (array-dimensions array))
	    (rank (length dims)))
       (unless (and (>= (first dims) index) (> rank 0))
	 (error "Invalid dimensions or too few elements to take."))
       (let* ((new-dims (cons (- (first dims) index) (rest dims)))
	      (result (make-array new-dims
				  :element-type (array-element-type array)))
	      (element-size (array-total-size result)))
	 (let ((offset (* index (reduce #'* (rest dims)))))
	   (dotimes (i element-size result)
	     (setf (row-major-aref result i)
		   (row-major-aref array (+ offset i))))))))
    (t (error "Invalid inputs to drop: ~A, ~A" index array))))

(defun array-row-major-index-to-subscript (dims index)
  "Convert row-major index to a list of subscripts for the given DIMS."
  (let ((coords ()))
    (dolist (dim (reverse dims) coords)
      (multiple-value-bind (q r) (floor index dim)
	(push r coords)
	(setf index q)))))


(defun reverse-array-first-axis (array)
  (cond
    ((stringp array) array)
    ((numberp array) array)
    ((arrayp array)
     (let* ((dimensions (array-dimensions array))
	    (first-dim (first dimensions))
	    (element-type (array-element-type array))
	    (reversed-array (make-array dimensions :element-type element-type)))
       (dotimes (i first-dim)
	 (let ((source-index (- (1- first-dim) i)))
	   (dotimes (j (reduce #'* (rest dimensions)))
	     (let ((indices (loop for dim in (rest dimensions)
				  for index = j then (floor index dim)
				  collect (mod index dim))))
	       (setf (apply #'aref reversed-array i indices)
		     (apply #'aref array source-index indices))))))
       reversed-array))
    (t (error "Cannot reverse ~A" array))))



(defun where (array)
  "Return the 1D array of index vectors of non-zero elements in an array."
  (let* ((dims (array-dimensions array))
	 (size (array-total-size array))
	 (indices '()))
    (dotimes (i size)
      (let ((val (row-major-aref array i)))
	(unless (zerop val)
	  (push (multiple-value-list (array-row-major-index-to-subscript dims i)) indices))))
    (make-array (length indices)
		:element-type 'vector
		:initial-contents (mapcar #'(lambda (lst) (coerce lst 'vector)) (nreverse indices)))))

(defun indexof (value array)
  "Return the index of the first occurrence of VALUE in ARRAY.
   If VALUE is not found, return the dimensions of the array."
  (let* ((dims (array-dimensions array))
	 (size (array-total-size array)))
    (dotimes (i size)
      (when (= (row-major-aref array i) value)
	(return-from indexof (coerce (multiple-value-list
			 (array-row-major-index-to-subscript dims i))
			'vector))))
    (coerce dims 'vector)))

;;; Joining, stacking, concatenating
(defun array-stack (array1 array2)
  "Join two arrays along a new axis
   Dimension analysis will be done and the
   most intuitive option returned, or an error
   if dimensions don't match"
  (let ((dims1 (array-dimensions array1))
	(dims2 (array-dimensions array2)))
    (cond
      ((= (length dims1) (length dims2))
       (cond
	 ((equal dims1 dims2)
	  (let* ((size1 (array-total-size array1))
		 (new-dims (push 2 dims1))
		 (new-array (make-array new-dims)))
	    (loop for i from 0 below size1 do
	      (setf (row-major-aref new-array i) (row-major-aref array1 i))
	      (setf (row-major-aref new-array (+ i size1)) (row-major-aref array2 i)))
	    new-array))
	 (t (error "Cannot concatenate arrays with different dimensions: ~A, ~A" dims1 dims2))))
      ((= (length dims1) (1- (length dims2))) ;; [[1 2][3 4]] and [5 6] --> [[1 2][3 4][5 6]]
       (cond
	 ((= (array-dimension array1 0) (array-dimension array2 0))
	  (let* ((new-dims (concatenate 'list
					(list (1+ (car dims2)))
					(cdr dims2)))
		 (new-array (make-array new-dims))
		 (size (array-total-size new-array)))
	    (loop for i from 0 below (array-total-size array1) do
	      (setf (row-major-aref new-array i) (row-major-aref array1 i)))
	    (loop for i from (array-total-size array1) below size do
	      (setf (row-major-aref new-array i) (row-major-aref array2 (- i (array-total-size array1)))))
	    new-array))
	 ((= (array-dimension array1 0) (array-dimension array2 1))
	  (let* ((new-dims (concatenate 'list
					(list (1+ (car dims2)))
					(cdr dims2)))
		 (new-array (make-array new-dims))
		 (size (array-total-size new-array)))
	    (loop for i from 0 below (array-total-size array1) do
	      (setf (row-major-aref new-array i) (row-major-aref array1 i)))
	    (loop for i from (array-total-size array1) below size do
	      (setf (row-major-aref new-array i) (row-major-aref array2 (- i (array-total-size array1)))))
	    new-array))
	 (t (error "Not implemented or not possible"))))
      ((= (length dims2) (1- (length dims1)))
       (cond
	 ((= (array-dimension array1 0) (array-dimension array2 0))
	  (let* ((new-dims (concatenate 'list
					(list (1+ (car dims1)))
					(cdr dims1)))
		 (new-array (make-array new-dims))
		 (size (array-total-size new-array)))
	    (loop for i from 0 below (array-total-size array1) do
	      (setf (row-major-aref new-array i) (row-major-aref array1 i)))
	    (loop for i from (array-total-size array1) below size do
	      (setf (row-major-aref new-array i) (row-major-aref array2 (- i (array-total-size array1)))))
	    new-array))
	 (t (error "Not implemented or not possible"))))
      (t (error "Not implemented or not possible")))))

(register-op 'size #'count-elements 1)
(register-op 'length #'length 1)
(register-op 'shape #'shape-fn 1)
(register-op 'range #'range-fn 1)
(register-op 'mesh-x #'meshgrid-x 2)
(register-op 'mesh-y #'meshgrid-y 2)
(register-op 'rotate #'rotate 1)
(register-op 'transpose #'transpose 1)
(register-op 'reshape #'reshape 2)
(register-op 'pick #'pick 2)
(register-op 'slice #'array-slice 2)
(register-op 'take #'take 2)
(register-op 'drop #'drop 2)
(register-op 'where #'where 1)
(register-op 'idx #'indexof 2)
(register-op 'flatten #'flatten 1)
(register-op 'rank #'rank-fn 1)
(register-op 'stack #'array-stack 2)

;; Magicl stuff
(handler-case (progn
		(ql:quickload :magicl)
		(load "linear_algebra.lisp"))
  (error ()
    (format t "Linear Algebra Not Loaded~%")))
  


;; Array-ops stuff
(ql:quickload :array-operations)

(defun nrow (a)
  "Number of rows in matrix"
  (aops:nrow a))

(defun ncol (a)
  "Number of columns in matrix"
  (aops:ncol a))

(defun sub (idx a)
  "Returns sub-array composed of the elements that would start with given subscripts"
  (aops:split a idx))

(defun zeros (dims)
  "Creates an array of dimensions DIMS filled with zeros"
  (aops:zeros (coerce dims 'list)))

(defun ones (dims)
  "Creates an array of dimensions DIMS filled with ones"
  (aops:ones (coerce dims 'list)))



(register-op 'nrow #'nrow 1)
(register-op 'ncol #'ncol 1)
(register-op 'sub #'sub 2)
(register-op 'zeros #'zeros 1)
(register-op 'ones #'ones 1)
(register-op 'reverse #'reverse-array-first-axis 1)
