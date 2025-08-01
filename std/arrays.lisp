;; Array functions
(defun range-fn (n)
  (let ((arr (make-array n :element-type 'number)))
    (dotimes (i n arr)
      (setf (aref arr i) i))))


;; table operations
;; rotate, transpose, reshape, mean, std, median
;; max min
;; reductions
(defun rotate (array)
  "Rotate clockwise, i.e. last element becomes first element"
  (if (null array)
      nil
      (let* ((len (length array))
	     (last (aref array (1- len))))
	(loop for i downfrom (1- len) above 0
	      do (setf (aref array i) (aref array (1- i))))
	(setf (aref array 0) last)
	array)))

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

(defun count-elements (array)
  "Count the number of elements in an array"
  (array-total-size array))

;; shape, length
(defun shape-fn (array)
  "Return the shape of an array"
  (array-dimensions array))


(defun flatten (array)
  "Flatten a nested list structure into a single list."
  (let* ((size (array-total-size array))
	 (flat (make-array size :element-type (array-element-type array))))
    (dotimes (i size flat)
      (setf (aref flat i) (row-major-aref array i)))))

(defun product (lst)
  (reduce #'* lst :initial-value 1))

(defun partition (lst size)
  (when lst
    (cons (subseq lst 0 size)
          (partition (nthcdr size lst) size))))

(defun reshape-rec (shape flat)
  (let ((dim (car shape))
        (rest (cdr shape)))
    (if (null rest)
        (partition flat dim) ;; Base case: just partition
        (let ((chunks (partition flat (* dim (product rest)))))
          (mapcar (lambda (chunk)
                    (reshape-rec rest chunk))
                  chunks)))))

(defun reshape (shape data)
  "Reshape a flat list into a multi-dimensional array based on the given shape.
   The shape is a list of dimensions, e.g. (2 3) for a 2x3 matrix."
  (let* ((flat (flatten data))
         (expected (product shape)))
    (unless (= (length flat) expected)
      (error "Cannot reshape ~D elements into shape ~A" (length flat) shape))
    (first (reshape-rec (reverse shape) flat))))

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

(register-op 'size #'count-elements 1)
(register-op 'length #'length 1)
(register-op 'shape #'shape-fn 1)
(register-op 'range #'range-fn 1)
(register-op 'rotate #'rotate 1)
(register-op 'transpose #'transpose 1)
(register-op 'reshape #'reshape 2)
(register-op 'pick #'pick 2)
(register-op 'take #'take 2)
(register-op 'drop #'drop 2)
(register-op 'where #'where 1)
(register-op 'idx #'indexof 2)
(register-op 'flatten #'flatten 1)
