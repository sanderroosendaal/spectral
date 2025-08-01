;; Array functions
(defun range-fn (n)
  (loop for i from 0 below n collect i))


;; table operations
;; rotate, transpose, reshape, mean, std, median
;; max min
;; reductions
(defun rotate (array)
  "Rotate clockwise, i.e. last element becomes first element"
  (if (null array)
      nil
      (cons (car (last array)) (butlast array))))

(defun transpose (matrix)
  "Transpose a matrix (list of lists)"
  (if (listp (first matrix))
      (apply #'mapcar #'list matrix)
      matrix))

(defun count-elements (array)
  "Count the number of elements in an array"
  (if (listp array)
      (reduce #'+ (mapcar #'count-elements array))
      1))

;; shape, length
(defun shape-fn (array)
  "Return the shape of an array"
  (labels ((shape-rec (lst)
	     (if (listp lst)
		 (cons (length lst)
		       (if (and lst (every #'listp lst))
			   (shape-rec (first lst))
			   '()))
		 '())))
    (shape-rec array)))


(defun flatten (lst)
  "Flatten a nested list structure into a single list."
  (cond
    ((null lst) nil)
    ((listp (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
    (t (cons (car lst) (flatten (cdr lst))))))

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
     (let ((v (nth index array)))
       (if v v (error "Invalid index ~A for ~A" index array))))
    ((listp index)
     (let ((array-c (copy-list array)))
       (loop for i in index do
	 (setf array-c (nth i array-c)))
       (if array-c array-c (error "Invalid index ~A for ~A" index array))))
    (t (error "Invalid inputs to pick: ~A, ~A" index array))))

(defun take (index array)
  "Take the first N elements from an array."
  (cond
    ((numberp index)
     (handler-case
	 (let ((v (subseq array 0 index)))
	   (if v v (error "Invalid index: take ~A ~A" index array)))
       (error
	   (condition)
	 (declare (ignore condition))
	 (error "Invalid index: take ~A ~A" index array))))
     (t (error "Invalid inputs to take: ~A, ~A" index array))))

(defun drop (index array)
  "Drop the first N elements from an array."
  (cond
    ((numberp index)
     (handler-case
	 (let ((v (subseq array index)))
	   (if v v (error "Invalid index: drop ~A ~A" index array)))
       (error
	   (condition)
	 (declare (ignore condition))
	 (error "Invalid index: drop ~A ~A" index array))))
    (t (error "Invalid inputs to drop: ~A, ~A" index array))))

(defun where (array)
  "Return the indices of non-zero elements in an array."
  (loop for item in array
	for i from 0
	unless (zerop item)
	  collect i))

(defun indexof (value array)
  "Return the index of the first occurrence of VALUE in ARRAY.
   If VALUE is not found, return the length of the array."
  (if (not (member value array :test #'equal))
      (length array)
      (position value array :test #'equal)))


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
