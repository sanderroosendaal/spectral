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
  (apply #'mapcar #'list matrix))

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
  (let* ((flat (flatten data))
         (expected (product shape)))
    (unless (= (length flat) expected)
      (error "Cannot reshape ~D elements into shape ~A" (length flat) shape))
    (first (reshape-rec (reverse shape) flat))))

(register-op 'size #'count-elements 1)
(register-op 'length #'length 1)
(register-op 'shape #'shape-fn 1)
(register-op 'range #'range-fn 1)
(register-op 'rotate #'rotate 1)
(register-op 'transpose #'transpose 1)
(register-op 'reshape #'reshape 2)
