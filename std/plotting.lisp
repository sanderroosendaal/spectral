(ql:quickload :vgplot)

(defmacro with-plot-rows-2d (array &optional (fun #'vgplot:plot))
"For a true 2D array, extract the first row as x and remaining rows as y1, y2, etc.
Expands into a call like (vgplot:plot x y1 x y2 ...)."
  (let ((array-sym (gensym "ARRAY"))
	(x-sym (gensym "X"))
	(ys-sym (gensym "YS"))
	(cols-sym (gensym "COLS"))
	(args-sym (gensym "ARGS")))
    `(let* ((,array-sym ,array)
	    (,cols-sym (array-dimension ,array-sym 1))
	    (,x-sym (loop for j below ,cols-sym
			  collect (aref ,array-sym 0 j)))
	    (,ys-sym (loop for i from 1 below (array-dimension ,array-sym 0)
			   collect (loop for j below ,cols-sym
						 collect (aref ,array-sym i j))))
	    (,args-sym (loop for y in ,ys-sym append (list ,x-sym y))))
       (declare (ignore ,cols-sym))
       (apply ,fun ,args-sym))))


(defun plot-fn (y)
  "Requires gnuplot. Creates a simple x-y line plot of the data array on the top of the stack.
   1D arrays are plotted index vs value. 2D arrays with two rows are interpreted as x-y-data.
   2D data sets are interpreted as x, y1, y2, y3 etc"
  (cond
    ((numberp y) (error "Need at least a 1D array to plot"))
    ((and (arrayp y) (= 1 (length (array-dimensions y))))
     (vgplot:plot
      (loop for x from 0 below (length y) collect (aref y x))))
    ((and (arrayp y) (= 2 (length (array-dimensions y))) (= 2 (first (array-dimensions y))))
     (let ((x (loop for i from 0 below (second (array-dimensions y)) collect (aref y 0 i)))
	   (y (loop for i from 0 below (second (array-dimensions y)) collect (aref y 1 i))))
       (vgplot:plot x y)))
    ((arrayp y)
     (with-plot-rows-2d y))
    (t (error "Invalid input or not implemented: ~A" y)))
  y)

(defun format-plot (text)
  "Sends string text directly to gnuplot and replots.
   Example: format-plot \"set ylabel \'Intensity\'"
  (vgplot:format-plot nil text)
  (vgplot:replot))

(defun 3D-plot (array)
  "3D plot. Array should have X, Y, and Z rows"
  (with-plot-rows-2d array #'vgplot:3d-plot)
  array)

(defun xyz-array-to-xyz (array)
  "Takes a (3xMxN) array and returns 3 (MxN) arrays"
  (unless (= (length (array-dimensions array)) 3)
    (error "Must be 3xMxN array"))
  (let* ((dimensions (array-dimensions array))
	 (xx (make-array (cdr dimensions)))
	 (yy (make-array (cdr dimensions)))
	 (zz (make-array (cdr dimensions)))
	 (m (first (cdr dimensions)))
	 (n (second (cdr dimensions))))
    (loop for i from 0 below m do
      (loop for j from 0 below n do
	(setf (aref xx i j) (aref array 0 i j))
	(setf (aref yy i j) (aref array 1 i j))
	(setf (aref zz i j) (aref array 2 i j))))
    (values xx yy zz)))

(defun surf-plot (y)
  "Plot 3-D surface mesh. Y could be a 2D table (just Z),
  or a 3D (X, Y, Z) array."
  (cond
    ((numberp y) (error "Need at least a 2D array to plot"))
    ((and (arrayp y) (= 2 (length (array-dimensions y))))
     (vgplot:surf y))
    ((and (arrayp y) (= 3 (length (array-dimensions y))))
     (multiple-value-bind (xx yy zz) (xyz-array-to-xyz y)
       (vgplot:surf xx yy zz)))
    (t (error "Invalid input for surface plot")))
  y)


(register-op 'plot #'plot-fn 1)
(register-op 'format-plot #'format-plot 1)
(register-op 'surf #'surf-plot 1)
(register-op '3d-plot #'3D-plot 1)
