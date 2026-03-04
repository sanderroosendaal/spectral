(load (merge-pathnames "fftw-ffi.lisp"
                       (make-pathname :defaults *load-truename* :name nil :type nil)))

(defun convert-to-float (input)
  "Convert the input 1D array to a float array. Do nothing if the array is already double float."
  (when (and (arrayp input) (= (array-rank input) 1)
	     (eq (array-element-type input) 'double-float))
    (return-from convert-to-float input))
  (let ((float-array (make-array (length input) :element-type 'double-float)))
    (dotimes (i (length input))
      (setf (aref float-array i) (coerce (aref input i) 'double-float)))
    float-array))

(defun fft-fn (input)
  "Fast Fourier Transform of the input signal."
  (fftw-ffi:fft-forward (convert-to-float input)))

(defun cfft-fn (input)
  "Complex forward Fast Fourier Transform of the input signal"
  (fftw-ffi:fft-forward input))

(defun fft-inverse (input)
  "Inverse Fast Fourier Transform of the input signal."
  (fftw-ffi:fft-backward (convert-to-float input)))

(defun find-peaks-fn (data)
  "Return 1D array of indices of local maxima. Strict: x[i] > both neighbors."
  (let* ((vec (if (listp data) (coerce data 'vector) data))
	 (n (length vec))
	 (peaks '()))
    (when (zerop n)
      (return-from find-peaks-fn (vector)))
    (when (= n 1)
      (return-from find-peaks-fn (vector 0)))
    ;; i=0: peak if greater than right neighbor
    (when (> (elt vec 0) (elt vec 1))
      (push 0 peaks))
    ;; interior: peak if greater than both neighbors
    (loop for i from 1 to (- n 2) do
      (when (and (> (elt vec i) (elt vec (1- i)))
		 (> (elt vec i) (elt vec (1+ i))))
	(push i peaks)))
    ;; i=n-1: peak if greater than left neighbor
    (when (> (elt vec (1- n)) (elt vec (- n 2)))
      (push (1- n) peaks))
    (coerce (nreverse peaks) 'vector)))

(defun bandpass-fn (params signal)
  "Bandpass filter: keep frequencies in [f_low, f_high] Hz. Params = [f_low f_high sample_rate]."
  (let* ((params-vec (if (listp params) (coerce params 'vector) params))
	 (f-low (coerce (elt params-vec 0) 'double-float))
	 (f-high (coerce (elt params-vec 1) 'double-float))
	 (fs (coerce (elt params-vec 2) 'double-float))
	 (data (convert-to-float (if (listp signal) (coerce signal 'vector) signal)))
	 (n (length data))
	 (spec (fftw-ffi:fft-forward (map 'vector (lambda (x) (complex x 0.0d0)) data))))
    (unless (>= n 2)
      (error "bandpass: signal must have at least 2 samples, got ~A" n))
    ;; Zero bins outside [f_low, f_high]. Bin k has freq k*Fs/N (k<=N/2) or (N-k)*Fs/N (k>N/2)
    (dotimes (k n)
      (let ((freq (if (<= k (floor n 2))
		      (* k fs (/ n))
		      (* (- n k) fs (/ n)))))
	(unless (and (>= freq f-low) (<= freq f-high))
	  (setf (aref spec k) (complex 0.0d0 0.0d0)))))
    (let ((filtered (fftw-ffi:fft-backward spec)))
      (map 'vector #'realpart filtered))))

(defun smooth-fn (window-size signal)
  "Boxcar moving average. window_size points, same-length output, partial windows at edges."
  (let* ((vec (if (listp signal) (coerce signal 'vector) signal))
	 (n (length vec))
	 (w (max 1 (floor (coerce window-size 'real)))))
    (when (zerop n)
      (return-from smooth-fn (vector)))
    (let ((half (floor w 2))
	  (result (make-array n :element-type 'double-float)))
      (dotimes (i n)
	(let ((lo (max 0 (- i half)))
	      (hi (min (1- n) (+ i half))))
	  (setf (aref result i)
		(/ (loop for j from lo to hi sum (coerce (elt vec j) 'double-float))
		   (1+ (- hi lo))))))
      result)))

;; Savitzky-Golay: compute convolution kernel from (window_length, poly_order)
(defun savgol-kernel (m n)
  "Return 1D array of SG coefficients for window m, polynomial order n."
  (let* ((x-center (/ (1- m) 2.0d0))
	 (X (make-array (list m (1+ n)) :element-type 'double-float :initial-element 0.0d0)))
    (dotimes (i m)
      (let ((xi (- i x-center)))
	(setf (aref X i 0) 1.0d0)
	(loop for j from 1 to n do
	  (setf (aref X i j) (* (aref X i (1- j)) xi)))))
    ;; (X'X)^{-1} X' via Gauss-Jordan; we need first row of result
    (let* ((np1 (1+ n))
	   (XtX (make-array (list np1 np1) :element-type 'double-float :initial-element 0.0d0)))
      ;; XtX = X' * X
      (dotimes (i np1)
	(dotimes (j np1)
	  (setf (aref XtX i j)
		(loop for k from 0 below m sum (* (aref X k i) (aref X k j))))))
      ;; Augment with identity for Gauss-Jordan to get XtX^{-1}
      (let ((aug (make-array (list np1 (* 2 np1)) :element-type 'double-float)))
	(dotimes (i np1)
	  (dotimes (j np1)
	    (setf (aref aug i j) (aref XtX i j)))
	  (dotimes (j np1)
	    (setf (aref aug i (+ np1 j)) (if (= i j) 1.0d0 0.0d0))))
	;; Gauss-Jordan elimination
	(dotimes (pivot np1)
	  (let ((p (aref aug pivot pivot)))
	    (when (< (abs p) 1.0d-14)
	      (error "savgol: singular matrix for window ~A poly ~A" m n))
	    (dotimes (col (* 2 np1))
	      (setf (aref aug pivot col) (/ (aref aug pivot col) p)))
	    (dotimes (row np1)
	      (unless (= row pivot)
		(let ((f (aref aug row pivot)))
		  (dotimes (col (* 2 np1))
		    (decf (aref aug row col) (* f (aref aug pivot col)))))))))
	;; aug now has [I | XtX^{-1}]. Compute first row of XtX^{-1} * X' = kernel
	(let ((kernel (make-array m :element-type 'double-float)))
	  (dotimes (j m)
	    (setf (aref kernel j)
		  (loop for i from 0 below np1 sum (* (aref aug 0 (+ np1 i)) (aref X j i)))))
	  kernel)))))

(defun savgol-reflect (vec idx)
  (let ((n (length vec)))
    (when (= n 1) (return-from savgol-reflect (aref vec 0)))
    (let* ((period (* 2 (1- n)))
	   (idx (mod idx period)))
      (aref vec (if (>= idx n) (- period idx) idx)))))

(defun savgol-fn (params signal)
  "Savitzky-Golay polynomial smoothing. Params = [window_length poly_order]."
  (let* ((params-vec (if (listp params) (coerce params 'vector) params))
	 (m-raw (max 3 (floor (coerce (elt params-vec 0) 'real))))
	 (m (if (oddp m-raw) m-raw (1+ m-raw)))
	 (n (min (1- m) (max 0 (floor (coerce (elt params-vec 1) 'real)))))
	 (vec (if (listp signal) (coerce signal 'vector) signal))
	 (len (length vec)))
    (when (zerop len)
      (return-from savgol-fn (vector)))
    (let* ((kernel (savgol-kernel m n))
	   (half (floor m 2))
	   (result (make-array len :element-type 'double-float)))
      (dotimes (i len)
	(setf (aref result i)
	      (loop for k from 0 below m
		    sum (* (aref kernel k)
			   (coerce (savgol-reflect vec (+ i (- k half))) 'double-float)))))
      result)))

(register-op 'fft #'fft-fn 1)
(register-op 'ifft #'fft-inverse 1)
(register-op 'cfft #'cfft-fn 1)
(register-op 'find-peaks #'find-peaks-fn 1)
(register-op 'bandpass #'bandpass-fn 2)
(register-op 'smooth #'smooth-fn 2)
(register-op 'savgol #'savgol-fn 2)
