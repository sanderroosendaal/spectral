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

(register-op 'fft #'fft-fn 1)
(register-op 'ifft #'fft-inverse 1)
(register-op 'cfft #'cfft-fn 1)
(register-op 'find-peaks #'find-peaks-fn 1)
(register-op 'bandpass #'bandpass-fn 2)
(register-op 'smooth #'smooth-fn 2)
