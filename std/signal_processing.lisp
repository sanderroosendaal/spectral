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

(register-op 'fft #'fft-fn 1)
(register-op 'ifft #'fft-inverse 1)
(register-op 'cfft #'cfft-fn 1)
