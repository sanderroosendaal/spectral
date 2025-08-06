(load "std/fftw-ffi.lisp")

(defun convert-to-float (input)
  "Convert the input 1D array to a float array. Do nothing if the array is already double float."
  (unless (and (arrayp input) (= (array-dimension input 0) 1)
	       (eq (array-element-type input) 'double-float))
    input)
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
