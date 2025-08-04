(load "std/fftw-ffi.lisp")

(defun fft-forward (input)
  "Fast Fourier Transform of the input signal."
  (fftw-ffi:fft-real-forward input))

(defun fft-inverse (input)
  "Inverse Fast Fourier Transform of the input signal."
  (fftw-ffi:fft-backward input))

(register-op 'fft #'fft-forward 1)
(register-op 'ifft #'fft-inverse 1)
