;;;; FFTW FFI Integration for Signal Processing
;;;; Requires: CFFI, and FFTW3 library installed
(ql:quickload :cffi)

(defpackage :fftw-ffi
  (:use :cl :cffi)
  (:export #:fft-forward
           #:fft-backward
           #:fft-cleanup
           #:with-fftw-arrays))

(in-package :fftw-ffi)

;;; Load the FFTW library
(define-foreign-library libfftw3
  (:unix (:or "libfftw3.so.3" "libfftw3.so"))
  (:darwin (:or "libfftw3.dylib" "/usr/local/lib/libfftw3.dylib"))
  (:windows "libfftw3-3.dll"))

(use-foreign-library libfftw3)

;;; FFTW Constants
(defconstant +fftw-forward+ -1)
(defconstant +fftw-backward+ +1)
(defconstant +fftw-estimate+ (ash 1 6))
(defconstant +fftw-measure+ 0)
(defconstant +fftw-patient+ (ash 1 5))

;;; Foreign type definitions
(defctype fftw-complex :double)
(defctype fftw-plan :pointer)

;;; Core FFTW functions
(defcfun ("fftw_plan_dft_1d" fftw-plan-dft-1d) fftw-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

(defcfun ("fftw_plan_dft_r2c_1d" fftw-plan-dft-r2c-1d) fftw-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (flags :unsigned-int))

(defcfun ("fftw_plan_dft_c2r_1d" fftw-plan-dft-c2r-1d) fftw-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (flags :unsigned-int))

(defcfun ("fftw_execute" fftw-execute) :void
  (plan fftw-plan))

(defcfun ("fftw_destroy_plan" fftw-destroy-plan) :void
  (plan fftw-plan))

(defcfun ("fftw_malloc" fftw-malloc) :pointer
  (size :size))

(defcfun ("fftw_free" fftw-free) :void
  (ptr :pointer))

(defcfun ("fftw_cleanup" fftw-cleanup) :void)

;;; Memory management utilities
(defmacro with-fftw-arrays (bindings &body body)
  "Automatically manage FFTW-aligned memory allocation and cleanup"
  `(let ,(mapcar (lambda (binding)
                   (destructuring-bind (var size) binding
                     `(,var (fftw-malloc ,size))))
                 bindings)
     (unwind-protect
          (progn ,@body)
       ,@(mapcar (lambda (binding)
                   `(when ,(first binding)
                      (fftw-free ,(first binding))))
                 bindings))))

;;; Helper functions for data conversion
(defun lisp-array-to-fftw-complex (lisp-array fftw-ptr)
  "Copy Lisp complex array to FFTW complex array"
  (loop for i from 0 below (length lisp-array)
        for offset from 0 by 16  ; 2 doubles = 16 bytes
        do (setf (mem-ref fftw-ptr :double offset)
                 (realpart (aref lisp-array i))
                 (mem-ref fftw-ptr :double (+ offset 8))
                 (imagpart (aref lisp-array i)))))

(defun fftw-complex-to-lisp-array (fftw-ptr size)
  "Convert FFTW complex array back to Lisp array"
  (let ((result (make-array size :element-type '(complex double-float))))
    (loop for i from 0 below size
          for offset from 0 by 16
          do (setf (aref result i)
                   (complex (mem-ref fftw-ptr :double offset)
                            (mem-ref fftw-ptr :double (+ offset 8)))))
    result))

(defun lisp-array-to-fftw-real (lisp-array fftw-ptr)
  "Copy Lisp real array to FFTW real array"
  (loop for i from 0 below (length lisp-array)
        for offset from 0 by 8  ; 1 double = 8 bytes
        do (setf (mem-ref fftw-ptr :double offset)
                 (coerce (aref lisp-array i) 'double-float))))

(defun fftw-real-to-lisp-array (fftw-ptr size)
  "Convert FFTW real array back to Lisp array"
  (let ((result (make-array size :element-type 'double-float)))
    (loop for i from 0 below size
          for offset from 0 by 8
          do (setf (aref result i)
                   (mem-ref fftw-ptr :double offset)))
    result))

;;; High-level FFT functions
(defun fft-forward (data &key (normalize t))
  "Compute forward FFT of complex data"
  (let* ((n (length data))
         (complex-size (* n 16)))  ; 2 doubles per complex number
    (with-fftw-arrays ((in-ptr complex-size)
                       (out-ptr complex-size))
      ;; Copy input data
      (lisp-array-to-fftw-complex data in-ptr)
      
      ;; Create and execute plan
      (let ((plan (fftw-plan-dft-1d n in-ptr out-ptr 
                                    +fftw-forward+ +fftw-estimate+)))
        (fftw-execute plan)
        (fftw-destroy-plan plan)
        
        ;; Convert result back
        (let ((result (fftw-complex-to-lisp-array out-ptr n)))
          (if normalize
              (map 'vector (lambda (x) (/ x (sqrt n))) result)
              result))))))

(defun fft-backward (data &key (normalize t))
  "Compute backward (inverse) FFT of complex data"
  (let* ((n (length data))
         (complex-size (* n 16)))
    (with-fftw-arrays ((in-ptr complex-size)
                       (out-ptr complex-size))
      ;; Copy input data
      (lisp-array-to-fftw-complex data in-ptr)
      
      ;; Create and execute plan
      (let ((plan (fftw-plan-dft-1d n in-ptr out-ptr 
                                    +fftw-backward+ +fftw-estimate+)))
        (fftw-execute plan)
        (fftw-destroy-plan plan)
        
        ;; Convert result back
        (let ((result (fftw-complex-to-lisp-array out-ptr n)))
          (if normalize
              (map 'vector (lambda (x) (/ x (if normalize (sqrt n) n))) result)
              result))))))

(defun fft-real-forward (data)
  "Compute forward FFT of real data (returns complex)"
  (let* ((n (length data))
         (real-size (* n 8))          ; n doubles for input
         (complex-size (* (+ (floor n 2) 1) 16))) ; (n/2+1) complex numbers for output
    (with-fftw-arrays ((in-ptr real-size)
                       (out-ptr complex-size))
      ;; Copy input data
      (lisp-array-to-fftw-real data in-ptr)
      
      ;; Create and execute plan
      (let ((plan (fftw-plan-dft-r2c-1d n in-ptr out-ptr +fftw-estimate+)))
        (fftw-execute plan)
        (fftw-destroy-plan plan)
        
        ;; Convert result back
        (fftw-complex-to-lisp-array out-ptr (+ (floor n 2) 1))))))

;;; Utility functions for testing
(defun make-test-signal (n &key (frequency 1.0) (sample-rate 10.0))
  "Generate a test sinusoidal signal"
  (let ((result (make-array n :element-type 'double-float)))
    (loop for i from 0 below n
          do (setf (aref result i)
                   (sin (* 2 pi frequency i (/ sample-rate)))))
    result))

(defun make-complex-test-signal (n &key (frequency 1.0) (sample-rate 10.0))
  "Generate a complex test signal"
  (let ((result (make-array n :element-type '(complex double-float))))
    (loop for i from 0 below n
          do (setf (aref result i)
                   (cis (* 2 pi frequency i (/ sample-rate)))))
    result))

;;; Cleanup function
(defun fft-cleanup ()
  "Clean up FFTW internal data structures"
  (fftw-cleanup))

;;; Example usage:
#|
;; Load and test
(ql:quickload :cffi)

;; Create a test signal
(defparameter *test-data* (make-test-signal 64 :frequency 2.0 :sample-rate 64.0))

;; Compute FFT
(defparameter *fft-result* (fft-real-forward *test-data*))

;; Print first few components
(loop for i from 0 below (min 10 (length *fft-result*))
      do (format t "~A: ~A~%" i (aref *fft-result* i)))

;; Clean up when done
(fft-cleanup)
|#
