;;;; Minimal script to write .sdat fixtures (no Spectral/Magicl deps).
;;;; Run: sbcl --noinform --script write-fixtures-minimal.lisp

(when *load-truename*
  (setf *default-pathname-defaults*
        (make-pathname :defaults (truename *load-truename*) :name nil :type nil)))

(defun write-uint32-le (n stream)
  (dotimes (i 4)
    (write-byte (ldb (byte 8 (* i 8)) (logand n #xFFFFFFFF)) stream)))

(defun write-int32-le (n stream)
  (write-uint32-le (ldb (byte 32 0) n) stream))

(defun write-float64-le (x stream)
  (let ((bits (sb-kernel:double-float-bits (float x 1.0d0))))
    (dotimes (j 8)
      (write-byte (ldb (byte 8 (* j 8)) bits) stream))))

(defun write-sdat (path data typecode bpe dims)
  (with-open-file (s path :direction :output :if-exists :supersede
                    :element-type '(unsigned-byte 8))
    (write-int32-le typecode s)
    (write-uint32-le bpe s)
    (write-uint32-le (length dims) s)
    (dolist (d dims) (write-uint32-le d s))
    (dolist (x data) (write-float64-le x s))))

(ensure-directories-exist (merge-pathnames "testdata/" *default-pathname-defaults*))

;; vec5
(write-sdat (merge-pathnames "testdata/vec5_float64.sdat" *default-pathname-defaults*) '(1.0 2.0 3.0 4.0 5.0) -1 8 '(5))
(format t "Wrote testdata/vec5_float64.sdat~%")

;; mat2x2 (row-major: 1 2 3 4)
(write-sdat "testdata/mat2x2_float64.sdat" '(1.0 2.0 3.0 4.0) -1 8 '(2 2))
(format t "Wrote testdata/mat2x2_float64.sdat~%")

;; vec_special
(write-sdat (merge-pathnames "testdata/vec_special.sdat" *default-pathname-defaults*) '(0.0 -1.0 2.0) -1 8 '(3))
(format t "Wrote testdata/vec_special.sdat~%")

;; vec100
(write-sdat (merge-pathnames "testdata/vec100.sdat" *default-pathname-defaults*) (loop for i from 0 to 99 collect (float i 1.0d0)) -1 8 '(100))
(format t "Wrote testdata/vec100.sdat~%")

(format t "Done. Fixtures in testdata/~%")
