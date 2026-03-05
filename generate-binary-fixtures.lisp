;;;; Generate .sdat test fixture files for binary I/O tests.
;;;; Run: sbcl --noinform --load generate-binary-fixtures.lisp

(let ((script-dir (make-pathname :defaults (truename *load-truename*)
                                 :name nil :type nil)))
  (setf *default-pathname-defaults* script-dir))

(load "spectral.lisp")
(in-package :spectral)

;; Ensure testdata exists
(ensure-directories-exist (merge-pathnames "testdata/" *default-pathname-defaults*))

;; Fixture 1: 1D float64 vector [1.0 2.0 3.0 4.0 5.0]
(let ((data (coerce #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0) 'vector)))
  (write-binary (merge-pathnames "testdata/vec5_float64.sdat" *default-pathname-defaults*)
                data)
  (format t "Wrote testdata/vec5_float64.sdat~%"))

;; Fixture 2: 2D float64 matrix [[1 2][3 4]]
(let ((data #2A((1.0d0 2.0d0) (3.0d0 4.0d0))))
  (write-binary (merge-pathnames "testdata/mat2x2_float64.sdat" *default-pathname-defaults*)
                data)
  (format t "Wrote testdata/mat2x2_float64.sdat~%"))

;; Fixture 3: 1D with special values (zero, negative)
(let ((data (coerce #(0.0d0 -1.5d0 3.14d0) 'vector)))
  (write-binary (merge-pathnames "testdata/vec_special.sdat" *default-pathname-defaults*)
                data)
  (format t "Wrote testdata/vec_special.sdat~%"))

;; Fixture 4: Longer 1D for shape/size tests
(let ((data (coerce (loop for i from 0 to 99 collect (float i 1.0d0)) 'vector)))
  (write-binary (merge-pathnames "testdata/vec100.sdat" *default-pathname-defaults*)
                data)
  (format t "Wrote testdata/vec100.sdat~%"))

(format t "Done. Fixtures in testdata/~%")
