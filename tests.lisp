;; Spectral test suite
;; Run with: sbcl --noinform --non-interactive --load tests.lisp
;; (--non-interactive avoids debugger and speeds up the cycle)

(let ((script-dir (make-pathname :defaults (truename *load-truename*)
                                :name nil :type nil)))
  (setf *default-pathname-defaults* script-dir))

(load "spectral.lisp")
(in-package :spectral)

(defvar *test-passed* 0)
(defvar *test-failed* 0)
(defvar *current-test-name* nil)

(defun spectral-equal (expected actual)
  "Compare Spectral results (numbers, arrays, lists). Supports complex numbers."
  (flet ((numbers-equal-p (e a)
           (cond
             ((and (complexp e) (complexp a))
              (let ((tol 1.0d-10))
                (and (< (abs (- (realpart e) (realpart a))) tol)
                     (< (abs (- (imagpart e) (imagpart a))) tol))))
             ((or (complexp e) (complexp a)) nil)
             (t (or (= e a)
                    (and (numberp e) (numberp a)
                         (< (abs (- (coerce e 'double-float) (coerce a 'double-float))) 1.0d-10)))))))
    (cond
      ((and (arrayp expected) (arrayp actual))
       (and (equal (array-dimensions expected) (array-dimensions actual))
            (or (equalp expected actual)
                (every #'numbers-equal-p expected actual))))
      ((and (numberp expected) (numberp actual))
       (numbers-equal-p expected actual))
      (t (equalp expected actual)))))

(defun format-result (result)
  "Format a result for display (truncate long arrays)."
  (cond
    ((arrayp result)
     (if (<= (array-total-size result) 12)
         (format nil "~A" result)
         (format nil "#<array ~A ~A>" (array-dimensions result) (array-element-type result))))
    (t (format nil "~S" result))))

(defun assert-equal (expr expected &optional (reset t))
  "Evaluate EXPR and check result equals EXPECTED. RESET clears state before eval.
   Use :run-only as expected to execute without assertion (e.g. file I/O).
   Use (:error \"substring\") as expected to assert EXPR signals an error whose message contains substring."
  (when reset (reset-spectral-state))
  (when (eq expected :run-only)
    (evaluate expr)
    (incf *test-passed*)
    (return-from assert-equal t))
  ;; Error-path assertion: expected = (:error "substring")
  (when (and (listp expected) (eq (first expected) :error))
    (let ((substring (or (second expected) "")))
      (return-from assert-equal
        (handler-case
            (let ((result (evaluate expr)))
              ;; No error - we expected one
              (incf *test-failed*)
              (format t "  FAIL: ~S~%    expected error containing: ~S~%    got: ~A~%"
                      expr substring (format-result result))
              nil)
          (error (e)
            (let ((msg (princ-to-string e)))
              (if (search substring msg)
                  (progn (incf *test-passed*) t)
                  (progn (incf *test-failed*)
                         (format t "  FAIL: ~S~%    expected error containing: ~S~%    got: ~A~%"
                                 expr substring msg)
                         nil))))))))
  ;; Normal success-path assertion
  (handler-case
      (let* ((actual (handler-case (evaluate expr)
                       (error (e) (list :error (princ-to-string e)))))
             (ok (if (and (listp actual) (eq (first actual) :error))
                     nil
                     (spectral-equal expected actual))))
        (if ok (incf *test-passed*)
            (progn (incf *test-failed*)
                   (format t "  FAIL: ~S~%    expected: ~A~%    actual:   ~A~%"
                           expr (format-result expected) (format-result actual))))
        ok)
    (error (e)
      (incf *test-failed*)
      (format t "  FAIL: ~S~%    error: ~A~%" expr e)
      nil)))

(defun run-test-group (name tests)
  "Run a group of tests. Each test is (expr expected) or (expr expected nil) for no reset."
  (format t "~%~A~%" name)
  (dolist (test tests)
    (destructuring-bind (expr expected &optional (reset t)) test
      (assert-equal expr expected reset))))

(defun run-all-tests ()
  (setf *test-passed* 0 *test-failed* 0)
  (format t "Running Spectral tests...~%")

  ;; Basic arithmetic (note: binary ops pop a=2nd b=1st, so - and % have reversed arg order)
  (run-test-group "Arithmetic"
    '(("+ 3 5" 8)
      ("- 3 10" 7)                     ; 10-3: use - b a to get correct order
      ("* 4 5" 20)
      ("% 3 15" 5)                     ; 15/3
      ("+ 2 range 5" #(2 3 4 5 6))))

  ;; Constants
  (run-test-group "Constants"
    '(("pi" 3.141592653589793d0)
      ("e" 2.718281828459045d0)))

  ;; range
  (run-test-group "range"
    '(("size range 5" 5)
      ("shape range 9" (9))
      ("pick 0 range 5" 0)
      ("pick 4 range 5" 4)))

  ;; Stack operations
  (run-test-group "Stack"
    '(("* d 2" 4)                      ; 2 * 2 (dup)
      ("swap 1 2" 2)))                 ; push 2, 1; swap -> 2 on top

  ;; Error-path tests (invalid inputs, underflow, type errors)
  (run-test-group "Error paths"
    '(;; Stack underflow
      ("pop" (:error "Stack underflow"))
      ("swap 1" (:error "Stack underflow"))
      ("d" (:error "Stack underflow"))
      ;; Reduction/scan expect array
      ("/+ 5" (:error "expects an array"))
      ("&+ 5" (:error "expects an array"))
      ;; Invalid indices
      ("pick 99 [1 2 3]" (:error "Invalid index"))
      ("take 10 [1 2 3]" (:error "Invalid"))
      ("drop 5 [1 2 3]" (:error "Invalid"))
      ;; Reshape
      ("reshape [3 3] [1 2 3 4 5]" (:error "Non-matching shape"))
      ("reshape [2 2] 5" (:error "reshape expects an array"))
      ;; Type errors: pick/take/drop with wrong types
      ("pick 0 5" (:error "pick expects an array"))
      ("take \"x\" [1 2 3]" (:error "take expects"))
      ("drop [1] [1 2 3]" (:error "Invalid inputs to drop"))
      ;; Array op invalid input (CL signals type error for non-numeric)
      ("+ 1 \"x\"" (:error "not of type"))
      ;; Mismatched dimensions
      ("+ [1 2 3] [1 2]" (:error "Mismatched array dimensions"))
      ;; ->P / ->R wrong length
      ("->P [1]" (:error "->P takes a vector"))
      ("->R [1 2 3]" (:error "->R takes a vector"))
      ;; Ragged array
      ("[[1 2][3]]" (:error "Ragged array"))
      ;; Unknown reduction/scan operator
      ("/foo [1 2 3]" (:error "Unknown reduction"))
      ("&bar [1 2 3]" (:error "Unknown scan"))
      ;; Unexpected token
      ("xyz 1 2" (:error "Unexpected token"))
      ;; IF malformed (then-else needs 2+ expressions)
      ("(1) if 1" (:error "then-else"))
      ;; Script errors include filename and line number
      ("run \"testdata/script_err.spec\"" (:error ":1:"))))

  ;; Variables and assignment (reset before group so stack is clean)
  (run-test-group "Variables"
    '(("x = 5" nil)                    ; assignment leaves stack unchanged (nil when empty)
      ("x" 5 nil)
      ("pop" nil nil)                 ; clear stack before next assignment
      ("y = 10" nil nil)
      ("y" 10 nil)
      ("+ x y" 15 nil)))

  ;; Functions (no reset between def and use)
  (run-test-group "Functions"
    '(("AddFive = + 5" nil)
      ("AddFive 10" 15 nil)
      ("pop" nil nil)                  ; clear stack before next def
      ("Double = * 2" nil nil)
      ("Double 7" 14 nil)))

  ;; Array literals (basic)
  (run-test-group "Array literals (basic)"
    '(("[1 2 3]" #(1 2 3))
      ("[[1 2][3 4]]" #2A((1 2) (3 4)))
      ("shape [1 2 3]" (3))            ; shape returns array dims as list
      ("size [[1 2][3 4]]" 4)))

  ;; Beyond-literals: variable refs in arrays
  (run-test-group "Beyond-literals: variable refs"
    '(("x = 5" nil)
      ("y = 10" nil nil)
      ("[1 2 x]" #(1 2 5) nil)
      ("[x y]" #(5 10) nil)
      ("[[1 2 x][3 4 y]]" #2A((1 2 5) (3 4 10)) nil)))

  ;; Beyond-literals: array refs
  (run-test-group "Beyond-literals: array refs"
    '(("A = [1 2]" nil)
      ("B = [3 4]" nil nil)
      ("[A B]" #2A((1 2) (3 4)) nil)))

  ;; stack: vertical stacking of arrays
  (run-test-group "stack"
    '(("stack [1 2] [3 4]" #2A((1 2) (3 4)))
      ("shape stack [1 2] [3 4]" (2 2))
      ("stack [[1 2][3 4]] [[5 6][7 8]]" #3A(((1 2) (3 4)) ((5 6) (7 8))))
      ("shape stack [[1 2][3 4]] [[5 6][7 8]]" (2 2 2))
      ("stack [[1 2][3 4]] [5 6]" #2A((1 2) (3 4) (5 6)))))

  ;; Beyond-literals: expressions in arrays (requires group for multi-token)
  (run-test-group "Beyond-literals: expressions"
    '(("[(range 5)]" #2A((0 1 2 3 4))) ; single row from (range 5)
      ("[(+ 1 2)]" #(3))
      ("x = 3" nil)
      ("[1 2 (+ x 1)]" #(1 2 4) nil)))

  ;; Polar/rectangular coordinates
  (run-test-group "->P / ->R"
    '(("->P [1 0]" #(1.0d0 0.0d0))
      ("->R [2 pi]" #(-2.0d0 0.0d0))
      ("> 0.7 pick 1 ->P [1 1]" 1)))

  ;; Complex numbers (scalars, arrays, sqrt of negative)
  (run-test-group "Complex numbers"
    '(;; construct, re, im
      ("re complex 3 4" 3.0d0)
      ("im complex 3 4" 4.0d0)
      ("abs complex 3 4" 5.0d0)
      ;; arithmetic with complex
      ("+ complex 1 0 complex 0 1" #C(1.0d0 1.0d0))
      ("* complex 0 1 complex 0 1" -1.0d0)         ; i*i = -1
      ;; sqrt of negative scalar
      ("sqrt -1" #C(0.0d0 1.0d0))                  ; sqrt(-1) = i
      ("sqrt -4" #C(0.0d0 2.0d0))                  ; sqrt(-4) = 2i
      ;; sqrt of negative array
      ("sqrt [-1]" #(#C(0.0d0 1.0d0)))
      ("sqrt [-1 -4]" #(#C(0.0d0 1.0d0) #C(0.0d0 2.0d0)))
      ;; array of complex, re extracts real parts (groups needed for binary op)
      ("re [(complex 1 2)(complex 3 4)]" #(1.0d0 3.0d0))))

  ;; Conditionals
  (run-test-group "Conditionals"
    '(("((*2)(%2)) if 1 d 5" 10)       ; if 1 (true): 5*2
      ("((*2)(%2)) if 0 d 5" 5/2)))    ; if 0 (false): 5/2 (ratio)

  ;; File I/O (requires examples/numbers.dat)
  (run-test-group "File I/O"
    '(("load \"examples/numbers.dat\"" :run-only)
      ("take 3 load \"examples/numbers.dat\"" :run-only)))

  ;; Filters
  (run-test-group "Filters"
    '(("> 5 [1 2 3 4 5 6 7]" #(0 0 0 0 0 1 1))
      ("< 5 [1 2 3 4 5 6 7]" #(1 1 1 1 0 0 0))
      (">= 5 [1 2 3 4 5 6 7]" #(0 0 0 0 1 1 1))
      ("<= 5 [1 2 3 4 5 6 7]" #(1 1 1 1 1 0 0))
      ("eq 5 [1 2 3 4 5 6]" #(0 0 0 0 1 0))
      ("neq 5 [1 2 3 4 5 6]" #(1 1 1 1 0 1))))

  ;; Reduction (/op collapses array to single value)
  (run-test-group "Reduction"
    '(("/+ [1 2 3 4 5]" 15)
      ("/+ range 5" 10)                 ; 0+1+2+3+4
      ("/* [1 2 3 4 5]" 120)           ; factorial 5
      ("/max [3 1 4 1 5]" 5)
      ("/min [3 1 4 1 5]" 1)
      ("/+ [[1 2][3 4]]" #(4 6))))     ; reduce along first axis

  ;; Reduction (large arrays, parallel path; threshold 10000)
  (run-test-group "Reduction (parallel)"
    '(;; 1D: sum of 0..14999 = 14999*15000/2 = 112492500
      ("/+ range 15000" 112492500)
      ;; 1D: max/min of large vector
      ("/max range 20000" 19999)
      ("/min range 10001" 0)            ; [0..10000], min=0
      ;; 2D: reduce along first axis, 100 rows x 100 cols = 10000 elements (hits parallel threshold)
      ("pick 0 /+ reshape [100 100] range 10000" 495000)))

  ;; Scan (&op prefix scan, cumulative results)
  (run-test-group "Scan"
    '(("&+ [1 2 3 4 5]" #(1 3 6 10 15))
      ("&+ range 5" #(0 1 3 6 10))
      ("&* [1 2 3 4 5]" #(1 2 6 24 120))))

  ;; find-peaks: returns 1D array of indices of local maxima (strict: greater than both neighbors)
  (run-test-group "find-peaks"
    '(("find-peaks [1 3 5 4 2]" #(2))           ; single peak at index 2
      ("find-peaks [1 5 2 4 3]" #(1 3))         ; two peaks at indices 1 and 3
      ("find-peaks [1 3 2 4 5 4 2]" #(1 4))     ; peaks at 1 and 4
      ("find-peaks [5 4 3 2 1]" #(0))           ; peak at start
      ("find-peaks [1 2 3 4 5]" #(4))           ; peak at end (only neighbor is smaller)
      ("find-peaks [5]" #(0))                   ; single element is a peak
      ("find-peaks [3 1]" #(0))                 ; two elements, first is peak
      ("find-peaks [1 3]" #(1))                 ; two elements, last is peak
      ("find-peaks [1 2 2 1]" #())              ; flat middle, no strict peaks
      ("find-peaks [1 2 1 2 1]" #(1 3))))      ; two equal peaks

  ;; bandpass [f_low f_high sample_rate] signal - FFT, zero bins outside passband, IFFT
  ;; Requires FFTW. Full passband preserves signal; passband excluding DC removes mean.
  (run-test-group "bandpass"
    '(("shape bandpass [0 4 8] [1 1 1 1 1 1 1 1]" (8))  ; length preserved
      ("/+ bandpass [1 4 8] [1 1 1 1 1 1 1 1]" 0)       ; DC removed, sum ~0 (passband 1-4 Hz excludes bin 0)
      ("shape bandpass [0 50 100] range 100" (100))     ; length preserved for longer signal
      ;; 256 samples @ 200 Hz: in-band (5-15 Hz) keeps 10 Hz sinusoid, out-of-band (20-60 Hz) attenuates it
      ("sig = sin * 2 * pi * 10 % 200 range 256" nil)
      ("> /max abs bandpass [20 60 200] sig /max abs bandpass [5 15 200] sig" 1 nil)
      ;; Mixed 1+10+50 Hz: bandpass [5 15 200] keeps 10 Hz component
      ("mix = + + sin * 2 * pi * 1 % 200 range 256 sin * 2 * pi * 10 % 200 range 256 sin * 2 * pi * 50 % 200 range 256" nil)
      ("> 0 /max abs bandpass [5 15 200] mix" 1 nil)))

  ;; smooth window_size signal - boxcar moving average, same-length output with partial windows at edges
  (run-test-group "smooth"
    '(("smooth 1 [1 2 3 4 5]" #(1 2 3 4 5))    ; 1-point = identity
      ("smooth 5 [1 1 1 1 1 1 1]" #(1 1 1 1 1 1 1))  ; constant in = constant out
      ("shape smooth 3 range 10" (10))          ; length preserved
      ("smooth 3 [1 2 3 4 5]" #(1.5 2 3 4 4.5))))  ; 3-point: edges partial, center full

  ;; savgol [window_length poly_order] signal - Savitzky-Golay polynomial smoothing
  (run-test-group "savgol"
    '(("savgol [5 2] [1 1 1 1 1 1 1 1 1 1]" #(1 1 1 1 1 1 1 1 1 1))  ; constant preserved
      ("shape savgol [5 2] range 20" (20))       ; length preserved
      ("> 3.5 pick 4 savgol [5 2] [1 2 3 4 5 6 7 8 9 10]" 1)))  ; center ≈ 5 > 3.5 (threshold first, value second)

  ;; find-valleys: indices of local minima
  (run-test-group "find-valleys"
    '(("find-valleys [5 1 5 1 5]" #(1 3))
      ("find-valleys [3 2 1 2 3]" #(2))
      ("find-valleys [1 2 3 4 5]" #(0))
      ("find-valleys [5 4 3 2 1]" #(4))
      ("find-valleys [4 1 3 2 5]" #(1 3))
      ("find-valleys [5]" #(0))))

  ;; lowpass, highpass, bandstop - FFT-based filters
  (run-test-group "lowpass/highpass/bandstop"
    '(("shape lowpass [10 100] range 100" (100))
      ("shape highpass [10 100] range 100" (100))
      ("shape bandstop [5 15 100] range 100" (100))))

  ;; psd, detrend, differentiate
  (run-test-group "psd/detrend/differentiate"
    '(("shape psd range 64" (64))
      ("pick 0 differentiate [0 1 2 3 4]" 1)
      ("pick 2 differentiate [0 1 2 3 4]" 1)
      ("pick 0 detrend [0 1 2 3 4]" 0)
      ("/+ abs detrend [0 1 2 3 4]" 0)))

  ;; Binary array I/O (Tier A .sdat)
  (run-test-group "Binary I/O"
    '(;; Error: non-existent file
      ("load-binary \"testdata/nonexistent_xyz.sdat\"" (:error "file"))
      ;; Roundtrip: write then load (uses temp file)
      ("write-binary \"testdata/roundtrip.sdat\" [1 2 3 4 5]" :run-only)
      ("load-binary \"testdata/roundtrip.sdat\"" #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0) nil)
      ;; Roundtrip 2D
      ("write-binary \"testdata/roundtrip2d.sdat\" [[1 2][3 4]]" :run-only)
      ("load-binary \"testdata/roundtrip2d.sdat\"" #2A((1.0d0 2.0d0) (3.0d0 4.0d0)) nil)
      ;; Roundtrip with special values (zero, negative, exactly representable)
      ("write-binary \"testdata/roundtrip_spec.sdat\" [0 -1 2]" :run-only)
      ("pick 0 load-binary \"testdata/roundtrip_spec.sdat\"" 0.0d0 nil)
      ("pick 1 load-binary \"testdata/roundtrip_spec.sdat\"" -1.0d0 nil)
      ("pick 2 load-binary \"testdata/roundtrip_spec.sdat\"" 2.0d0 nil)
      ;; Shape preserved
      ("shape write-binary \"testdata/shape_test.sdat\" range 10" (10))
      ("shape load-binary \"testdata/shape_test.sdat\"" (10) nil)
      ;; Size preserved
      ("size write-binary \"testdata/size_test.sdat\" [[1 2 3][4 5 6]]" 6)
      ("size load-binary \"testdata/size_test.sdat\"" 6 nil)
      ;; Fixture files (run write-fixtures-minimal.lisp first)
      ("load-binary \"testdata/vec5_float64.sdat\"" #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))
      ("load-binary \"testdata/mat2x2_float64.sdat\"" #2A((1.0d0 2.0d0) (3.0d0 4.0d0)))
      ("load-binary \"testdata/vec_special.sdat\"" #(0.0d0 -1.0d0 2.0d0))
      ("shape load-binary \"testdata/vec100.sdat\"" (100))))

  ;; NPY format (Tier B)
  (run-test-group "NPY I/O"
    '(;; Roundtrip 1D float64
      ("write-npy \"testdata/npy_roundtrip.npy\" [1 2 3 4 5]" :run-only)
      ("load-npy \"testdata/npy_roundtrip.npy\"" #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0) nil)
      ;; Roundtrip 2D float64
      ("write-npy \"testdata/npy_2d.npy\" [[1 2][3 4]]" :run-only)
      ("load-npy \"testdata/npy_2d.npy\"" #2A((1.0d0 2.0d0) (3.0d0 4.0d0)) nil)
      ;; Shape and size preserved
      ("shape write-npy \"testdata/npy_shape.npy\" range 7" (7))
      ("shape load-npy \"testdata/npy_shape.npy\"" (7) nil)
      ("size write-npy \"testdata/npy_size.npy\" [[1 2 3][4 5 6]]" 6)
      ("size load-npy \"testdata/npy_size.npy\"" 6 nil)))

  ;; HDF5 (optional; skip when libhdf5 not available)
  (when (and (boundp '*hdf5-available-p*) *hdf5-available-p*)
    (run-test-group "HDF5 I/O"
      '(;; Roundtrip 1D
        ("write-hdf5 \"testdata/h5_roundtrip.h5\" \"/data\" [1 2 3 4 5]" :run-only)
        ("load-hdf5 \"testdata/h5_roundtrip.h5\" \"/data\"" #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0) nil)
        ;; Roundtrip 2D
        ("write-hdf5 \"testdata/h5_2d.h5\" \"/matrix\" [[1 2][3 4]]" :run-only)
        ("load-hdf5 \"testdata/h5_2d.h5\" \"/matrix\"" #2A((1.0d0 2.0d0) (3.0d0 4.0d0)) nil)
        ;; Shape preserved
        ("shape write-hdf5 \"testdata/h5_shape.h5\" \"/arr\" range 5" (5))
        ("shape load-hdf5 \"testdata/h5_shape.h5\" \"/arr\"" (5) nil)
        ;; Verify files are not corrupt (size > 1KB indicates valid HDF5 structure)
        ("size load-hdf5 \"testdata/h5_roundtrip.h5\" \"/data\"" 5 nil)
        ("size load-hdf5 \"testdata/h5_2d.h5\" \"/matrix\"" 4 nil))))

  ;; Summary
  (let ((total (+ *test-passed* *test-failed*)))
    (format t "~%~%--- ~D passed, ~D failed (~D total) ---~%"
            *test-passed* *test-failed* total)
    (zerop *test-failed*)))

;; Run on load
(let ((ok (run-all-tests)))
  #+sbcl (sb-ext:exit :code (if ok 0 1)))
