;; Spectral test suite
;; Run with: sbcl --noinform --load tests.lisp

(let ((script-dir (make-pathname :defaults (truename *load-truename*)
                                :name nil :type nil)))
  (setf *default-pathname-defaults* script-dir))

(load "spectral.lisp")
(in-package :spectral)

(defvar *test-passed* 0)
(defvar *test-failed* 0)
(defvar *current-test-name* nil)

(defun spectral-equal (expected actual)
  "Compare Spectral results (numbers, arrays, lists)."
  (cond
    ((and (arrayp expected) (arrayp actual))
     (and (equal (array-dimensions expected) (array-dimensions actual))
          (equalp expected actual)))
    ((and (numberp expected) (numberp actual))
     (or (= expected actual)
         (and (floatp expected) (floatp actual)
              (< (abs (- expected actual)) 1.0d-10))))
    (t (equalp expected actual))))

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
   Use :run-only as expected to execute without assertion (e.g. file I/O)."
  (when reset (reset-spectral-state))
  (when (eq expected :run-only)
    (evaluate expr)
    (incf *test-passed*)
    (return-from assert-equal t))
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

  ;; Stack operations
  (run-test-group "Stack"
    '(("* d 2" 4)                      ; 2 * 2 (dup)
      ("swap 1 2" 2)))                 ; push 2, 1; swap -> 2 on top

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

  ;; Beyond-literals: expressions in arrays (requires group for multi-token)
  (run-test-group "Beyond-literals: expressions"
    '(("[(range 5)]" #2A((0 1 2 3 4))) ; single row from (range 5)
      ("[(+ 1 2)]" #(3))
      ("x = 3" nil)
      ("[1 2 (+ x 1)]" #(1 2 4) nil)))

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
      ("eq 5 [1 2 3 4 5 6]" #(0 0 0 0 1 0))))

  ;; Reduction (/op collapses array to single value)
  (run-test-group "Reduction"
    '(("/+ [1 2 3 4 5]" 15)
      ("/+ range 5" 10)                 ; 0+1+2+3+4
      ("/* [1 2 3 4 5]" 120)           ; factorial 5
      ("/max [3 1 4 1 5]" 5)
      ("/min [3 1 4 1 5]" 1)
      ("/+ [[1 2][3 4]]" #(4 6))))     ; reduce along first axis

  ;; Scan (&op prefix scan, cumulative results)
  (run-test-group "Scan"
    '(("&+ [1 2 3 4 5]" #(1 3 6 10 15))
      ("&+ range 5" #(0 1 3 6 10))
      ("&* [1 2 3 4 5]" #(1 2 6 24 120))))

  ;; Summary
  (let ((total (+ *test-passed* *test-failed*)))
    (format t "~%~%--- ~D passed, ~D failed (~D total) ---~%"
            *test-passed* *test-failed* total)
    (zerop *test-failed*)))

;; Run on load
(let ((ok (run-all-tests)))
  #+sbcl (sb-ext:exit :code (if ok 0 1)))
