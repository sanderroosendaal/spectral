;; Test beyond-literals: array literals with variable references
(load "spectral.lisp")
(in-package :spectral)

(format t "~%--- Test 1: [1 2 x] with x=5 ---~%")
(evaluate "x = 5")
(evaluate "[1 2 x]")
(pretty-print-stack)

(format t "~%--- Test 2: [x y] with x=5, y=10 ---~%")
(evaluate "y = 10")
(evaluate "[x y]")
(pretty-print-stack)

(format t "~%--- Test 3: [[1 2 x][3 4 y]] ---~%")
(evaluate "[[1 2 x][3 4 y]]")
(pretty-print-stack)

(format t "~%--- Test 4: [range 5] ---~%")
(evaluate "[range 5]")
(pretty-print-stack)

(format t "~%--- Test 5: [A B] with A=[1 2], B=[3 4] ---~%")
(evaluate "A = [1 2]")
(evaluate "B = [3 4]")
(evaluate "[A B]")
(pretty-print-stack)

(format t "~%All tests completed.~%")
