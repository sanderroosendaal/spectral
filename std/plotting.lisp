(ql:quickload :vgplot)

(defun plot-fn (y)
  (vgplot:plot
   (loop for x from 0 below (length y) collect x)
   (loop for x from 0 below (length y) collect (aref y x)))
  y)

(register-op 'plot #'plot-fn 1)
