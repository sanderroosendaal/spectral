(ql:quickload :gurafu)

(defun plot-fn (y)
  (plot:plot
   (vega:defplot :simple-line-plot
   `(:title "ΣpectraΛ"
     :data (:values ,y)
     :mark :line
  ))))
