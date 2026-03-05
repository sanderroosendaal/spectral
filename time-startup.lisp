;; Time Spectral startup. Run: sbcl --noinform --non-interactive --load time-startup.lisp
(let ((script-dir (make-pathname :defaults (truename *load-truename*)
                                :name nil :type nil)))
  (setf *default-pathname-defaults* script-dir))

(let ((start (get-internal-real-time)))
  (load "spectral.lisp")
  (format t "~&Startup: ~,3f sec~%"
          (/ (- (get-internal-real-time) start)
             internal-time-units-per-second)))
