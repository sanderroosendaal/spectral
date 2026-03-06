;;;; Run Spectral tests non-interactively.
;;;; Usage: sbcl --noinform --non-interactive --load tests.lisp
;;;;        sbcl --noinform --script run-tests.lisp  (if Quicklisp is at ~/quicklisp)
(let ((script-dir (make-pathname :defaults (truename *load-truename*)
                                :name nil :type nil)))
  (setf *default-pathname-defaults* script-dir))
(unless (find-package :ql)
    (let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (if (probe-file ql-setup)
        (load ql-setup)
        (error "Quicklisp not found. Use: sbcl --noinform --non-interactive --load tests.lisp"))))
(load "tests.lisp")
