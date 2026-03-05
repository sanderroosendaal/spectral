;;; Spectral dependencies — load before other std libs.
;;; Centralizes all ql:quickload so loading is ordered and predictable.

(handler-case (ql:quickload :named-readtables) (error () nil))
(ql:quickload :cl-ansi-text)
(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :cl-csv)
(ql:quickload :array-operations)
(ql:quickload :cffi)
(ql:quickload :vgplot)

;; Optional (can fail if libs not installed)
(handler-case (ql:quickload :magicl) (error () nil))
;; hdf5-cffi loaded in io.lisp handler-case (needs to succeed before setting *hdf5-available-p*)
