;;; Debug HDF5 load on Windows — loads hdf5-ffi WITHOUT handler-case to see actual error
(require "asdf")
(ql:quickload :cffi)
(setf *default-pathname-defaults* (truename #P"."))
(format t "Loading hdf5-ffi directly...~%")
(load (merge-pathnames "std/hdf5-ffi.lisp" *default-pathname-defaults*))
(format t "HDF5 FFI loaded successfully!~%")
