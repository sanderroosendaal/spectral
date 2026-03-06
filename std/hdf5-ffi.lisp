;;; Minimal CFFI bindings for HDF5 — compatible with HDF5 1.14 and 2.0.
;;; No dependency on hdf5-cffi. Loaded only when HDF5 support is requested.
;;; Defines package :hdf5 with the symbols needed by hdf5-io.lisp.

(defpackage :hdf5
  (:use :cl :cffi)
  (:export
   #:+H5F-ACC-RDONLY+ #:+H5F-ACC-TRUNC+
   #:+H5T-NATIVE-DOUBLE+ #:+H5S-ALL+ #:+H5P-DEFAULT+
   #:H5open #:H5Fopen #:H5Fcreate #:H5Fclose
   #:H5Dopen2 #:H5Dcreate2 #:H5Dget-space #:H5Dread #:H5Dwrite #:H5Dclose
   #:H5Sget-simple-extent-ndims #:H5Sget-simple-extent-dims
   #:H5Screate-simple #:H5Sclose))

(in-package :hdf5)

;;; Load libhdf5 — platform-specific names
;;; Windows: hdf5.dll (HDF Group installer puts it in …/HDF5/x.x.x/bin)
(cffi:define-foreign-library libhdf5
  (:darwin (:or "libhdf5.dylib" "libhdf5.10.dylib" "libhdf5.200.dylib"))
  (:unix (:or "libhdf5.so" "libhdf5.so.200" "libhdf5.so.10" "libhdf5.so.0"))
  (:windows (:or "hdf5.dll" "libhdf5.dll"))
  (t (:default "libhdf5")))

;;; On Windows, add HDF5 bin to search path if not in PATH.
;;; Set HDF5_DIR or HDF5_ROOT, or we scan C:/Program Files/HDF_Group/HDF5.
#+(or windows win32 os-windows)
(eval-when (:load-toplevel)
  (ignore-errors
    (flet ((add (d)
             (when (and d (uiop:directory-exists-p d))
               (pushnew d cffi:*foreign-library-directories* :test #'equal))))
      (when (uiop:getenv "HDF5_DIR")
        (add (uiop:merge-pathnames* "bin/" (uiop:parse-native-namestring (uiop:getenv "HDF5_DIR")))))
      (when (uiop:getenv "HDF5_ROOT")
        (add (uiop:merge-pathnames* "bin/" (uiop:parse-native-namestring (uiop:getenv "HDF5_ROOT")))))
      (block nil
        (dolist (p (list "C:/Program Files/HDF_Group/HDF5" "C:/Program Files (x86)/HDF_Group/HDF5"))
          (let ((dir (ignore-errors (uiop:parse-native-namestring p))))
            (when (and dir (uiop:directory-exists-p dir))
              (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                (let ((bin (uiop:merge-pathnames* "bin/" sub)))
                  (when (uiop:directory-exists-p bin)
                    (add bin)
                    (return)))))))))))

(cffi:use-foreign-library libhdf5)

;;; C types (hid_t is int64 in HDF5)
(cffi:defctype hid-t :long)
(cffi:defctype hsize-t :unsigned-long)
(cffi:defctype herr-t :int)

;;; Constants from HDF5 headers
(defconstant +H5P-DEFAULT+ 0)
(defconstant +H5S-ALL+ 0)
(defconstant +H5F-ACC-RDONLY+ 0)
(defconstant +H5F-ACC-TRUNC+ 2)

;;; H5open — call once so H5T_NATIVE_DOUBLE_g is valid
(cffi:defcfun ("H5open" h5open) herr-t)

;;; H5T_NATIVE_DOUBLE — foreign variable (valid after H5open)
(cffi:defcvar ("H5T_NATIVE_DOUBLE_g" +H5T-NATIVE-DOUBLE+) :long)

;;; File
(cffi:defcfun ("H5Fopen" H5Fopen) hid-t
  (name :string) (flags :unsigned-int) (fapl-id hid-t))
(cffi:defcfun ("H5Fcreate" H5Fcreate) hid-t
  (name :string) (flags :unsigned-int) (fcpl-id hid-t) (fapl-id hid-t))
(cffi:defcfun ("H5Fclose" H5Fclose) herr-t (file-id hid-t))

;;; Dataset
(cffi:defcfun ("H5Dopen2" H5Dopen2) hid-t
  (loc-id hid-t) (name :string) (dapl-id hid-t))
(cffi:defcfun ("H5Dcreate2" H5Dcreate2) hid-t
  (loc-id hid-t) (name :string) (type-id hid-t) (space-id hid-t)
  (dcpl-id hid-t) (dapl-id hid-t))
(cffi:defcfun ("H5Dget_space" H5Dget-space) hid-t (dset-id hid-t))
(cffi:defcfun ("H5Dread" H5Dread) herr-t
  (dset-id hid-t) (mem-type-id hid-t) (mem-space-id hid-t)
  (file-space-id hid-t) (xfer-plist-id hid-t) (buf :pointer))
(cffi:defcfun ("H5Dwrite" H5Dwrite) herr-t
  (dset-id hid-t) (mem-type-id hid-t) (mem-space-id hid-t)
  (file-space-id hid-t) (xfer-plist-id hid-t) (buf :pointer))
(cffi:defcfun ("H5Dclose" H5Dclose) herr-t (dset-id hid-t))

;;; Dataspace
(cffi:defcfun ("H5Sget_simple_extent_ndims" H5Sget-simple-extent-ndims) :int
  (space-id hid-t))
(cffi:defcfun ("H5Sget_simple_extent_dims" H5Sget-simple-extent-dims) :int
  (space-id hid-t) (dims :pointer) (maxdims :pointer))
(cffi:defcfun ("H5Screate_simple" H5Screate-simple) hid-t
  (rank :int) (dims :pointer) (maxdims :pointer))
(cffi:defcfun ("H5Sclose" H5Sclose) herr-t (space-id hid-t))

;;; Initialize library at load time (H5T_NATIVE_DOUBLE_g populated by H5open)
(let ((err (h5open)))
  (when (< err 0)
    (error "HDF5: H5open failed with code ~A" err)))
