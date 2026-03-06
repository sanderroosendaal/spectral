;;; Minimal CFFI bindings for HDF5 — compatible with HDF5 1.14 and 2.0.
;;; No dependency on hdf5-cffi. Loaded only when HDF5 support is requested.
;;; Defines package :hdf5 with the symbols needed by hdf5-io.lisp.

(defpackage :hdf5
  (:use :cl :cffi)
  (:export
   #:+H5F-ACC-RDONLY+ #:+H5F-ACC-TRUNC+
   #:get-h5t-native-double #:+H5S-ALL+ #:+H5P-DEFAULT+
   #:H5open #:H5Fopen #:H5Fcreate #:H5Fclose
   #:H5Dopen2 #:H5Dcreate2 #:H5Dget-space #:H5Dread #:H5Dwrite #:H5Dclose
   #:H5Sget-simple-extent-ndims #:H5Sget-simple-extent-dims
   #:H5Screate-simple #:H5Sclose))

(in-package :hdf5)

;;; Load libhdf5 — platform-specific names
;;; Windows: hdf5.dll (HDF Group installer puts it in …/HDF5/x.x.x/bin)
;;; On Linux, libhdf5.so may be a linker script; versioned .so has the actual symbols.
;;; H5T_NATIVE_DOUBLE_g is defined in libhdf5.so.320 (HDF5 3.x) or libhdf5.so.200 etc.
(cffi:define-foreign-library libhdf5
  (:darwin (:or "libhdf5.dylib" "libhdf5.10.dylib" "libhdf5.200.dylib"))
  (:unix (:or "libhdf5.so.320" "libhdf5.so.320.0.0"
              "libhdf5.so.200" "libhdf5.so.10" "libhdf5.so.0" "libhdf5.so"))
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

;;; H5open — NOT called at load time (causes FLOATING-POINT-INVALID-OPERATION on some systems)
;;; The library initializes itself on first use.
(cffi:defcfun ("H5open" h5open) herr-t)

;;; H5T_NATIVE_DOUBLE — type ID for native double. On Linux, H5T_NATIVE_DOUBLE_g
;;; can be "U" (undefined) in the main libhdf5.so; it lives in a companion object.
;;; We resolve it at runtime via foreign-symbol-pointer (global lookup) instead of
;;; defcvar, and fall back to H5T_IEEE_F64LE_g on little-endian platforms.
(defvar *h5t-native-double-cache* nil
  "Cached H5T_NATIVE_DOUBLE_g value, or :unavailable if lookup failed.")

(defun get-h5t-native-double ()
  "Return H5T_NATIVE_DOUBLE type ID. Calls H5open on first use; uses
foreign-symbol-pointer for global lookup (works when symbol is in a dependency).
Requires H5open to succeed before using type globals — otherwise they are uninitialized
and produce corrupt files."
  (when (eq *h5t-native-double-cache* :unavailable)
    (return-from get-h5t-native-double 0))
  (when *h5t-native-double-cache*
    (return-from get-h5t-native-double *h5t-native-double-cache*))
  ;; H5open must succeed — H5T_NATIVE_DOUBLE_g is only valid after successful init
  (let ((err (ignore-errors (h5open))))
    (when (or (null err) (and (integerp err) (< err 0)))
      (setf *h5t-native-double-cache* :unavailable)
      (when (integerp err)
        (warn "HDF5: H5open failed (code ~A); H5T_NATIVE_DOUBLE_g uninitialized. HDF5 I/O disabled." err))
      (return-from get-h5t-native-double 0)))
  (flet ((try-symbol (name)
           (let ((ptr (cffi:foreign-symbol-pointer name)))
             (when ptr
               (cffi:mem-ref ptr :long)))))
    (let ((val (or (try-symbol "H5T_NATIVE_DOUBLE_g")
                   ;; Fallback: H5T_IEEE_F64LE is equivalent to native double on x86/ARM LE
                   (try-symbol "H5T_IEEE_F64LE_g"))))
      (cond ((and val (not (zerop val)))
             (setf *h5t-native-double-cache* val))
            (t
             (setf *h5t-native-double-cache* :unavailable)
             (warn "Could not resolve H5T_NATIVE_DOUBLE_g or H5T_IEEE_F64LE_g (HDF5 I/O disabled)")
             (setq val 0)))
      val)))

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

;;; NOTE: H5open() is called on first get-h5t-native-double(). H5T_NATIVE_DOUBLE_g
;;; is resolved via foreign-symbol-pointer (global lookup) so it works when the
;;; symbol lives in a dependency rather than the main libhdf5.so.
