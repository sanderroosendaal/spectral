;;; Minimal HDF5 FFI test script
;;; Run with: sbcl --noinform --non-interactive --load test-hdf5-ffi.lisp

(require "asdf")

;; Load CFFI
(asdf:load-system :cffi)

(format t "~&=== HDF5 FFI Isolation Test ===~%~%")

;; Test 1: Can we load libhdf5?
(format t "Test 1: Loading libhdf5...~%")
(handler-case
    (progn
      (cffi:define-foreign-library libhdf5
        (:darwin (:or "libhdf5.dylib" "libhdf5.10.dylib" "libhdf5.200.dylib"))
        (:unix (:or "libhdf5.so" "libhdf5.so.200" "libhdf5.so.10" "libhdf5.so.0"))
        (:windows (:or "hdf5.dll" "libhdf5.dll"))
        (t (:default "libhdf5")))
      (cffi:use-foreign-library libhdf5)
      (format t "  ✓ libhdf5 loaded successfully~%"))
  (error (e)
    (format t "  ✗ Failed to load libhdf5: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Test 2: Call H5open() first (required on Linux before H5T_NATIVE_DOUBLE_g is valid)
(format t "~%Test 2: Calling H5open()...~%")
(handler-case
    (progn
      (cffi:defcfun ("H5open" h5open) :int)
      (let ((err (h5open)))
        (if (< err 0)
            (error "H5open failed with code ~A" err)
            (format t "  ✓ H5open() succeeded~%")))
  (error (e)
    (format t "  ✗ H5open failed: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Test 3: Resolve H5T_NATIVE_DOUBLE_g via foreign-symbol-pointer (global lookup)
;; Uses same approach as hdf5-ffi.lisp — works when symbol is in a dependency.
(format t "~%Test 3: Resolving H5T_NATIVE_DOUBLE_g via foreign-symbol-pointer...~%")
(handler-case
    (let ((ptr (cffi:foreign-symbol-pointer "H5T_NATIVE_DOUBLE_g")))
      (if ptr
          (let ((val (cffi:mem-ref ptr :long)))
            (format t "  H5T_NATIVE_DOUBLE_g = ~A~%" val)
            (format t "  ✓ Successfully resolved H5T_NATIVE_DOUBLE_g~%"))
          (progn
            (format t "  ✗ foreign-symbol-pointer returned NIL (symbol not in loaded image)~%")
            (format t "  Trying H5T_IEEE_F64LE_g fallback...~%")
            (let ((ptr2 (cffi:foreign-symbol-pointer "H5T_IEEE_F64LE_g")))
              (if ptr2
                  (let ((val (cffi:mem-ref ptr2 :long)))
                    (format t "  H5T_IEEE_F64LE_g = ~A~%" val)
                    (format t "  ✓ Fallback H5T_IEEE_F64LE_g works~%"))
                  (progn
                    (format t "  ✗ Neither symbol resolvable~%")
                    (sb-ext:exit :code 1))))))
  (error (e)
    (format t "  ✗ Error: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "~%=== All tests passed! ===~%")
(sb-ext:exit :code 0)
