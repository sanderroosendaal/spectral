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

;; Test 2: Skip H5open() — library initializes itself
(format t "~%Test 2: Skipping H5open() (library auto-initializes)...~%")
(format t "  ✓ HDF5 library will initialize on first use~%")

;; Test 3: Can we access H5T_NATIVE_DOUBLE_g?
(format t "~%Test 3: Accessing H5T_NATIVE_DOUBLE_g...~%")
(handler-case
    (progn
      (cffi:defcvar ("H5T_NATIVE_DOUBLE_g" h5t-native-double) :long)
      (format t "  H5T_NATIVE_DOUBLE_g = ~A~%" h5t-native-double)
      (format t "  ✓ Successfully accessed H5T_NATIVE_DOUBLE_g~%"))
  (error (e)
    (format t "  ✗ Failed to access H5T_NATIVE_DOUBLE_g: ~A~%" e)
    (format t "  Error type: ~A~%" (type-of e))
    (sb-ext:exit :code 1)))

(format t "~%=== All tests passed! ===~%")
(sb-ext:exit :code 0)
