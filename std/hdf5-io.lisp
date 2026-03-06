;;; HDF5 I/O — loaded only when hdf5-ffi is available
;;; Uses :hdf5 package from std/hdf5-ffi.lisp (minimal CFFI binding)

(in-package :spectral)
(use-package :hdf5)

(defun load-hdf5-inner (filename path)
  "Load dataset at path from HDF5 file. Returns double-float array."
  ;; Ensure HDF5 library is initialized
  (ignore-errors (hdf5:h5open))
  (let ((type-id (hdf5:get-h5t-native-double)))
    (when (zerop type-id)
      (spectral-error "HDF5 type ID unavailable (H5T_NATIVE_DOUBLE_g unresolved); HDF5 I/O disabled")))
  (let ((abs-filename (namestring (merge-pathnames filename))))
    (let ((file-id (hdf5:H5Fopen abs-filename
                                 hdf5:+H5F-ACC-RDONLY+ hdf5:+H5P-DEFAULT+)))
      (when (< file-id 0)
        (spectral-error "HDF5: Cannot open file ~A for reading (absolute path: ~A)" filename abs-filename))
      (unwind-protect
           (let* ((dset-id (hdf5:H5Dopen2 file-id path hdf5:+H5P-DEFAULT+)))
             (when (< dset-id 0)
               (spectral-error "HDF5: Cannot open dataset ~A in file ~A" path filename))
             (let ((space-id (hdf5:H5Dget-space dset-id)))
               (when (< space-id 0)
                 (spectral-error "HDF5: Cannot get dataspace for dataset ~A" path))
               (unwind-protect
                    (let* ((rank (hdf5:H5Sget-simple-extent-ndims space-id)))
                      (when (< rank 0)
                        (spectral-error "HDF5: Cannot get rank of dataset ~A" path))
                      (cffi:with-foreign-objects ((dims :unsigned-long rank)
                                                  (maxdims :unsigned-long rank))
                        (let ((status (hdf5:H5Sget-simple-extent-dims space-id dims maxdims)))
                          (when (< status 0)
                            (spectral-error "HDF5: Cannot get dimensions of dataset ~A" path))
                          (let* ((dims-list (loop for i below rank
                                                  collect (cffi:mem-aref dims :unsigned-long i)))
                                 (total (reduce #'* dims-list))
                                 (arr (make-array dims-list
                                                  :element-type 'double-float))
                                 (buf (cffi:foreign-alloc :double :count total)))
                            (unwind-protect
                                 (progn
                                   (let ((read-status (hdf5:H5Dread dset-id
                                                                     (hdf5:get-h5t-native-double)
                                                                     hdf5:+H5S-ALL+ hdf5:+H5S-ALL+
                                                                     hdf5:+H5P-DEFAULT+ buf)))
                                     (when (< read-status 0)
                                       (spectral-error "HDF5: Failed to read dataset ~A" path)))
                                   (dotimes (i total)
                                     (setf (row-major-aref arr i)
                                           (cffi:mem-aref buf :double i))))
                              (cffi:foreign-free buf))
                            arr))))
                 (hdf5:H5Sclose space-id)
                 (hdf5:H5Dclose dset-id))))
        (hdf5:H5Fclose file-id)))))

(defun load-hdf5-fn (filename path)
  (load-hdf5-inner filename path))

(defun write-hdf5-inner (filename path data)
  "Write array to HDF5 file at path. Overwrites if exists."
  ;; Ensure HDF5 library is initialized
  (ignore-errors (hdf5:h5open))
  (let ((type-id (hdf5:get-h5t-native-double)))
    (when (zerop type-id)
      (spectral-error "HDF5 type ID unavailable (H5T_NATIVE_DOUBLE_g unresolved); HDF5 I/O disabled")))
  (ensure-directories-exist (make-pathname :defaults (merge-pathnames filename) :name nil :type nil))
  (unless (arrayp data)
    (spectral-error "Expected an array, got ~A" (type-of data)))
  (let* ((dims (array-dimensions data))
         (rank (length dims))
         (abs-filename (namestring (merge-pathnames filename))))
    ;; Allocate dims array that persists for H5Screate-simple call
    (cffi:with-foreign-object (dims-ptr :unsigned-long rank)
      (dotimes (i rank)
        (setf (cffi:mem-aref dims-ptr :unsigned-long i)
              (nth i dims)))
      (let ((space-id (hdf5:H5Screate-simple rank dims-ptr (cffi:null-pointer))))
        (when (< space-id 0)
          (spectral-error "HDF5: Failed to create dataspace for ~A" path))
        (unwind-protect
             (let ((file-id (hdf5:H5Fcreate abs-filename
                                            hdf5:+H5F-ACC-TRUNC+
                                            hdf5:+H5P-DEFAULT+ hdf5:+H5P-DEFAULT+)))
               (when (< file-id 0)
                 (spectral-error "HDF5: Failed to create file ~A (absolute path: ~A)" filename abs-filename))
               (unwind-protect
                    (let ((dset-id (hdf5:H5Dcreate2 file-id path
                                                    (hdf5:get-h5t-native-double)
                                                    space-id hdf5:+H5P-DEFAULT+
                                                    hdf5:+H5P-DEFAULT+)))
                      (when (< dset-id 0)
                        (spectral-error "HDF5: Failed to create dataset ~A in ~A" path filename))
                      (unwind-protect
                           (let* ((total (array-total-size data))
                                  (buf (cffi:foreign-alloc :double :count total)))
                             (unwind-protect
                                  (progn
                                    (dotimes (i total)
                                      (setf (cffi:mem-aref buf :double i)
                                            (coerce (row-major-aref data i)
                                                    'double-float)))
                                    (let ((status (hdf5:H5Dwrite dset-id
                                                                  (hdf5:get-h5t-native-double)
                                                                  hdf5:+H5S-ALL+ hdf5:+H5S-ALL+
                                                                  hdf5:+H5P-DEFAULT+ buf)))
                                      (when (< status 0)
                                        (spectral-error "HDF5: Failed to write dataset ~A" path))))
                               (cffi:foreign-free buf)))
                        (hdf5:H5Dclose dset-id)))
                 (hdf5:H5Fclose file-id)))
          (hdf5:H5Sclose space-id)))))
  data)

(defun write-hdf5-fn (filename path data)
  (write-hdf5-inner filename path data))
