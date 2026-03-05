;;; HDF5 I/O — loaded only when hdf5-cffi is available
;;; Uses :hdf5 package from hdf5-cffi

(in-package :spectral)

(defun load-hdf5-inner (filename path)
  "Load dataset at path from HDF5 file. Returns double-float array."
  (let ((file-id (hdf5:H5Fopen (namestring (pathname filename))
                               hdf5:+H5F-ACC-RDONLY+ -1)))
    (unwind-protect
         (let* ((dset-id (hdf5:H5Dopen2 file-id path -1))
                (space-id (hdf5:H5Dget-space dset-id)))
           (unwind-protect
                (let* ((rank (hdf5:H5Sget-simple-extent-ndims space-id))
                       (dims (cffi:foreign-alloc :unsigned-long
                                                 :count (max 1 rank)))
                       (maxdims (cffi:foreign-alloc :unsigned-long
                                                   :count (max 1 rank))))
                  (unwind-protect
                       (progn
                         (hdf5:H5Sget-simple-extent-dims space-id dims maxdims)
                         (let* ((dims-list (loop for i below rank
                                                 collect (cffi:mem-aref dims :unsigned-long i)))
                                (total (reduce #'* dims-list))
                                (arr (make-array dims-list
                                                 :element-type 'double-float))
                                (buf (cffi:foreign-alloc :double :count total)))
                           (unwind-protect
                                (progn
                                  (hdf5:H5Dread dset-id
                                                hdf5:+H5T-NATIVE-DOUBLE+
                                                hdf5:+H5S-ALL+ hdf5:+H5S-ALL+
                                                -1 buf)
                                  (dotimes (i total)
                                    (setf (row-major-aref arr i)
                                          (cffi:mem-aref buf :double i))))
                             (cffi:foreign-free buf))
                           arr))
                    (cffi:foreign-free maxdims)
                    (cffi:foreign-free dims)))
             (hdf5:H5Sclose space-id)
             (hdf5:H5Dclose dset-id)))
      (hdf5:H5Fclose file-id))))

(defun load-hdf5-fn (path filename)
  (load-hdf5-inner filename path))

(defun write-hdf5-inner (filename path data)
  "Write array to HDF5 file at path. Overwrites if exists."
  (unless (arrayp data)
    (error "Expected an array, got ~A" (type-of data)))
  (let* ((dims (array-dimensions data))
         (rank (length dims))
         (space-id (cffi:with-foreign-object (dims-ptr :unsigned-long rank)
                    (dotimes (i rank)
                      (setf (cffi:mem-aref dims-ptr :unsigned-long i)
                            (nth i dims)))
                    (hdf5:H5Screate-simple rank dims-ptr (cffi:null-pointer)))))
    (unwind-protect
         (let ((file-id (hdf5:H5Fcreate (namestring (pathname filename))
                                        hdf5:+H5F-ACC-TRUNC+
                                        -1 -1)))
           (unwind-protect
                (let ((dset-id (hdf5:H5Dcreate1 file-id path
                                                hdf5:+H5T-NATIVE-DOUBLE+
                                                space-id -1)))
                  (unwind-protect
                       (let ((total (array-total-size data))
                             (buf (cffi:foreign-alloc :double :count total)))
                         (unwind-protect
                              (progn
                                (dotimes (i total)
                                  (setf (cffi:mem-aref buf :double i)
                                        (coerce (row-major-aref data i)
                                                'double-float)))
                                (hdf5:H5Dwrite dset-id
                                               hdf5:+H5T-NATIVE-DOUBLE+
                                               hdf5:+H5S-ALL+ hdf5:+H5S-ALL+
                                               -1 buf))
                           (cffi:foreign-free buf)))
                    (hdf5:H5Dclose dset-id)))
             (hdf5:H5Fclose file-id)))
      (hdf5:H5Sclose space-id)))
  data)

(defun write-hdf5-fn (data path filename)
  (write-hdf5-inner filename path data))
