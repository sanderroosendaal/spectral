# API Documentation

Generated from Lisp source files.

## Built-in Operators

These operators are part of the core language (parser-level) and are not
registered via `register-op`.

### Reduction

Reduce an array to a single value (or along the first axis for 2D arrays).

| Operator | Operation | Example |
|----------|-----------|---------|
| `/+` | Sum | `/+ [1 2 3 4 5]` -> 15 |
| `/*` | Product | `/* [1 2 3 4 5]` -> 120 |
| `/max` | Maximum | `/max [3 1 4 1 5]` -> 5 |
| `/min` | Minimum | `/min [3 1 4 1 5]` -> 1 |

For 2D arrays, reduction is along the first axis: `/+ [[1 2][3 4]]` -> #(4 6).

Scalar operands produce a clear error; use array syntax, e.g. `/+ [1 2 3]`.

---

### Scan

Prefix scan (inclusive): cumulative result at each position.

| Operator | Operation | Example |
|----------|-----------|---------|
| `&+` | Cumulative sum | `&+ [1 2 3 4 5]` -> #(1 3 6 10 15) |
| `&*` | Cumulative product | `&* [1 2 3 4 5]` -> #(1 2 6 24 120) |

Scalar operands produce a clear error; use array syntax, e.g. `&+ [1 2 3]`.

---

### REPL Error Handling

The interactive REPL catches evaluation errors, prints the message, and continues.
The session stays alive instead of exiting on error.

---

## Registered Operations

### std/arrays.lisp 

---
#### `corrcoeff`

- **Function:** `array-of-correlation-coefficients`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DATA)`

**Description:**

Assumes a 2D array with X values in first row, Y values in other rows. Returns 1D array of correlation coefficients.

---

#### `drop`

- **Function:** `drop`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(INDEX ARRAY)`

**Description:**

Drop the first N elements from an array.

---

#### `flatten`

- **Function:** `flatten`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Flatten a nested list structure into a single list.

---

#### `idx`

- **Function:** `indexof`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(VALUE ARRAY)`

**Description:**

Return the index of the first occurrence of VALUE in ARRAY.    If VALUE is not found, return the dimensions of the array.

---

#### `length`

- **Function:** `spectral-length`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Return the number of elements along the first dimension (or total size for 1D arrays).

---

#### `mean`

- **Function:** `mean-fn`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DATA)`

**Description:**

Mean

---

#### `mean-std`

- **Function:** `calculate-mean-and-std`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DATA)`

**Description:**

Return mean and standard deviation of array elements. Handles 1D and flattens higher dimensions.

---

#### `mesh-x`

- **Function:** `meshgrid-x`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(M N)`

**Description:**

Generates a mesh in X direction of dimensions MxN.

---

#### `mesh-y`

- **Function:** `meshgrid-y`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(M N)`

**Description:**

Generates a mesh in Y direction of dimensions MxN.

---

#### `ncol`

- **Function:** `ncol`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(A)`

**Description:**

Number of columns in matrix

---

#### `nrow`

- **Function:** `nrow`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(A)`

**Description:**

Number of rows in matrix

---

#### `ones`

- **Function:** `ones`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DIMS)`

**Description:**

Creates an array of dimensions DIMS filled with ones

---

#### `pick`

- **Function:** `pick`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(INDEX ARRAY)`

**Description:**

Pick an element from an array based on the index.    If index is a number, it returns the nth element.    If index is a list, it traverses the array according to the indices in the list.

---

#### `range`

- **Function:** `range-fn`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(N)`

**Description:**

Generate n elements from 0 to n-1. E.g. range 5 -> [0 1 2 3 4], range 9 -> [0..8].

---

#### `rank`

- **Function:** `rank-fn`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Return the number of dimensions (rank) of the array.

---

#### `reshape`

- **Function:** `reshape`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(SHAPE ARRAY)`

**Description:**

Reshape an array into a multi-dimensional array based on the given shape.    The shape is a list of dimensions, e.g. (2 3) for a 2x3 matrix.

---

#### `reverse`

- **Function:** `reverse-array-first-axis`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Reverse the order of elements along the first axis of the array.

---

#### `rotate`

- **Function:** `rotate`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Rotate clockwise, i.e. last element becomes first element

---

#### `shape`

- **Function:** `shape-fn`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Return the shape of an array

---

#### `size`

- **Function:** `count-elements`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Count the number of elements in an array

---

#### `slice`

- **Function:** `array-slice`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(N ARR)`

**Description:**

Pick nth row from array

---

#### `stack`

- **Function:** `array-stack`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY1 ARRAY2)`

**Description:**

Join two arrays along a new axis    Dimension analysis will be done and the    most intuitive option returned, or an error    if dimensions don't match

---

#### `std`

- **Function:** `std-fn`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DATA)`

**Description:**

Standard Deviation

---

#### `sub`

- **Function:** `sub`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(IDX A)`

**Description:**

Returns sub-array composed of the elements that would start with given subscripts

---

#### `take`

- **Function:** `take`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(INDEX ARRAY)`

**Description:**

Take the first N elements from an array.

---

#### `transpose`

- **Function:** `transpose`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(MATRIX)`

**Description:**

Transpose a matrix (list of lists)

---

#### `where`

- **Function:** `where`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Return the 1D array of index vectors of non-zero elements in an array.

---

#### `zeros`

- **Function:** `zeros`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DIMS)`

**Description:**

Creates an array of dimensions DIMS filled with zeros

---

### std/filters.lisp 

---
#### `<`

- **Function:** `smaller-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

---

#### `<=`

- **Function:** `smaller-equal-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

---

#### `>`

- **Function:** `greater-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

---

#### `>=`

- **Function:** `greater-equal-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

---

#### `eq`

- **Function:** `eql-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

---

#### `neq`

- **Function:** `not-eql-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

---

### std/io.lisp 

---
#### `load`

- **Function:** `load-numbers`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Load numbers from a text file (one per line)

---

#### `load-binary`

- **Function:** `load-binary`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Load array from .sdat binary file. Returns double-float array.

---

#### `load-csv`

- **Function:** `load-csv`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Loads a table from a CSV file, comma separated, row oriented

---

#### `load-hdf5`

- **Function:** `load-hdf5-fn`
- **Arity:** 2
- **Source:** std/io.lisp

**Parameters:** `(PATH FILENAME)`

---

#### `load-npy`

- **Function:** `load-npy`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Load array from .npy file. Returns double-float. Supports <f8, <i4, 1D/2D, C order.

---

#### `run`

- **Function:** `run-script`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Execute a script file line by line. Errors include filename and line number.

---

#### `write`

- **Function:** `write-numbers`
- **Arity:** 2
- **Source:** std/io.lisp

**Parameters:** `(FILENAME DATA)`

**Description:**

Write numbers (vector) to a text file (one per line)

---

#### `write-binary`

- **Function:** `write-binary`
- **Arity:** 2
- **Source:** std/io.lisp

**Parameters:** `(FILENAME DATA)`

**Description:**

Write array to .sdat binary file. Supports float64, float32, int32, int16.

---

#### `write-csv`

- **Function:** `write-csv`
- **Arity:** 2
- **Source:** std/io.lisp

**Parameters:** `(MATRIX FILENAME &OPTIONAL (DELIMITER ,))`

**Description:**

Saves a 2D lisp array to a CSV file, comma separated, row oriented

---

#### `write-hdf5`

- **Function:** `write-hdf5-fn`
- **Arity:** 3
- **Source:** std/io.lisp

**Parameters:** `(DATA PATH FILENAME)`

---

#### `write-npy`

- **Function:** `write-npy`
- **Arity:** 2
- **Source:** std/io.lisp

**Parameters:** `(FILENAME DATA)`

**Description:**

Write array to .npy file. Supports float64, int32, 1D/2D, C order.

---

### std/linear_algebra.lisp 

---
#### `@`

- **Function:** `matrix-multiply`
- **Arity:** 2
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A B)`

**Description:**

BLAS/LAPACK Required: Matrix Multiply two matrices A and B.

---

#### `conjugate-transpose`

- **Function:** `dagger`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Conjugate transpose of matrix.

---

#### `dagger`

- **Function:** `dagger`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Conjugate transpose of matrix.

---

#### `det`

- **Function:** `det-fn`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Determinant of matrix

---

#### `eig`

- **Function:** `eig-fn`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Returns eigenvalues and eigenvectors of matrix

---

#### `inv`

- **Function:** `matrix-inv`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Matrix inverse

---

#### `mmult`

- **Function:** `matrix-multiply`
- **Arity:** 2
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A B)`

**Description:**

BLAS/LAPACK Required: Matrix Multiply two matrices A and B.

---

#### `trace`

- **Function:** `trace-fn`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Trace (sum of diagonal elements) of matrix.

---

#### `tril`

- **Function:** `tril`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Lower triangular part of matrix

---

#### `triu`

- **Function:** `triu`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Upper triangular part of matrix

---

### std/math.lisp 

---
#### `!`

- **Function:** `fact-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Factorial ! 8 -> 40320

---

#### `%`

- **Function:** `div-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Division

---

#### `*`

- **Function:** `mul-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Multiplication

---

#### `+`

- **Function:** `add-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Addition

---

#### `-`

- **Function:** `sub-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Subtraction

---

#### `->p`

- **Function:** `->p`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Converts a = [x, y] in Rectangular coordinates to Polar coordinates [r, theta]

---

#### `->r`

- **Function:** `->r`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Converts a = [r, theta] in Polar coordinates to Rectangular coordinates [x, y]

---

#### `complex`

- **Function:** `complex-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Creates complex number a+ib

---

#### `max`

- **Function:** `max-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Maximum

---

#### `min`

- **Function:** `min-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Minimum

---

#### `y^x`

- **Function:** `y^x-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

y^x first on stack is exponent, second is base: y^x 3 2 --> 8

---

### std/plotting.lisp 

---
#### `3d-plot`

- **Function:** `3d-plot`
- **Arity:** 1
- **Source:** std/plotting.lisp

---

#### `format-plot`

- **Function:** `format-plot`
- **Arity:** 1
- **Source:** std/plotting.lisp

---

#### `plot`

- **Function:** `plot-fn`
- **Arity:** 1
- **Source:** std/plotting.lisp

---

#### `surf`

- **Function:** `surf-plot`
- **Arity:** 1
- **Source:** std/plotting.lisp

---

### std/signal_processing.lisp 

---
#### `bandpass`

- **Function:** `bandpass-fn`
- **Arity:** 2
- **Source:** std/signal_processing.lisp

**Parameters:** `(PARAMS SIGNAL)`

**Description:**

Bandpass filter: keep frequencies in [f_low, f_high] Hz. Params = [f_low f_high sample_rate].

---

#### `bandstop`

- **Function:** `bandstop-fn`
- **Arity:** 2
- **Source:** std/signal_processing.lisp

**Parameters:** `(PARAMS SIGNAL)`

**Description:**

Bandstop (notch) filter: zero frequencies in [f_low, f_high] Hz. Params = [f_low f_high sample_rate].

---

#### `cfft`

- **Function:** `cfft-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(INPUT)`

**Description:**

Complex forward Fast Fourier Transform of the input signal

---

#### `detrend`

- **Function:** `detrend-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(SIGNAL)`

**Description:**

Remove linear trend from signal. Same-length output.

---

#### `differentiate`

- **Function:** `differentiate-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(SIGNAL)`

**Description:**

First derivative via central differencing. Same-length output; edges use forward/backward diff.

---

#### `fft`

- **Function:** `fft-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(INPUT)`

**Description:**

Fast Fourier Transform of the input signal.

---

#### `find-peaks`

- **Function:** `find-peaks-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(DATA)`

**Description:**

Return 1D array of indices of local maxima. Strict: x[i] > both neighbors.

---

#### `find-valleys`

- **Function:** `find-valleys-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(DATA)`

**Description:**

Return 1D array of indices of local minima. Strict: x[i] < both neighbors.

---

#### `highpass`

- **Function:** `highpass-fn`
- **Arity:** 2
- **Source:** std/signal_processing.lisp

**Parameters:** `(PARAMS SIGNAL)`

**Description:**

Highpass filter: keep frequencies above f_cutoff Hz. Params = [f_cutoff sample_rate].

---

#### `ifft`

- **Function:** `fft-inverse`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(INPUT)`

**Description:**

Inverse Fast Fourier Transform of the input signal.

---

#### `lowpass`

- **Function:** `lowpass-fn`
- **Arity:** 2
- **Source:** std/signal_processing.lisp

**Parameters:** `(PARAMS SIGNAL)`

**Description:**

Lowpass filter: keep frequencies below f_cutoff Hz. Params = [f_cutoff sample_rate].

---

#### `psd`

- **Function:** `psd-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(SIGNAL)`

**Description:**

Power spectral density: |FFT(signal)|^2. Returns real array of power at each frequency bin.

---

#### `savgol`

- **Function:** `savgol-fn`
- **Arity:** 2
- **Source:** std/signal_processing.lisp

**Parameters:** `(PARAMS SIGNAL)`

**Description:**

Savitzky-Golay polynomial smoothing. Params = [window_length poly_order].

---

#### `smooth`

- **Function:** `smooth-fn`
- **Arity:** 2
- **Source:** std/signal_processing.lisp

**Parameters:** `(WINDOW-SIZE SIGNAL)`

**Description:**

Boxcar moving average. window_size points, same-length output, partial windows at edges.

---

### std/stack.lisp 

---
#### `d`

- **Function:** `dup`
- **Arity:** 0
- **Source:** std/stack.lisp

**Description:**

Duplicate the top element of the stack.

---

#### `dup`

- **Function:** `dup`
- **Arity:** 0
- **Source:** std/stack.lisp

**Description:**

Duplicate the top element of the stack.

---

#### `peek`

- **Function:** `pretty-print-stack`
- **Arity:** 0
- **Source:** std/stack.lisp

**Description:**

Print the top N items of the stack in a readable format (N = *peek-stack-limit*).

---

#### `pop`

- **Function:** `pop-stack`
- **Arity:** 0
- **Source:** std/stack.lisp

**Description:**

Pops the top element off the stack

---

#### `swap`

- **Function:** `swap`
- **Arity:** 0
- **Source:** std/stack.lisp

**Description:**

Swap top and second element of the stack

---

## All Documented Functions

### `->p`

**Parameters:** `(A)`

**Source:** std/math.lisp

Converts a = [x, y] in Rectangular coordinates to Polar coordinates [r, theta]

---

### `->r`

**Parameters:** `(A)`

**Source:** std/math.lisp

Converts a = [r, theta] in Polar coordinates to Rectangular coordinates [x, y]

---

### `add-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Addition

---

### `array-fn`

**Parameters:** `(OP A)`

**Source:** spectral.lisp

Apply a unary operation element-wise on an n-dimensional array.
    List inputs are coerced to vectors.

---

### `array-of-correlation-coefficients`

**Parameters:** `(DATA)`

**Source:** std/arrays.lisp

Assumes a 2D array with X values in first row, Y values in other rows. Returns 1D array of correlation coefficients.

---

### `array-op`

**Parameters:** `(OP A B)`

**Source:** spectral.lisp

Apply binary operation element-wise on n-dimensional arrays.
    List inputs are coerced to vectors.

---

### `array-row-major-index-to-subscript`

**Parameters:** `(DIMS INDEX)`

**Source:** std/arrays.lisp

Convert row-major index to a list of subscripts for the given DIMS.

---

### `array-slice`

**Parameters:** `(N ARR)`

**Source:** std/arrays.lisp

Pick nth row from array

---

### `array-stack`

**Parameters:** `(ARRAY1 ARRAY2)`

**Source:** std/arrays.lisp

Join two arrays along a new axis    Dimension analysis will be done and the    most intuitive option returned, or an error    if dimensions don't match

---

### `bandpass-fn`

**Parameters:** `(PARAMS SIGNAL)`

**Source:** std/signal_processing.lisp

Bandpass filter: keep frequencies in [f_low, f_high] Hz. Params = [f_low f_high sample_rate].

---

### `bandstop-fn`

**Parameters:** `(PARAMS SIGNAL)`

**Source:** std/signal_processing.lisp

Bandstop (notch) filter: zero frequencies in [f_low, f_high] Hz. Params = [f_low f_high sample_rate].

---

### `calculate-mean-and-std`

**Parameters:** `(DATA)`

**Source:** std/arrays.lisp

Return mean and standard deviation of array elements. Handles 1D and flattens higher dimensions.

---

### `cfft-fn`

**Parameters:** `(INPUT)`

**Source:** std/signal_processing.lisp

Complex forward Fast Fourier Transform of the input signal

---

### `complex-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Creates complex number a+ib

---

### `convert-to-float`

**Parameters:** `(INPUT)`

**Source:** std/signal_processing.lisp

Convert the input 1D array to a float array. Do nothing if the array is already double float.

---

### `copy-array-range`

**Parameters:** `(DEST DEST-START SOURCE SOURCE-START COUNT)`

**Source:** std/arrays.lisp

Copy COUNT elements from SOURCE (row-major offset SOURCE-START) to DEST (row-major offset DEST-START).

---

### `count-elements`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Count the number of elements in an array

---

### `dagger`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Conjugate transpose of matrix.

---

### `det-fn`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Determinant of matrix

---

### `detrend-fn`

**Parameters:** `(SIGNAL)`

**Source:** std/signal_processing.lisp

Remove linear trend from signal. Same-length output.

---

### `differentiate-fn`

**Parameters:** `(SIGNAL)`

**Source:** std/signal_processing.lisp

First derivative via central differencing. Same-length output; edges use forward/backward diff.

---

### `div-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Division

---

### `drop`

**Parameters:** `(INDEX ARRAY)`

**Source:** std/arrays.lisp

Drop the first N elements from an array.

---

### `dup`

**Parameters:** `()`

**Source:** std/stack.lisp

Duplicate the top element of the stack.

---

### `eig-fn`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Returns eigenvalues and eigenvectors of matrix

---

### `ensure-double-float-vector`

**Parameters:** `(X)`

**Source:** std/signal_processing.lisp

Ensure input is a double-float vector. Coerces lists and non-float arrays.

---

### `ensure-vector`

**Parameters:** `(X)`

**Source:** std/signal_processing.lisp

Coerce list to vector; return other sequences (e.g. arrays) unchanged.

---

### `eval-node`

**Parameters:** `(ELEMENT &OPTIONAL (DEBUG NIL))`

**Source:** spectral.lisp

Execute AST element

---

### `evaluate`

**Parameters:** `(EXPR-STRING &OPTIONAL (DEBUG NIL))`

**Source:** spectral.lisp

Parse and evaluate a Spectral expression. Returns the top-of-stack value.

---

### `fact-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Factorial ! 8 -> 40320

---

### `fft-backward`

**Parameters:** `(DATA &KEY (NORMALIZE T))`

**Source:** std/fftw-ffi.lisp

Compute backward (inverse) FFT of complex data

---

### `fft-cleanup`

**Parameters:** `()`

**Source:** std/fftw-ffi.lisp

Clean up FFTW internal data structures

---

### `fft-fn`

**Parameters:** `(INPUT)`

**Source:** std/signal_processing.lisp

Fast Fourier Transform of the input signal.

---

### `fft-forward`

**Parameters:** `(DATA &KEY (NORMALIZE T))`

**Source:** std/fftw-ffi.lisp

Compute forward FFT of complex data

---

### `fft-inverse`

**Parameters:** `(INPUT)`

**Source:** std/signal_processing.lisp

Inverse Fast Fourier Transform of the input signal.

---

### `fft-real-forward`

**Parameters:** `(DATA)`

**Source:** std/fftw-ffi.lisp

Compute forward FFT of real data (returns complex)

---

### `fftw-complex-to-lisp-array`

**Parameters:** `(FFTW-PTR SIZE)`

**Source:** std/fftw-ffi.lisp

Convert FFTW complex array back to Lisp array

---

### `fftw-real-to-lisp-array`

**Parameters:** `(FFTW-PTR SIZE)`

**Source:** std/fftw-ffi.lisp

Convert FFTW real array back to Lisp array

---

### `find-peaks-fn`

**Parameters:** `(DATA)`

**Source:** std/signal_processing.lisp

Return 1D array of indices of local maxima. Strict: x[i] > both neighbors.

---

### `find-valleys-fn`

**Parameters:** `(DATA)`

**Source:** std/signal_processing.lisp

Return 1D array of indices of local minima. Strict: x[i] < both neighbors.

---

### `flatten`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Flatten a nested list structure into a single list.

---

### `highpass-fn`

**Parameters:** `(PARAMS SIGNAL)`

**Source:** std/signal_processing.lisp

Highpass filter: keep frequencies above f_cutoff Hz. Params = [f_cutoff sample_rate].

---

### `hstack`

**Parameters:** `(A B)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Concatenate matrices A, B horizontally

---

### `indexof`

**Parameters:** `(VALUE ARRAY)`

**Source:** std/arrays.lisp

Return the index of the first occurrence of VALUE in ARRAY.    If VALUE is not found, return the dimensions of the array.

---

### `is-header-row`

**Parameters:** `(ROW)`

**Source:** std/io.lisp

Check if a row contains headers

---

### `lisp-array-to-fftw-complex`

**Parameters:** `(LISP-ARRAY FFTW-PTR)`

**Source:** std/fftw-ffi.lisp

Copy Lisp complex array to FFTW complex array

---

### `lisp-array-to-fftw-real`

**Parameters:** `(LISP-ARRAY FFTW-PTR)`

**Source:** std/fftw-ffi.lisp

Copy Lisp real array to FFTW real array

---

### `load-binary`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Load array from .sdat binary file. Returns double-float array.

---

### `load-csv`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Loads a table from a CSV file, comma separated, row oriented

---

### `load-npy`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Load array from .npy file. Returns double-float. Supports <f8, <i4, 1D/2D, C order.

---

### `load-numbers`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Load numbers from a text file (one per line)

---

### `lowpass-fn`

**Parameters:** `(PARAMS SIGNAL)`

**Source:** std/signal_processing.lisp

Lowpass filter: keep frequencies below f_cutoff Hz. Params = [f_cutoff sample_rate].

---

### `make-complex-test-signal`

**Parameters:** `(N &KEY (FREQUENCY 1.0) (SAMPLE-RATE 10.0))`

**Source:** std/fftw-ffi.lisp

Generate a complex test signal

---

### `make-test-signal`

**Parameters:** `(N &KEY (FREQUENCY 1.0) (SAMPLE-RATE 10.0))`

**Source:** std/fftw-ffi.lisp

Generate a test sinusoidal signal

---

### `matrix-inv`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Matrix inverse

---

### `matrix-multiply`

**Parameters:** `(A B)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Matrix Multiply two matrices A and B.

---

### `max-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Maximum

---

### `mean-fn`

**Parameters:** `(DATA)`

**Source:** std/arrays.lisp

Mean

---

### `meshgrid-x`

**Parameters:** `(M N)`

**Source:** std/arrays.lisp

Generates a mesh in X direction of dimensions MxN.

---

### `meshgrid-y`

**Parameters:** `(M N)`

**Source:** std/arrays.lisp

Generates a mesh in Y direction of dimensions MxN.

---

### `min-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Minimum

---

### `mul-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Multiplication

---

### `ncol`

**Parameters:** `(A)`

**Source:** std/arrays.lisp

Number of columns in matrix

---

### `npy-parse-shape`

**Parameters:** `(STR)`

**Source:** std/io.lisp

Extract shape tuple from e.g. '5, ' or '2, 3)' as list of dims.

---

### `nrow`

**Parameters:** `(A)`

**Source:** std/arrays.lisp

Number of rows in matrix

---

### `ones`

**Parameters:** `(DIMS)`

**Source:** std/arrays.lisp

Creates an array of dimensions DIMS filled with ones

---

### `parse-npy-header`

**Parameters:** `(HEADER)`

**Source:** std/io.lisp

Parse NPY header dict. Returns (descr dims). Supports <f8, <i4, fortran_order False.

---

### `parse-number`

**Parameters:** `(STR)`

**Source:** std/io.lisp

Parse a string as a number

---

### `parse-number-safe`

**Parameters:** `(STRING)`

**Source:** std/io.lisp

Safely parse a string to number

---

### `pick`

**Parameters:** `(INDEX ARRAY)`

**Source:** std/arrays.lisp

Pick an element from an array based on the index.    If index is a number, it returns the nth element.    If index is a list, it traverses the array according to the indices in the list.

---

### `pop-stack`

**Parameters:** `()`

**Source:** std/stack.lisp

Pops the top element off the stack

---

### `pretty-print-array`

**Parameters:** `(ARRAY &OPTIONAL (STREAM *STANDARD-OUTPUT*))`

**Source:** std/stack.lisp

Pretty-print an n-dimensional ARRAY with square brackets, showing
 a maximum of *print-limit-per-dim* items per dimension (elided with ...).

---

### `pretty-print-stack`

**Parameters:** `()`

**Source:** std/stack.lisp

Print the top N items of the stack in a readable format (N = *peek-stack-limit*).

---

### `pretty-print-stack-item`

**Parameters:** `(ITEM)`

**Source:** std/stack.lisp

Print a single stack item (number, string, list, or array) to *standard-output*.

---

### `psd-fn`

**Parameters:** `(SIGNAL)`

**Source:** std/signal_processing.lisp

Power spectral density: |FFT(signal)|^2. Returns real array of power at each frequency bin.

---

### `range-fn`

**Parameters:** `(N)`

**Source:** std/arrays.lisp

Generate n elements from 0 to n-1. E.g. range 5 -> [0 1 2 3 4], range 9 -> [0..8].

---

### `rank-fn`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Return the number of dimensions (rank) of the array.

---

### `register-op`

**Parameters:** `(NAME FUNCTION ARITY)`

**Source:** spectral.lisp

Register a new operation with the given name, function, and arity.

---

### `register-stack-op`

**Parameters:** `(NAME FUNCTION ARITY)`

**Source:** spectral.lisp

Registers a new stack operation with the given name, function and arity

---

### `reset-spectral-state`

**Parameters:** `()`

**Source:** spectral.lisp

Clear stack, variables, and functions. Used for fresh test runs.

---

### `reshape`

**Parameters:** `(SHAPE ARRAY)`

**Source:** std/arrays.lisp

Reshape an array into a multi-dimensional array based on the given shape.    The shape is a list of dimensions, e.g. (2 3) for a 2x3 matrix.

---

### `reverse-array-first-axis`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Reverse the order of elements along the first axis of the array.

---

### `rotate`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Rotate clockwise, i.e. last element becomes first element

---

### `run-script`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Execute a script file line by line. Errors include filename and line number.

---

### `savgol-fn`

**Parameters:** `(PARAMS SIGNAL)`

**Source:** std/signal_processing.lisp

Savitzky-Golay polynomial smoothing. Params = [window_length poly_order].

---

### `savgol-kernel`

**Parameters:** `(M N)`

**Source:** std/signal_processing.lisp

Return 1D array of SG coefficients for window m, polynomial order n.

---

### `shape-fn`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Return the shape of an array

---

### `smooth-fn`

**Parameters:** `(WINDOW-SIZE SIGNAL)`

**Source:** std/signal_processing.lisp

Boxcar moving average. window_size points, same-length output, partial windows at edges.

---

### `spectral-error`

**Parameters:** `(FORMAT-CONTROL &REST FORMAT-ARGS)`

**Source:** spectral.lisp

Signal a user-facing Spectral error with consistent formatting.

---

### `spectral-length`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Return the number of elements along the first dimension (or total size for 1D arrays).

---

### `std-fn`

**Parameters:** `(DATA)`

**Source:** std/arrays.lisp

Standard Deviation

---

### `sub`

**Parameters:** `(IDX A)`

**Source:** std/arrays.lisp

Returns sub-array composed of the elements that would start with given subscripts

---

### `sub-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Subtraction

---

### `swap`

**Parameters:** `()`

**Source:** std/stack.lisp

Swap top and second element of the stack

---

### `take`

**Parameters:** `(INDEX ARRAY)`

**Source:** std/arrays.lisp

Take the first N elements from an array.

---

### `tokenize`

**Parameters:** `(EXPR-STRING)`

**Source:** spectral.lisp

Simple tokenizer

---

### `trace-fn`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Trace (sum of diagonal elements) of matrix.

---

### `transpose`

**Parameters:** `(MATRIX)`

**Source:** std/arrays.lisp

Transpose a matrix (list of lists)

---

### `tril`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Lower triangular part of matrix

---

### `triu`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Upper triangular part of matrix

---

### `vstack`

**Parameters:** `(A B)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Concatenate matrices A, B vertically

---

### `where`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Return the 1D array of index vectors of non-zero elements in an array.

---

### `write-binary`

**Parameters:** `(FILENAME DATA)`

**Source:** std/io.lisp

Write array to .sdat binary file. Supports float64, float32, int32, int16.

---

### `write-csv`

**Parameters:** `(MATRIX FILENAME &OPTIONAL (DELIMITER ,))`

**Source:** std/io.lisp

Saves a 2D lisp array to a CSV file, comma separated, row oriented

---

### `write-csv-field`

**Parameters:** `(VALUE STREAM DELIMITER)`

**Source:** std/io.lisp

Write a single field to CSV stream, properly escaping if necessary.

---

### `write-npy`

**Parameters:** `(FILENAME DATA)`

**Source:** std/io.lisp

Write array to .npy file. Supports float64, int32, 1D/2D, C order.

---

### `write-numbers`

**Parameters:** `(FILENAME DATA)`

**Source:** std/io.lisp

Write numbers (vector) to a text file (one per line)

---

### `xyz-array-to-xyz`

**Parameters:** `(ARRAY)`

**Source:** std/plotting.lisp

Takes a (3xMxN) array and returns 3 (MxN) arrays

---

### `y^x-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

y^x first on stack is exponent, second is base: y^x 3 2 --> 8

---

### `zeros`

**Parameters:** `(DIMS)`

**Source:** std/arrays.lisp

Creates an array of dimensions DIMS filled with zeros

---

