# API Documentation

Generated from Lisp source files.

## Registered Operations

### std/arrays.lisp 

---
#### `corrcoeff`

- **Function:** `array-of-correlation-coefficients`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DATA)`

**Description:**

Assums a 2D array with X values in first row, Y values in other rows. Returns 1D array of correlation coefficients.

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

#### `mean-std`

- **Function:** `calculate-mean-and-std`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DATA)`

---

#### `size`

- **Function:** `count-elements`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Count the number of elements in an array

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

- **Function:** `length`
- **Arity:** 1
- **Source:** std/arrays.lisp

---

#### `mean`

- **Function:** `mean-fn`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(DATA)`

**Description:**

Mean

---

#### `mesh-x`

- **Function:** `meshgrid-x`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(M N)`

**Description:**

Generates a mesh in Y direction of dimensions MxN

---

#### `mesh-y`

- **Function:** `meshgrid-y`
- **Arity:** 2
- **Source:** std/arrays.lisp

**Parameters:** `(M N)`

**Description:**

Generates a mesh in X direction of dimensions MxN

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

Generates a range from 0 to n-1: range 9: [0 1 2 3 4 5 6 7 8 9]

---

#### `rank`

- **Function:** `rank-fn`
- **Arity:** 1
- **Source:** std/arrays.lisp

**Parameters:** `(ARRAY)`

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
#### `eq`

- **Function:** `eql-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

**Parameters:** `(A B)`

**Description:**

Equal, eq 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 0 0]

---

#### `>=`

- **Function:** `greater-equal-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

**Parameters:** `(A B)`

**Description:**

Greater than or equal, >= 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 1 1]

---

#### `>`

- **Function:** `greater-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

**Parameters:** `(A B)`

**Description:**

Greater than, > 5 [1 2 3 4 5 6 7] -> [0 0 0 0 0 1 1]

---

#### `neq`

- **Function:** `not-eql-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

**Parameters:** `(A B)`

**Description:**

Not Equal, neq 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 1 1]

---

#### `<=`

- **Function:** `smaller-equal-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

**Parameters:** `(A B)`

**Description:**

Smaller than or equal, <= 5 [1 2 3 4 5 6 7] -> [1 1 1 1 1 0 0]

---

#### `<`

- **Function:** `smaller-fn`
- **Arity:** 2
- **Source:** std/filters.lisp

**Parameters:** `(A B)`

**Description:**

Smaller than, < 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 0 0]

---

### std/io.lisp 

---
#### `load-csv`

- **Function:** `load-csv`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Loads a table from a CSV file, comma separated, row oriented

---

#### `load`

- **Function:** `load-numbers`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Load numbers from a text file (one per line)

---

#### `run`

- **Function:** `run-script`
- **Arity:** 1
- **Source:** std/io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Execute a script file line by line

---

#### `write-csv`

- **Function:** `write-csv`
- **Arity:** 2
- **Source:** std/io.lisp

**Parameters:** `(MATRIX FILENAME &OPTIONAL (DELIMITER ,))`

**Description:**

Saves a 2D lisp array to a CSV file, comma separated, row oriented

---

### std/linear_algebra.lisp 

---
#### `conjugate-transpose`

- **Function:** `dagger`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Conjugate transport of matrix

---

#### `dagger`

- **Function:** `dagger`
- **Arity:** 1
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A)`

**Description:**

BLAS/LAPACK Required: Conjugate transport of matrix

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

#### `@`

- **Function:** `matrix-multiply`
- **Arity:** 2
- **Source:** std/linear_algebra.lisp

**Parameters:** `(A B)`

**Description:**

BLAS/LAPACK Required: Matrix Multiply two matrices A and B.

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

BLAS/LAPACK Required: Trace (sum of diagnoal elements) of matrix

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

#### `1/x`

- **Function:** `1/x-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Reciprocal: 1/a

---

#### `10^x`

- **Function:** `10^x`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

10^a

---

#### `log`

- **Function:** `10log-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

10log a

---

#### `abs`

- **Function:** `abs-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Absolute value, also works for complex numbers

---

#### `acos`

- **Function:** `acos-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Acos

---

#### `acosh`

- **Function:** `acosh-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Acosh

---

#### `+`

- **Function:** `add-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Addition

---

#### `asin`

- **Function:** `asin-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Asin

---

#### `asinh`

- **Function:** `asinh-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Asinh

---

#### `atan`

- **Function:** `atan-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Atan

---

#### `atanh`

- **Function:** `atanh-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Atanh

---

#### `chs`

- **Function:** `chs`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Change sign a -> -a

---

#### `complex`

- **Function:** `complex-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Creates complex number a+ib

---

#### `cos`

- **Function:** `cos-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Cos

---

#### `cosh`

- **Function:** `cosh-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Cosh

---

#### `%`

- **Function:** `div-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Division

---

#### `exp`

- **Function:** `exp-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Exponential: e^a

---

#### `!`

- **Function:** `fact-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Factorial ! 8 -> 40320

---

#### `frac`

- **Function:** `frac-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Returns the fractional part 1.23 -> .23

---

#### `im`

- **Function:** `imagpart-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Get the imaginary part of complex number

---

#### `intg`

- **Function:** `intg-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Floor

---

#### `ln`

- **Function:** `log-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Natural logarithm ln a

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

#### `*`

- **Function:** `mul-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Multiplication

---

#### `re`

- **Function:** `realpart-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Get the real part of complex number

---

#### `rnd`

- **Function:** `rnd-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Round

---

#### `sin`

- **Function:** `sin-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Sin

---

#### `sinh`

- **Function:** `sinh-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Sinh

---

#### `square`

- **Function:** `sqr-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Square a^2

---

#### `sqrt`

- **Function:** `sqrt-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Square root

---

#### `-`

- **Function:** `sub-fn`
- **Arity:** 2
- **Source:** std/math.lisp

**Parameters:** `(A B)`

**Description:**

Subtraction

---

#### `tan`

- **Function:** `tan-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Tan

---

#### `tanh`

- **Function:** `tanh-fn`
- **Arity:** 1
- **Source:** std/math.lisp

**Parameters:** `(A)`

**Description:**

Tanh

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
#### `cfft`

- **Function:** `cfft-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(INPUT)`

**Description:**

Complex forward Fast Fourier Transform of the input signal

---

#### `fft`

- **Function:** `fft-fn`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(INPUT)`

**Description:**

Fast Fourier Transform of the input signal.

---

#### `ifft`

- **Function:** `fft-inverse`
- **Arity:** 1
- **Source:** std/signal_processing.lisp

**Parameters:** `(INPUT)`

**Description:**

Inverse Fast Fourier Transform of the input signal.

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

#### `pop`

- **Function:** `pop-stack`
- **Arity:** 0
- **Source:** std/stack.lisp

**Description:**

Pops the top element off the stack

---

#### `peek`

- **Function:** `pretty-print-stack`
- **Arity:** 0
- **Source:** std/stack.lisp

**Description:**

Print the top 5 items of the stack in a readable format.

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

### `1/x-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Reciprocal: 1/a

---

### `10^x`

**Parameters:** `(A)`

**Source:** std/math.lisp

10^a

---

### `10log-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

10log a

---

### `abs-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Absolute value, also works for complex numbers

---

### `acos-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Acos

---

### `acosh-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Acosh

---

### `add-fn`

**Parameters:** `(A B)`

**Source:** std/math.lisp

Addition

---

### `array-of-correlation-coefficients`

**Parameters:** `(DATA)`

**Source:** std/arrays.lisp

Assums a 2D array with X values in first row, Y values in other rows. Returns 1D array of correlation coefficients.

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

### `asin-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Asin

---

### `asinh-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Asinh

---

### `atan-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Atan

---

### `atanh-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Atanh

---

### `cfft-fn`

**Parameters:** `(INPUT)`

**Source:** std/signal_processing.lisp

Complex forward Fast Fourier Transform of the input signal

---

### `chs`

**Parameters:** `(A)`

**Source:** std/math.lisp

Change sign a -> -a

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

### `cos-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Cos

---

### `cosh-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Cosh

---

### `count-elements`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Count the number of elements in an array

---

### `dagger`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Conjugate transport of matrix

---

### `det-fn`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Determinant of matrix

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

### `eql-fn`

**Parameters:** `(A B)`

**Source:** std/filters.lisp

Equal, eq 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 0 0]

---

### `exp-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Exponential: e^a

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

### `flatten`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Flatten a nested list structure into a single list.

---

### `frac-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Returns the fractional part 1.23 -> .23

---

### `greater-equal-fn`

**Parameters:** `(A B)`

**Source:** std/filters.lisp

Greater than or equal, >= 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 1 1]

---

### `greater-fn`

**Parameters:** `(A B)`

**Source:** std/filters.lisp

Greater than, > 5 [1 2 3 4 5 6 7] -> [0 0 0 0 0 1 1]

---

### `hstack`

**Parameters:** `(A B)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Concatenate matrices A, B horizontally

---

### `imagpart-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Get the imaginary part of complex number

---

### `indexof`

**Parameters:** `(VALUE ARRAY)`

**Source:** std/arrays.lisp

Return the index of the first occurrence of VALUE in ARRAY.    If VALUE is not found, return the dimensions of the array.

---

### `intg-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Floor

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

### `load-csv`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Loads a table from a CSV file, comma separated, row oriented

---

### `load-numbers`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Load numbers from a text file (one per line)

---

### `log-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Natural logarithm ln a

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

Generates a mesh in Y direction of dimensions MxN

---

### `meshgrid-y`

**Parameters:** `(M N)`

**Source:** std/arrays.lisp

Generates a mesh in X direction of dimensions MxN

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

### `not-eql-fn`

**Parameters:** `(A B)`

**Source:** std/filters.lisp

Not Equal, neq 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 1 1]

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

Pretty-print an n-dimensional ARRAY with square brackets, showing a maximum of 10 items per dimension.

---

### `pretty-print-stack`

**Parameters:** `()`

**Source:** std/stack.lisp

Print the top 5 items of the stack in a readable format.

---

### `range-fn`

**Parameters:** `(N)`

**Source:** std/arrays.lisp

Generates a range from 0 to n-1: range 9: [0 1 2 3 4 5 6 7 8 9]

---

### `realpart-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Get the real part of complex number

---

### `reshape`

**Parameters:** `(SHAPE ARRAY)`

**Source:** std/arrays.lisp

Reshape an array into a multi-dimensional array based on the given shape.    The shape is a list of dimensions, e.g. (2 3) for a 2x3 matrix.

---

### `rnd-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Round

---

### `rotate`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Rotate clockwise, i.e. last element becomes first element

---

### `run-script`

**Parameters:** `(FILENAME)`

**Source:** std/io.lisp

Execute a script file line by line

---

### `shape-fn`

**Parameters:** `(ARRAY)`

**Source:** std/arrays.lisp

Return the shape of an array

---

### `sin-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Sin

---

### `sinh-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Sinh

---

### `smaller-equal-fn`

**Parameters:** `(A B)`

**Source:** std/filters.lisp

Smaller than or equal, <= 5 [1 2 3 4 5 6 7] -> [1 1 1 1 1 0 0]

---

### `smaller-fn`

**Parameters:** `(A B)`

**Source:** std/filters.lisp

Smaller than, < 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 0 0]

---

### `sqr-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Square a^2

---

### `sqrt-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Square root

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

### `tan-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Tan

---

### `tanh-fn`

**Parameters:** `(A)`

**Source:** std/math.lisp

Tanh

---

### `trace-fn`

**Parameters:** `(A)`

**Source:** std/linear_algebra.lisp

BLAS/LAPACK Required: Trace (sum of diagnoal elements) of matrix

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

