To load "cl-csv":
  Load 1 ASDF system:
    cl-csv
; Loading "cl-csv"
.....
# API Documentation

Generated from Lisp source files.

## Registered Operations

### arrays.lisp 

#### `size`

- **Function:** `count-elements`
- **Arity:** 1
- **Source:** arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Count the number of elements in an array

---

#### `drop`

- **Function:** `drop`
- **Arity:** 2
- **Source:** arrays.lisp

**Parameters:** `(INDEX ARRAY)`

**Description:**

Drop the first N elements from an array.

---

#### `flatten`

- **Function:** `flatten`
- **Arity:** 1
- **Source:** arrays.lisp

**Parameters:** `(LST)`

**Description:**

Flatten a nested list structure into a single list.

---

#### `idx`

- **Function:** `indexof`
- **Arity:** 2
- **Source:** arrays.lisp

**Parameters:** `(VALUE ARRAY)`

**Description:**

Return the index of the first occurrence of VALUE in ARRAY.    If VALUE is not found, return the length of the array.

---

#### `length`

- **Function:** `length`
- **Arity:** 1
- **Source:** arrays.lisp

---

#### `pick`

- **Function:** `pick`
- **Arity:** 2
- **Source:** arrays.lisp

**Parameters:** `(INDEX ARRAY)`

**Description:**

Pick an element from an array based on the index.    If index is a number, it returns the nth element.    If index is a list, it traverses the array according to the indices in the list.

---

#### `range`

- **Function:** `range-fn`
- **Arity:** 1
- **Source:** arrays.lisp

**Parameters:** `(N)`

---

#### `reshape`

- **Function:** `reshape`
- **Arity:** 2
- **Source:** arrays.lisp

**Parameters:** `(SHAPE DATA)`

**Description:**

Reshape a flat list into a multi-dimensional array based on the given shape.    The shape is a list of dimensions, e.g. (2 3) for a 2x3 matrix.

---

#### `rotate`

- **Function:** `rotate`
- **Arity:** 1
- **Source:** arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Rotate clockwise, i.e. last element becomes first element

---

#### `shape`

- **Function:** `shape-fn`
- **Arity:** 1
- **Source:** arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Return the shape of an array

---

#### `take`

- **Function:** `take`
- **Arity:** 2
- **Source:** arrays.lisp

**Parameters:** `(INDEX ARRAY)`

**Description:**

Take the first N elements from an array.

---

#### `transpose`

- **Function:** `transpose`
- **Arity:** 1
- **Source:** arrays.lisp

**Parameters:** `(MATRIX)`

**Description:**

Transpose a matrix (list of lists)

---

#### `where`

- **Function:** `where`
- **Arity:** 1
- **Source:** arrays.lisp

**Parameters:** `(ARRAY)`

**Description:**

Return the indices of non-zero elements in an array.

---

### filters.lisp 

#### `eq`

- **Function:** `eql-fn`
- **Arity:** 2
- **Source:** filters.lisp

**Parameters:** `(A B)`

**Description:**

Equal, eq 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 0 0]

---

#### `>=`

- **Function:** `greater-equal-fn`
- **Arity:** 2
- **Source:** filters.lisp

**Parameters:** `(A B)`

**Description:**

Greater than or equal, >= 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 1 1]

---

#### `>`

- **Function:** `greater-fn`
- **Arity:** 2
- **Source:** filters.lisp

**Parameters:** `(A B)`

**Description:**

Greater than, > 5 [1 2 3 4 5 6 7] -> [0 0 0 0 0 1 1]

---

#### `neq`

- **Function:** `not-eql-fn`
- **Arity:** 2
- **Source:** filters.lisp

**Parameters:** `(A B)`

**Description:**

Not Equal, neq 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 1 1]

---

#### `<=`

- **Function:** `smaller-equal-fn`
- **Arity:** 2
- **Source:** filters.lisp

**Parameters:** `(A B)`

**Description:**

Smaller than or equal, <= 5 [1 2 3 4 5 6 7] -> [1 1 1 1 1 0 0]

---

#### `<`

- **Function:** `smaller-fn`
- **Arity:** 2
- **Source:** filters.lisp

**Parameters:** `(A B)`

**Description:**

Smaller than, < 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 0 0]

---

### io.lisp 

#### `load-csv`

- **Function:** `load-csv`
- **Arity:** 1
- **Source:** io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Loads a table from a CSV file, comma separated, row oriented

---

#### `load`

- **Function:** `load-numbers`
- **Arity:** 1
- **Source:** io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Load numbers from a text file (one per line)

---

#### `run`

- **Function:** `run-script`
- **Arity:** 1
- **Source:** io.lisp

**Parameters:** `(FILENAME)`

**Description:**

Execute a script file line by line

---

### math.lisp 

#### `->p`

- **Function:** `->p`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Converts a = [x, y] in Rectangular coordinates to Polar coordinates [r, theta]

---

#### `->r`

- **Function:** `->r`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Converts a = [r, theta] in Polar coordinates to Rectangular coordinates [x, y]

---

#### `1/x`

- **Function:** `1/x-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Reciprocal: 1/a

---

#### `10^x`

- **Function:** `10^x`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

10^a

---

#### `log`

- **Function:** `10log-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

10log a

---

#### `abs`

- **Function:** `abs-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Absolute value, also works for complex numbers

---

#### `acos`

- **Function:** `acos-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Acos

---

#### `acosh`

- **Function:** `acosh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Acosh

---

#### `+`

- **Function:** `add-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Addition

---

#### `asin`

- **Function:** `asin-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Asin

---

#### `asinh`

- **Function:** `asinh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Asinh

---

#### `atan`

- **Function:** `atan-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Atan

---

#### `atanh`

- **Function:** `atanh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Atanh

---

#### `chs`

- **Function:** `chs`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Change sign a -> -a

---

#### `complex`

- **Function:** `complex-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Creates complex number a+ib

---

#### `cos`

- **Function:** `cos-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Cos

---

#### `cosh`

- **Function:** `cosh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Cosh

---

#### `%`

- **Function:** `div-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Division

---

#### `exp`

- **Function:** `exp-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Exponential: e^a

---

#### `!`

- **Function:** `fact-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Factorial ! 8 -> 40320

---

#### `frac`

- **Function:** `frac-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Returns the fractional part 1.23 -> .23

---

#### `intg`

- **Function:** `intg-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Floor

---

#### `ln`

- **Function:** `log-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Natural logarithm ln a

---

#### `max`

- **Function:** `max-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Maximum

---

#### `min`

- **Function:** `min-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Minimum

---

#### `*`

- **Function:** `mul-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Multiplication

---

#### `rnd`

- **Function:** `rnd-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Round

---

#### `sin`

- **Function:** `sin-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Sin

---

#### `sinh`

- **Function:** `sinh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Sinh

---

#### `square`

- **Function:** `sqr-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Square a^2

---

#### `sqrt`

- **Function:** `sqrt-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Square root

---

#### `-`

- **Function:** `sub-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Subtraction

---

#### `tan`

- **Function:** `tan-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Tan

---

#### `tanh`

- **Function:** `tanh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Tanh

---

#### `y^x`

- **Function:** `y^x-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

y^x first on stack is exponent, second is base: y^x 3 2 --> 8

---

### stack.lisp 

#### `dup`

- **Function:** `dup`
- **Arity:** 0
- **Source:** stack.lisp

**Description:**

Duplicate the top element of the stack.

---

#### `pop`

- **Function:** `pop-stack`
- **Arity:** 0
- **Source:** stack.lisp

---

#### `peek`

- **Function:** `pretty-print-stack`
- **Arity:** 0
- **Source:** stack.lisp

**Description:**

Print the top 5 items of the stack in a readable format.

---

#### `swap`

- **Function:** `swap`
- **Arity:** 0
- **Source:** stack.lisp

**Description:**

Swap top and second element of the stack

---

## All Documented Functions

### `->p`

**Parameters:** `(A)`

**Source:** math.lisp

Converts a = [x, y] in Rectangular coordinates to Polar coordinates [r, theta]

---

### `->r`

**Parameters:** `(A)`

**Source:** math.lisp

Converts a = [r, theta] in Polar coordinates to Rectangular coordinates [x, y]

---

### `1/x-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Reciprocal: 1/a

---

### `10^x`

**Parameters:** `(A)`

**Source:** math.lisp

10^a

---

### `10log-fn`

**Parameters:** `(A)`

**Source:** math.lisp

10log a

---

### `abs-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Absolute value, also works for complex numbers

---

### `acos-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Acos

---

### `acosh-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Acosh

---

### `add-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Addition

---

### `asin-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Asin

---

### `asinh-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Asinh

---

### `atan-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Atan

---

### `atanh-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Atanh

---

### `chs`

**Parameters:** `(A)`

**Source:** math.lisp

Change sign a -> -a

---

### `complex-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Creates complex number a+ib

---

### `cos-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Cos

---

### `cosh-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Cosh

---

### `count-elements`

**Parameters:** `(ARRAY)`

**Source:** arrays.lisp

Count the number of elements in an array

---

### `div-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Division

---

### `drop`

**Parameters:** `(INDEX ARRAY)`

**Source:** arrays.lisp

Drop the first N elements from an array.

---

### `dup`

**Parameters:** `()`

**Source:** stack.lisp

Duplicate the top element of the stack.

---

### `eql-fn`

**Parameters:** `(A B)`

**Source:** filters.lisp

Equal, eq 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 0 0]

---

### `exp-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Exponential: e^a

---

### `fact-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Factorial ! 8 -> 40320

---

### `flatten`

**Parameters:** `(LST)`

**Source:** arrays.lisp

Flatten a nested list structure into a single list.

---

### `frac-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Returns the fractional part 1.23 -> .23

---

### `greater-equal-fn`

**Parameters:** `(A B)`

**Source:** filters.lisp

Greater than or equal, >= 5 [1 2 3 4 5 6 7] -> [0 0 0 0 1 1 1]

---

### `greater-fn`

**Parameters:** `(A B)`

**Source:** filters.lisp

Greater than, > 5 [1 2 3 4 5 6 7] -> [0 0 0 0 0 1 1]

---

### `indexof`

**Parameters:** `(VALUE ARRAY)`

**Source:** arrays.lisp

Return the index of the first occurrence of VALUE in ARRAY.    If VALUE is not found, return the length of the array.

---

### `intg-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Floor

---

### `is-header-row`

**Parameters:** `(ROW)`

**Source:** io.lisp

Check if a row contains headers

---

### `load-csv`

**Parameters:** `(FILENAME)`

**Source:** io.lisp

Loads a table from a CSV file, comma separated, row oriented

---

### `load-numbers`

**Parameters:** `(FILENAME)`

**Source:** io.lisp

Load numbers from a text file (one per line)

---

### `log-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Natural logarithm ln a

---

### `max-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Maximum

---

### `min-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Minimum

---

### `mul-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Multiplication

---

### `not-eql-fn`

**Parameters:** `(A B)`

**Source:** filters.lisp

Not Equal, neq 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 1 1]

---

### `parse-number`

**Parameters:** `(STR)`

**Source:** io.lisp

Parse a string as a number

---

### `parse-number-safe`

**Parameters:** `(STRING)`

**Source:** io.lisp

Safely parse a string to number

---

### `pick`

**Parameters:** `(INDEX ARRAY)`

**Source:** arrays.lisp

Pick an element from an array based on the index.    If index is a number, it returns the nth element.    If index is a list, it traverses the array according to the indices in the list.

---

### `pretty-print-stack`

**Parameters:** `()`

**Source:** stack.lisp

Print the top 5 items of the stack in a readable format.

---

### `reshape`

**Parameters:** `(SHAPE DATA)`

**Source:** arrays.lisp

Reshape a flat list into a multi-dimensional array based on the given shape.    The shape is a list of dimensions, e.g. (2 3) for a 2x3 matrix.

---

### `rnd-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Round

---

### `rotate`

**Parameters:** `(ARRAY)`

**Source:** arrays.lisp

Rotate clockwise, i.e. last element becomes first element

---

### `run-script`

**Parameters:** `(FILENAME)`

**Source:** io.lisp

Execute a script file line by line

---

### `shape-fn`

**Parameters:** `(ARRAY)`

**Source:** arrays.lisp

Return the shape of an array

---

### `sin-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Sin

---

### `sinh-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Sinh

---

### `smaller-equal-fn`

**Parameters:** `(A B)`

**Source:** filters.lisp

Smaller than or equal, <= 5 [1 2 3 4 5 6 7] -> [1 1 1 1 1 0 0]

---

### `smaller-fn`

**Parameters:** `(A B)`

**Source:** filters.lisp

Smaller than, < 5 [1 2 3 4 5 6 7] -> [1 1 1 1 0 0 0]

---

### `sqr-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Square a^2

---

### `sqrt-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Square root

---

### `sub-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Subtraction

---

### `swap`

**Parameters:** `()`

**Source:** stack.lisp

Swap top and second element of the stack

---

### `take`

**Parameters:** `(INDEX ARRAY)`

**Source:** arrays.lisp

Take the first N elements from an array.

---

### `tan-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Tan

---

### `tanh-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Tanh

---

### `transpose`

**Parameters:** `(MATRIX)`

**Source:** arrays.lisp

Transpose a matrix (list of lists)

---

### `where`

**Parameters:** `(ARRAY)`

**Source:** arrays.lisp

Return the indices of non-zero elements in an array.

---

### `y^x-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

y^x first on stack is exponent, second is base: y^x 3 2 --> 8

---

