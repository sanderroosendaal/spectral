# API Documentation

Generated from Lisp source files.

## Registered Operations

### `!`

- **Function:** `#'FACT-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `%`

- **Function:** `#'DIV-FN`
- **Arity:** 2
- **Source:** math.lisp

---

### `*`

- **Function:** `#'MUL-FN`
- **Arity:** 2
- **Source:** math.lisp

---

### `+`

- **Function:** `#'ADD-FN`
- **Arity:** 2
- **Source:** math.lisp

---

### `-`

- **Function:** `#'SUB-FN`
- **Arity:** 2
- **Source:** math.lisp

---

### `->p`

- **Function:** `#'->P`
- **Arity:** 1
- **Source:** math.lisp

---

### `->r`

- **Function:** `#'->R`
- **Arity:** 1
- **Source:** math.lisp

---

### `1/x`

- **Function:** `#'1/X-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `10^x`

- **Function:** `#'10^X`
- **Arity:** 1
- **Source:** math.lisp

---

### `<`

- **Function:** `#'SMALLER-FN`
- **Arity:** 2
- **Source:** filters.lisp

---

### `<=`

- **Function:** `#'SMALLER-EQUAL-FN`
- **Arity:** 2
- **Source:** filters.lisp

---

### `>`

- **Function:** `#'GREATER-FN`
- **Arity:** 2
- **Source:** filters.lisp

---

### `>=`

- **Function:** `#'GREATER-EQUAL-FN`
- **Arity:** 2
- **Source:** filters.lisp

---

### `abs`

- **Function:** `#'ABS-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `acos`

- **Function:** `#'ACOS-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `acosh`

- **Function:** `#'ACOSH-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `asin`

- **Function:** `#'ASIN-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `asinh`

- **Function:** `#'ASINH-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `atan`

- **Function:** `#'ATAN-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `atanh`

- **Function:** `#'ATANH-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `chs`

- **Function:** `#'CHS`
- **Arity:** 1
- **Source:** math.lisp

---

### `complex`

- **Function:** `#'COMPLEX-FN`
- **Arity:** 2
- **Source:** math.lisp

---

### `cos`

- **Function:** `#'COS-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `cosh`

- **Function:** `#'COSH-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `eq`

- **Function:** `#'EQL-FN`
- **Arity:** 2
- **Source:** filters.lisp

---

### `exp`

- **Function:** `#'EXP-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `frac`

- **Function:** `#'FRAC-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `intg`

- **Function:** `#'INTG-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `ln`

- **Function:** `#'LOG-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `log`

- **Function:** `#'10LOG-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `max`

- **Function:** `#'MAX-FN`
- **Arity:** 2
- **Source:** math.lisp

---

### `min`

- **Function:** `#'MIN-FN`
- **Arity:** 2
- **Source:** math.lisp

---

### `neq`

- **Function:** `#'NOT-EQL-FN`
- **Arity:** 2
- **Source:** filters.lisp

---

### `rnd`

- **Function:** `#'RND-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `sin`

- **Function:** `#'SIN-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `sinh`

- **Function:** `#'SINH-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `sqrt`

- **Function:** `#'SQRT-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `square`

- **Function:** `#'SQR-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `tan`

- **Function:** `#'TAN-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `tanh`

- **Function:** `#'TANH-FN`
- **Arity:** 1
- **Source:** math.lisp

---

### `y^x`

- **Function:** `#'Y^X-FN`
- **Arity:** 2
- **Source:** math.lisp

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

### `div-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

Division

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

### `intg-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Floor

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

### `rnd-fn`

**Parameters:** `(A)`

**Source:** math.lisp

Round

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

### `y^x-fn`

**Parameters:** `(A B)`

**Source:** math.lisp

y^x first on stack is exponent, second is base: y^x 3 2 --> 8

---

