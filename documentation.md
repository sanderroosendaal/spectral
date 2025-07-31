# API Documentation

Generated from Lisp source files.

## Registered Operations

### `!`

- **Function:** `fact-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Factorial ! 8 -> 40320

---

### `%`

- **Function:** `div-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Division

---

### `*`

- **Function:** `mul-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Multiplication

---

### `+`

- **Function:** `add-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Addition

---

### `-`

- **Function:** `sub-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Subtraction

---

### `->p`

- **Function:** `->p`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Converts a = [x, y] in Rectangular coordinates to Polar coordinates [r, theta]

---

### `->r`

- **Function:** `->r`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Converts a = [r, theta] in Polar coordinates to Rectangular coordinates [x, y]

---

### `1/x`

- **Function:** `1/x-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Reciprocal: 1/a

---

### `10^x`

- **Function:** `10^x`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

10^a

---

### `abs`

- **Function:** `abs-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Absolute value, also works for complex numbers

---

### `acos`

- **Function:** `acos-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Acos

---

### `acosh`

- **Function:** `acosh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Acosh

---

### `asin`

- **Function:** `asin-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Asin

---

### `asinh`

- **Function:** `asinh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Asinh

---

### `atan`

- **Function:** `atan-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Atan

---

### `atanh`

- **Function:** `atanh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Atanh

---

### `chs`

- **Function:** `chs`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Change sign a -> -a

---

### `complex`

- **Function:** `complex-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Creates complex number a+ib

---

### `cos`

- **Function:** `cos-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Cos

---

### `cosh`

- **Function:** `cosh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Cosh

---

### `exp`

- **Function:** `exp-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Exponential: e^a

---

### `frac`

- **Function:** `frac-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Returns the fractional part 1.23 -> .23

---

### `intg`

- **Function:** `intg-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Floor

---

### `ln`

- **Function:** `log-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Natural logarithm ln a

---

### `log`

- **Function:** `10log-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

10log a

---

### `max`

- **Function:** `max-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Maximum

---

### `min`

- **Function:** `min-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

Minimum

---

### `rnd`

- **Function:** `rnd-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Round

---

### `sin`

- **Function:** `sin-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Sin

---

### `sinh`

- **Function:** `sinh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Sinh

---

### `sqrt`

- **Function:** `sqrt-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Square root

---

### `square`

- **Function:** `sqr-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Square a^2

---

### `tan`

- **Function:** `tan-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Tan

---

### `tanh`

- **Function:** `tanh-fn`
- **Arity:** 1
- **Source:** math.lisp

**Parameters:** `(A)`

**Description:**

Tanh

---

### `y^x`

- **Function:** `y^x-fn`
- **Arity:** 2
- **Source:** math.lisp

**Parameters:** `(A B)`

**Description:**

y^x first on stack is exponent, second is base: y^x 3 2 --> 8

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

