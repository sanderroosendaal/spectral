# Spectral: Functional Requirements for C++ Production Implementation

This document specifies the behavior of Spectral, a stack-based array language for scientific computing. It is intended as the authoritative reference for developing a production C++ implementation that is compatible with the existing Lisp prototype.

---

## 1. Overview

### 1.1 Purpose

Spectral is a **stack-based array language** for scientific computing, targeting detector data, time series, and experimental analysis. Data flows **right-to-left** through operations, and execution is RPN-like (stack-based).

### 1.2 Design Principles

- **Right-to-left data flow**: Natural reading of processing pipelines
- **Stack-based execution**: No operator precedence; arguments are pushed then popped
- **Scientific focus**: Built for detector data, time series, experimental analysis
- **Performance-oriented**: Designed for large arrays and real-time processing
- **Graceful degradation**: Optional features (FFT, linear algebra, HDF5) degrade cleanly when dependencies are unavailable

---

## 2. Execution Model

### 2.1 Pipeline

1. **Preprocess**: Remove comments (from `;` to end of line), expand pipe alternation `|`, add spacing around brackets and single-character tokens
2. **Tokenize**: `read`-style tokenization on preprocessed string
3. **Parse**: Expressions → AST
4. **Eval**: Interpreter evaluates AST nodes; operations pop arguments from stack, compute, push result

### 2.2 Stack Semantics

- **Push order**: Rightmost token is pushed first (popped last)
- **Binary ops**: `op a b` → push `b`, push `a`, then `funcall(op, a, b)` where `a` = top of stack, `b` = second
- **Semantic convention**: Op receives `(a, b)` so that left-to-right reading matches: `+ 3 5` ⇒ `3 + 5`
- **Subtraction/division**: Use `op(b, a)` so `- 3 10` ⇒ `10 - 3 = 7`, `% 3 15` ⇒ `15 / 3 = 5`

### 2.3 Supported Arity

The evaluator must support operations with arity **0, 1, 2, and 3**:

- **0**: Nullary (e.g. constants, stack ops like `dup`)
- **1**: Unary (e.g. `sin`, `shape`, `load`)
- **2**: Binary (e.g. `+`, `>`, `load-csv` with headers)
- **3**: Ternary (e.g. `write-hdf5 filename path data`)

---

## 3. Data Types and Conventions

### 3.1 Scalars

| Type   | Description |
|--------|-------------|
| Integer | Arbitrary precision; coerced to `double` in many ops |
| Float  | `double` (64-bit IEEE 754 LE) is the canonical numeric type |
| Complex | `(real, imag)` pairs; supported by `complex`, `re`, `im`, `fft`, `cfft`, `ifft` |
| String | Used for filenames, paths; passed through unchanged |
| Boolean | `1` = true, `0` = false; non-zero and non-empty also truthy |

### 3.2 Arrays

- **Layout**: Row-major (C order)
- **Element type**: `double` canonical; implementations may support `float`, `int32`, `int16` for I/O
- **Rank**: 1D (vector), 2D (matrix); higher ranks supported for many ops (reshape, flatten, etc.)
- **Indexing**: Zero-based

### 3.3 Truthiness (`is_true`)

- Number: `value != 0`
- Array: `length > 0`
- List: `length > 0`
- Other: true

---

## 4. Operator Reference

### 4.1 Argument Order Convention

For all documented APIs, parameters are listed in **evaluation order** (first popped = first parameter). Spectral source uses right-to-left: `op x y` means push `y`, push `x`, then `op(x, y)`.

### 4.2 Constants (Arity 0)

| Name     | Value |
|----------|-------|
| `pi`     | π (double) |
| `e`      | e (Euler's number) |
| `epsilon`| `double_epsilon` (machine epsilon) |

### 4.3 Stack Operations (Arity 0)

| Op   | Alias | Behavior |
|------|-------|----------|
| `dup`| `d`   | Duplicate top of stack |
| `swap`| —   | Swap top two elements |
| `pop`| —    | Pop and discard top |
| `peek`| —   | Print top of stack (side effect) |

### 4.4 Arithmetic (Arity 2)

| Op | Signature | Behavior | Example |
|----|-----------|----------|---------|
| `+` | `(a, b)` | `a + b` | `+ 3 5` → 8 |
| `-` | `(a, b)` | `b - a` | `- 3 10` → 7 |
| `*` | `(a, b)` | `a * b` | `* 4 5` → 20 |
| `%` | `(a, b)` | `b / a` | `% 3 15` → 5 |

**Broadcasting**: Scalar vs array and array vs array, element-wise. Arrays must have identical dimensions for array-array ops.

### 4.5 Unary Math (Arity 1)

| Op | Behavior |
|----|-----------|
| `abs` | Absolute value |
| `chs` | Change sign: `-x` |
| `intg` | Floor |
| `frac` | Fractional part (`x - floor(x)`) |
| `rnd` | Round |
| `square` | `x²` |
| `sqrt` | √x |
| `exp` | e^x |
| `ln` | Natural log |
| `log` | Log base 10 |
| `10^x` | 10^x |
| `1/x` | Reciprocal |
| `!` | Factorial |

### 4.6 Binary Math (Arity 2)

| Op | Signature | Behavior |
|----|-----------|----------|
| `y^x` | `(x, y)` | `y^x` (exponent first, base second on stack) |
| `max` | `(a, b)` | Element-wise max |
| `min` | `(a, b)` | Element-wise min |

### 4.7 Trigonometry (Arity 1)

`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh` — standard definitions, element-wise on arrays.

### 4.8 Complex (Arity 2, 1)

| Op | Signature | Behavior |
|----|-----------|----------|
| `complex` | `(re, im)` | Construct complex `re + i*im` |
| `re` | `(z)` | Real part |
| `im` | `(z)` | Imaginary part |

### 4.9 Coordinate Conversion (Arity 1)

| Op | Input | Output |
|----|-------|--------|
| `->P` | `[x, y]` | `[r, θ]` (rectangular → polar) |
| `->R` | `[r, θ]` | `[x, y]` (polar → rectangular) |

**Note**: Expect list/vector of length 2.

### 4.10 Reduction Operators (Prefix `/`)

Syntax: `/op array`. Reduce array to scalar (1D) or along first axis (2D).

| Op | Behavior |
|----|-----------|
| `/+` | Sum |
| `/*` | Product |
| `/max` | Maximum |
| `/min` | Minimum |

**1D**: Fold left over elements.  
**2D**: Reduce along first axis; output shape = `(dims[1], dims[2], ...)`.

**Error**: Reduction on scalar → error: "Reduction expects an array".

### 4.11 Scan Operators (Prefix `&`)

Syntax: `&op array`. Inclusive prefix scan; output same length as input.

| Op | Behavior |
|----|-----------|
| `&+` | Cumulative sum |
| `&*` | Cumulative product |

**Error**: Scan on scalar → error: "Scan expects an array".

### 4.12 Filters (Arity 2)

Threshold: `op threshold array` → mask `[0|1]` (1 where condition holds).

| Op | Condition | Example |
|----|-----------|---------|
| `>` | `array > threshold` | `> 5 [1..7]` → `[0 0 0 0 0 1 1]` |
| `>=` | `>=` | |
| `<` | `array < threshold` | |
| `<=` | `<=` | |
| `eq` | `array == threshold` | |
| `neq` | `array != threshold` | |

### 4.13 Array Operations

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `range` | 1 | `(n)` | `[0, 1, ..., n-1]` |
| `shape` | 1 | `(arr)` | Dimensions as list/tuple |
| `size` | 1 | `(arr)` | Total element count |
| `rank` | 1 | `(arr)` | Number of dimensions |
| `length` | 1 | `(arr)` | Length along first axis |
| `nrow` | 1 | `(arr)` | Rows (2D) |
| `ncol` | 1 | `(arr)` | Columns (2D) |
| `reshape` | 2 | `(shape, arr)` | Reshape; total size must match |
| `transpose` | 1 | `(arr)` | Transpose 2D matrix |
| `flatten` | 1 | `(arr)` | Flatten to 1D row-major |
| `take` | 2 | `(n, arr)` | First n elements (along first axis) |
| `drop` | 2 | `(n, arr)` | Drop first n elements |
| `pick` | 2 | `(index, arr)` | Element at index (scalar or vector) |
| `slice` | 2 | `(row, arr)` | Row `row` from 2D array |
| `sub` | 2 | `(idx, arr)` | Sub-array from index |
| `reverse` | 1 | `(arr)` | Reverse along first axis |
| `rotate` | 1 | `(arr)` | Rotate (last → first) |
| `stack` | 2 | `(a, b)` | Stack two arrays along new axis |
| `zeros` | 1 | `(dims)` | Array of zeros |
| `ones` | 1 | `(dims)` | Array of ones |
| `mesh-x` | 2 | `(m, n)` | Meshgrid X (M×N) |
| `mesh-y` | 2 | `(m, n)` | Meshgrid Y (M×N) |
| `where` | 1 | `(arr)` | Indices of non-zero elements |
| `idx` | 2 | `(value, arr)` | Index of first occurrence of value |

### 4.14 Statistics

| Op | Arity | Behavior |
|----|-------|----------|
| `mean` | 1 | Mean of elements (flattens if needed) |
| `std` | 1 | Standard deviation |
| `mean-std` | 1 | Returns (mean, std) |
| `corrcoeff` | 1 | Correlation coefficients (2D: first row = X, others = Y) |

### 4.15 Signal Processing

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `fft` | 1 | `(signal)` | Forward FFT (real → complex) |
| `ifft` | 1 | `(spectrum)` | Inverse FFT |
| `cfft` | 1 | `(signal)` | Forward FFT (complex input) |
| `bandpass` | 2 | `([f_low, f_high, fs], signal)` | Keep freq in [f_low, f_high] Hz |
| `lowpass` | 2 | `([f_cutoff, fs], signal)` | Keep freq &lt; f_cutoff |
| `highpass` | 2 | `([f_cutoff, fs], signal)` | Keep freq &gt; f_cutoff |
| `bandstop` | 2 | `([f_low, f_high, fs], signal)` | Zero freq in [f_low, f_high] |
| `smooth` | 2 | `(window_size, signal)` | Boxcar moving average |
| `savgol` | 2 | `([window, poly_order], signal)` | Savitzky-Golay smoothing |
| `find-peaks` | 1 | `(signal)` | Indices of strict local maxima |
| `find-valleys` | 1 | `(signal)` | Indices of strict local minima |
| `psd` | 1 | `(signal)` | Power spectral density |
| `detrend` | 1 | `(signal)` | Remove linear trend |
| `differentiate` | 1 | `(signal)` | First derivative (central diff) |

**find-peaks / find-valleys**: Strict comparison: `x[i] > x[i-1]` and `x[i] > x[i+1]`. Edges: one neighbor only. Single element → `[0]`. Flat regions → no peaks.

**Smooth**: Partial windows at edges; output same length as input.

**Savgol**: Window length forced odd; polynomial order ≤ window-1. Reflection at edges.

### 4.16 Linear Algebra (Optional)

Requires LAPACK/BLAS. If unavailable: graceful degradation (ops undefined or return error).

| Op | Alias | Arity | Behavior |
|----|-------|-------|----------|
| `mmult` | `@` | 2 | Matrix multiply |
| `det` | — | 1 | Determinant |
| `trace` | — | 1 | Trace |
| `triu` | — | 1 | Upper triangular |
| `tril` | — | 1 | Lower triangular |
| `dagger` | `conjugate-transpose` | 1 | Conjugate transpose |
| `inv` | — | 1 | Matrix inverse |
| `eig` | — | 1 | Eigenvalues and eigenvectors |

---

## 5. I/O Operations

### 5.1 Text I/O

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `load` | 1 | `(filename)` | One number per line → 1D vector. Invalid line → 0. |
| `write` | 2 | `(filename, data)` | Write 1D vector, one number per line |

### 5.2 CSV I/O

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `load-csv` | 1 | `(filename)` | Row-oriented CSV. If first row has non-numeric cell → treat as header; return `(headers, data)`. Otherwise numeric 2D array. |
| `write-csv` | 2 | `(filename, matrix)` | 2D array to CSV, comma-separated, row-oriented |

### 5.3 Script Execution

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `run` | 1 | `(filename)` | Execute script file line-by-line; skip blank lines and comments (`;`) |

### 5.4 Binary Format (.sdat)

**Extension**: `.sdat` (Spectral data)

**Header** (little-endian):

| Offset | Size | Type | Description |
|--------|------|------|-------------|
| 0 | 4 | int32 | Type code: -1 float64, -2 float32, -3 int32, -4 int16 |
| 4 | 4 | uint32 | Bytes per element |
| 8 | 4 | uint32 | ndims (1–8) |
| 12 | 4×ndims | uint32 | Dimension sizes |

**Body**: Row-major binary data, little-endian.

**Type codes**:

| Code | Type | Bytes/elem |
|------|------|------------|
| -1 | float64 | 8 |
| -2 | float32 | 4 |
| -3 | int32 | 4 |
| -4 | int16 | 2 |

**API**:

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `load-binary` | 1 | `(filename)` | Load .sdat; always return `double` array |
| `write-binary` | 2 | `(filename, data)` | Write array; preserve float64/32/int32/16; others coerce to float64 |

**Constraints**: Max 8 dimensions. IEEE 754 LE for floats.

### 5.5 NPY Format

**Spec**: NEP 1 (NumPy .npy format)

**Supported**:
- Dtypes: `<f8` (float64), `<i4` (int32)
- Ranks: 1D, 2D
- Order: C only (`fortran_order: False`)

**Reject**: `fortran_order: True` → error.

**Header**: ASCII dict `{'descr': '<f8', 'fortran_order': False, 'shape': (n,) or (r,c), }` padded to (6+2+2+hlen) mod 16 = 0.

**API**:

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `load-npy` | 1 | `(filename)` | Load .npy; return double array |
| `write-npy` | 2 | `(filename, data)` | Write .npy; coerce to float64 if needed |

### 5.6 HDF5 (Optional)

**Requires**: libhdf5. If unavailable: `load-hdf5` and `write-hdf5` disabled (no-op or clear error).

**API**:

| Op | Arity | Signature | Behavior |
|----|-------|-----------|----------|
| `load-hdf5` | 2 | `(filename, path)` | Read dataset at path; return double array |
| `write-hdf5` | 3 | `(filename, path, data)` | Write dataset; overwrite if exists |

**Scope**: Single dataset read/write. No groups, no attributes. Double-precision only.

---

## 6. Language Syntax

### 6.1 Literals

- **Numbers**: `123`, `3.14`, `1e-5`
- **Strings**: `"path/file.dat"`
- **Arrays**: `[1 2 3]`, `[[1 2][3 4]]`
- **Groups**: `(expr1 expr2 expr3)` — sequence of expressions

### 6.2 Comments

`;` to end of line.

### 6.3 Pipe Alternation

`(a|b|c)` → `((a)(b)(c))` — alternate branches (used with `if`).

### 6.4 Conditionals

```
((then)(else)) if condition
```

- Evaluate `condition`; if truthy, evaluate `then`, else `else`.

### 6.5 Assignment

```
name = expression
```

- Variables: `name` stores result of `expression`
- Functions: if result is function, store as callable

### 6.6 Array Literals with Variables

- `[1 2 x]` — x from variables
- `[[1 2 x][3 4 y]]` — 2D with variable refs
- `[A B]` — A, B as arrays (stack/concatenate)
- `[(range 5)]` — expression in array → row

### 6.7 Rectangular Arrays

All rows must have same length. Ragged arrays → error.

---

## 7. Error Handling

### 7.1 Mandatory Errors

| Condition | Message / Behavior |
|-----------|--------------------|
| Stack underflow | "Stack underflow: cannot pop from an empty stack" |
| Unknown reduction op | "Unknown reduction operator: ..." |
| Unknown scan op | "Unknown scan operator: ..." |
| Wrong type (e.g. reduction on scalar) | "Reduction expects an array, got ..." |
| Arity not supported | "Functions of arity X are not implemented" |
| Dimension mismatch | "Mismatched array dimensions" |
| Invalid .sdat (ndims>8, unknown typecode) | "Invalid .sdat" / "Unknown .sdat type code" |
| NPY fortran_order True | "NPY: fortran_order True not supported" |
| NPY unsupported dtype | "NPY: only <f8 and <i4 supported" |
| HDF5 unavailable | Warn and disable ops |

### 7.2 Numeric Comparison

Tests use tolerance `1e-10` for float equality.

---

## 8. Dependencies

### 8.1 Required

- No external runtime deps for core + text/CSV + .sdat binary I/O

### 8.2 Optional (Graceful Degradation)

| Feature | Dependency |
|---------|------------|
| FFT | FFTW3 |
| Linear algebra | LAPACK/BLAS (Magicl, etc.) |
| HDF5 | libhdf5 |
| Plotting | gnuplot |

---

## 9. Test Compatibility

The C++ implementation should pass (or produce equivalent results for) the test suite in `tests.lisp`. Key behaviors to preserve:

1. **Arithmetic**: `- 3 10` → 7, `% 3 15` → 5
2. **Stack**: `* d 2` → 4, `swap 1 2` → 2 on top
3. **Reduction**: `/+ [1 2 3 4 5]` → 15; `/+ [[1 2][3 4]]` → `[4 6]`
4. **Scan**: `&+ [1 2 3 4 5]` → `[1 3 6 10 15]`
5. **find-peaks**: Strict local maxima; edge cases per tests
6. **Filters**: bandpass, smooth, savgol — length preserved, specific numeric behavior
7. **Binary/NPY/HDF5**: Roundtrip identity, shape/size preservation, special values (0, -1)

---

## 10. Non-Functional Requirements

- **Performance**: Optimize for large arrays; consider SIMD, cache locality
- **Memory**: Row-major layout; avoid unnecessary copies
- **Portability**: Little-endian for binary I/O; IEEE 754
- **Embedding**: Clean C/C++ API for use as library
- **Scripting**: REPL and `run` for interactive and batch use

---

## Appendix A: Quick Reference — Operator List

**Stack**: `dup`/`d`, `swap`, `pop`, `peek`  
**Arithmetic**: `+`, `-`, `*`, `%`  
**Math**: `abs`, `chs`, `intg`, `frac`, `rnd`, `square`, `sqrt`, `exp`, `ln`, `log`, `10^x`, `1/x`, `!`, `y^x`, `max`, `min`  
**Trig**: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`  
**Complex**: `complex`, `re`, `im`  
**Reduction**: `/+`, `/*`, `/max`, `/min`  
**Scan**: `&+`, `&*`  
**Filters**: `>`, `>=`, `<`, `<=`, `eq`, `neq`  
**Arrays**: `range`, `shape`, `size`, `rank`, `length`, `nrow`, `ncol`, `reshape`, `transpose`, `flatten`, `take`, `drop`, `pick`, `slice`, `sub`, `reverse`, `rotate`, `stack`, `zeros`, `ones`, `mesh-x`, `mesh-y`, `where`, `idx`  
**Stats**: `mean`, `std`, `mean-std`, `corrcoeff`  
**Signal**: `fft`, `ifft`, `cfft`, `bandpass`, `lowpass`, `highpass`, `bandstop`, `smooth`, `savgol`, `find-peaks`, `find-valleys`, `psd`, `detrend`, `differentiate`  
**Linear algebra**: `mmult`/`@`, `det`, `trace`, `triu`, `tril`, `dagger`, `conjugate-transpose`, `inv`, `eig`  
**I/O**: `load`, `write`, `load-csv`, `write-csv`, `run`, `load-binary`, `write-binary`, `load-npy`, `write-npy`, `load-hdf5`, `write-hdf5`  
**Plotting**: `plot`, `format-plot`, `surf`, `3d-plot`
