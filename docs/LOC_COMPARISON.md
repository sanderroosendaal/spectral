# Lines of Code: Lisp Prototype vs Production Implementations

This document estimates lines of code (LOC) for a production-grade Spectral implementation in Rust or C++, compared to the current Lisp prototype.

---

## Lisp Prototype (Baseline)


| Component                  | Lines      |
| -------------------------- | ---------- |
| spectral.lisp              | 597        |
| std/arrays.lisp            | 361        |
| std/io.lisp                | 388        |
| std/signal_processing.lisp | 295        |
| std/fftw-ffi.lisp          | 186        |
| std/math.lisp              | 114        |
| std/linear_algebra.lisp    | 101        |
| std/stack.lisp             | 101        |
| std/hdf5-io.lisp           | 83         |
| std/plotting.lisp          | 76         |
| std/filters.lisp           | 20         |
| std/deps.lisp              | 13         |
| **Total**                  | **~2,335** |


Lisp is compact because of dynamic typing, minimal boilerplate, and macros (`def-filter`, `def-unary-op`) that collapse repetitive definitions into single lines. Error handling is a simple `spectral-error` call; FFI uses Quicklisp and CFFI.

---

## Why C++ and Rust Will Be Larger

Production implementations need more explicit structure:


| Factor                | Lisp                 | C++ / Rust                       |
| --------------------- | -------------------- | -------------------------------- |
| **Type declarations** | Mostly implicit      | Required everywhere              |
| **Error handling**    | One function call    | `Result`/exceptions, propagation |
| **FFI**               | `ql:quickload`, CFFI | Direct bindings, build config    |
| **Code reuse**        | Macros               | Templates, traits, generics      |
| **Project structure** | Few files            | Modules, headers (C++)           |


---

## Estimated LOC by Implementation


| Implementation     | Low   | Mid   | High  | vs Lisp |
| ------------------ | ----- | ----- | ----- | ------- |
| **Lisp (current)** | —     | 2,335 | —     | 1×      |
| **Rust**           | 4,500 | 6,000 | 7,500 | ~2.5×   |
| **C++**            | 5,500 | 7,000 | 9,000 | ~3×     |


---

## Component-Level Reasoning

### Core interpreter (parser, eval, stack)

- Lisp: ~600 lines (dense; `defun`, `cond`, minimal ceremony)
- Rust/C++: 1,200–2,000 lines — explicit token types, AST enums, visitor pattern or match arms, stack as `Vec<Value>` with proper `Value` enum

### I/O (text, CSV, binary, NPY, HDF5)

- Lisp: ~550 lines across io.lisp, hdf5-io
- Rust/C++: 700–1,200 — explicit buffer handling, error types, byte-order logic; HDF5/NPY bindings add boilerplate

### Array operations

- Lisp: ~360 lines (arrays.lisp)
- Rust/C++: 500–900 — no `array-op`/`array-fn` helpers; each op is a full function; dimensional checks and indexing are explicit

### Math, filters, signal processing

- Lisp: ~430 lines (math, filters, signal_processing) — macros and `array-op` reduce repetition
- Rust/C++: 600–1,100 — each unary/binary op written out or generated; FFT bindings, bandpass/savgol logic are similar in size but more verbose

### Linear algebra, plotting

- Lisp: ~180 lines (wraps Magicl, gnuplot)
- Rust/C++: 250–400 — Eigen/ndarray or nalgebra; gnuplot subprocess or similar

---

## Why Rust Might Be Shorter Than C++

- **No header duplication** — declarations live with definitions
- `**Result` instead of exceptions** — less try/catch scaffolding
- **Cargo and crates** — parsing, serialization, and numerics reduce custom code
- **Iterators and `?`** — more compact control flow

C++ gains lines from headers, explicit template instantiation, and more manual error handling.

---

## Caveats

- **“Working”** means feature parity with the Lisp prototype, not hardened for production (that would add tests, CLI, packaging, docs).
- Estimates assume one developer using Cursor/AI assistance; hand-written code may differ.
- Actual LOC will depend on style (minimal vs defensive), use of code generation, and choice of libraries.

