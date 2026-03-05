# Spectral Lisp Codebase: Critical Review and Refactoring Opportunities

## Executive Summary

The Lisp prototype is functional and reasonably structured, but there are dead code paths, duplication, inconsistent error handling, and several areas that would benefit from refactoring before or during a C++ port. This document catalogs issues by severity and suggests concrete refactoring steps.

---

## 1. Dead / Unused Code

### 1.1 `array-op-list` and `array-fn-list` (spectral.lisp:106–123)

**Issue:** These functions handle list inputs for binary/unary ops but are **never called**. `array-op` and `array-fn` only handle `numberp` and `arrayp`; `listp` falls through to an error.

**Evidence:** No call sites in the codebase. Documentation mentions them but they appear to be vestigial.

**Refactor:** Either remove them and update documentation, or wire `array-op`/`array-fn` to delegate to them when inputs are lists (e.g. for `->P`/`->R` outputs).

### 1.2 `strip-token` (spectral.lisp:167–171)

**Issue:** Defined but never used.

**Refactor:** Remove or use (e.g. for a future lexer that strips `/` or `&` from reduction/scan tokens).

### 1.3 `handle-error` (errors.lisp)

**Issue:** `handle-error` is defined but never called. The REPL and evaluator use `handler-case`/`format` directly.

**Additional bug:** Both branches `setf` `error-message` to the result of `format stream ...`, which returns `nil`, so `error-message` becomes `nil` and the formatted string is discarded.

**Refactor:** Remove if truly unused, or fix the logic and integrate into error handling (e.g. in `run-script` for file/line context).

---

## 2. Code Duplication

### 2.1 Little-endian read/write (io.lisp)

**Issue:** `write-uint32-le`, `read-uint32-le`, `write-uint16-le`, `read-uint16-le`, `write-int32-le`, `read-int32-le` follow the same pattern. Similar repetition for float/int encoding.

**Refactor:** Introduce a small `binary-io` helper module with generic `read-le`/`write-le` for fixed widths, or at least share the byte-loop logic.

### 2.2 `ensure-vector` / `coerce-to-float` pattern (signal_processing.lisp)

**Issue:** Multiple functions use `(if (listp x) (coerce x 'vector) x)` and `convert-to-float`-like logic.

**Refactor:** Centralize `ensure-vector` and `ensure-double-float-vector` and reuse.

### 2.3 Filter functions (filters.lisp)

**Issue:** All six filter ops share the same structure: `(array-op (lambda (b a) (if (pred a b) 1 0)) a b)`.

**Refactor:** Use a macro or higher-order function:

```lisp
(defmacro def-filter (name pred)
  `(defun ,name (a b)
     (array-op (lambda (b a) (if (,pred a b) 1 0)) a b)))
(def-filter greater-fn >)
(def-filter smaller-fn <)
;; etc.
```

### 2.4 Math wrappers (math.lisp)

**Issue:** Many unary ops are one-liners: `(defun sin-fn (a) (array-fn #'sin a))`.

**Refactor:** Macro `(def-unary-op sin sin-fn)` to reduce boilerplate and registration.

### 2.5 Array-stack copy loops (arrays.lisp)

**Issue:** The same pattern appears multiple times:

```lisp
(loop for i from 0 below (array-total-size array1) do
  (setf (row-major-aref new-array i) (row-major-aref array1 i)))
(loop for i from (array-total-size array1) below size do
  (setf (row-major-aref new-array i) (row-major-aref array2 (- i ...))))
```

**Refactor:** Extract `copy-array-range` and `concat-arrays-along-axis` helpers.

---

## 3. Structural Issues

### 3.1 Load order and package (spectral.lisp)

**Issue:** Std libs are loaded with `(load (merge-pathnames "std/..." *spectral-root*))` but `linear_algebra.lisp` is loaded inside `arrays.lisp` with a relative path `(load "std/linear_algebra.lisp")`, which can break depending on `*default-pathname-defaults*`. HDF5 uses `(truename *load-truename*)` with a different path resolution.

**Refactor:** Use a single path resolution strategy (e.g. `*spectral-root*`) for all std loads.

### 3.2 Quickload at top level (multiple files)

**Issue:** `ql:quickload` runs at load time in `io.lisp`, `arrays.lisp`, `signal_processing.lisp`, `plotting.lisp`. This slows startup and couples loading to Quicklisp.

**Refactor:** Move dependency loading to a single `deps.lisp` or startup section, or use ASDF system definitions.

### 3.3 Mixed list vs array semantics

**Issue:** `->P` and `->R` expect and return lists. `array-op` expects arrays. Passing `->P [x y]` yields a list; using that in a subsequent op can fail. `check-rectangular` and `compute-dimensions` also mix list/array handling.

**Refactor:** Standardize: either coerce `->P`/`->R` to return vectors (or arrays), or extend `array-op`/`array-fn` to accept lists and delegate to `array-op-list`/`array-fn-list`.

---

## 4. Potential Bugs

### 4.1 Destructive modification in `array-stack` (arrays.lisp:232)

**Issue:** `(new-dims (push 2 dims1))` mutates `dims1`. `array-dimensions` may return a fresh list, but mutation is fragile and unclear.

**Refactor:** Use `(list* 2 dims1)` or `(cons 2 (copy-list dims1))` to avoid mutation.

### 4.2 Variable shadowing in `reduce-array` (spectral.lisp:184)

**Issue:** `(loop for i below (first dims) collect ...)` shadows the outer `i` from `(dotimes (i (array-total-size result) ...)`.

**Refactor:** Use a different loop variable (e.g. `k`) for the inner loop.

### 4.3 `run-script` never passes filename/line to `evaluate` (io.lisp:115)

**Issue:** Comment says "need to add filename line-count" but they are never used. Errors don't report script location.

**Refactor:** Thread filename and line number into evaluation (e.g. dynamic variables) and use them in error messages.

### 4.4 `write-hdf5` arity 3 vs evaluator (spectral.lisp)

**Issue:** `write-hdf5` is registered with arity 3, but the evaluator only supports arity 0–2. A call to `write-hdf5` will signal "Functions of arity 3 are not implemented".

**Refactor:** Extend `eval-node` to support arity 3 for the `:op` case, or document that `write-hdf5` is not yet callable from Spectral.

---

## 5. Error Handling

### 5.1 Inconsistent error messages

**Issue:** Some errors are user-facing ("Reduction expects an array, got ..."), others are implementation-oriented ("Invalid inputs: ~S and ~S"). Format and tone vary.

**Refactor:** Define `spectral-error` and standard format strings; use them consistently.

### 5.2 Missing input validation

**Issue:** Several ops assume valid input (e.g. `pick` with out-of-range index, `reshape` with incompatible shape). Errors may be obscure.

**Refactor:** Add explicit validation at API boundaries with clear messages.

### 5.3 `write-csv` missing `ensure-directories-exist` (io.lisp)

**Issue:** `load-binary`, `write-binary`, etc. don't ensure parent directories exist. `write-csv` likewise. Failure mode is a low-level file error.

**Refactor:** Add `ensure-directories-exist` before writes, or document that the caller must provide an existing path.

---

## 6. Performance

### 6.1 `spectral-color-text` (spectral.lisp:38–47)

**Issue:** `(concatenate 'string result ...)` in a loop allocates a new string each iteration.

**Refactor:** Use `(make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)` and `vector-push-extend`, or collect parts and `apply #'concatenate` once.

### 6.2 `array-op` / `array-fn` (spectral.lisp)

**Issue:** Row-major loops are correct but unoptimized. No type declarations, no `declare (optimize (speed 3))`, no use of `array-operations` for element-wise ops where applicable.

**Refactor:** Consider `aops:each` or similar for hot paths; add declarations for arrays.

### 6.3 Reduction over 2D arrays (spectral.lisp:179–188)

**Issue:** `reduce-array` for 2D uses `(apply #'aref a (cons i idx))` and allocates in a loop. For large arrays this can be costly.

**Refactor:** Use row-major indexing to avoid `apply` and list allocation.

---

## 7. Style and Maintainability

### 7.1 Indentation

**Issue:** Mix of 2-space and 4-space indent; tabs in some files. Inconsistent alignment.

**Refactor:** Enforce a single style (e.g. 2-space) and apply project-wide.

### 7.2 Magic numbers

**Issue:** e.g. `10` (print limit in pretty-print), `5` (stack items in peek), `8` (max ndims in .sdat), `16` (NPY padding). Scattered without constants.

**Refactor:** Define `*max-ndims*`, `*npy-alignment*`, etc., in one place.

### 7.3 Comment quality

**Issue:** Some functions have good docstrings; others have none or outdated comments (e.g. "HDF5 support not yet implemented" in README was fixed elsewhere).

**Refactor:** Add or refresh docstrings for public APIs; remove obsolete comments.

---

## 8. Test and Documentation Gaps

### 8.1 No tests for error paths

**Issue:** Tests focus on success. Invalid inputs, underflow, type errors are not systematically tested.

**Refactor:** Add an "Error handling" test group with `assert-error`-style checks.

### 8.2 `range-fn` docstring wrong (arrays.lisp:3)

**Issue:** Docstring says "range 9: [0 1 2 3 4 5 6 7 8 9]" but `range 9` produces 9 elements (0..8), not 10.

**Refactor:** Fix docstring to match implementation.

### 8.3 Duplicate fixture scripts

**Issue:** `write-fixtures-minimal.lisp` and `generate-binary-fixtures.lisp` overlap. `generate-binary-fixtures` used to produce different `vec_special`; that was corrected but the duplication remains.

**Refactor:** Consolidate into one script or make one call the other; document which to use.

---

## 9. Refactoring Priority Matrix

| Priority | Category              | Effort | Impact    |
|----------|-----------------------|--------|-----------|
| High     | Dead code removal     | Low    | Clarity   |
| High     | array-stack mutation  | Low    | Correctness |
| High     | write-hdf5 arity 3    | Low    | Feature completeness |
| High     | reduce-array shadowing| Low    | Correctness |
| Medium   | Filter macro          | Low    | DRY       |
| Medium   | Binary I/O helpers    | Medium | DRY       |
| Medium   | Load path consistency | Low    | Portability |
| Medium   | Error message consistency | Medium | UX   |
| Low      | Math wrappers macro   | Low    | DRY       |
| Low      | spectral-color-text perf | Low | Startup |
| Low      | ASDF system definition| Medium | Tooling  |

---

## 10. Recommended First Steps

1. **Remove or wire dead code:** `array-op-list`, `array-fn-list`, `strip-token`, `handle-error`.
2. **Fix bugs:** `array-stack` mutation, `reduce-array` variable shadowing, `write-hdf5` arity.
3. **Introduce filter macro** to reduce repetition in `filters.lisp`.
4. **Unify std load paths** using `*spectral-root*`.
5. **Add error-path tests** for stack underflow, invalid reduction, type errors.

These steps improve correctness and maintainability without large structural changes. Deeper refactors (ASDF, shared binary I/O layer, full list/array unification) can follow as needed for the C++ port or long-term maintenance.
