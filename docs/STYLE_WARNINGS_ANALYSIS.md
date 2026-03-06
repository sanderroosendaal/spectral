# Style Warnings Analysis

Analysis of style warnings emitted when running the test suite (`sbcl --noinform --non-interactive --load tests.lisp`).

---

## Summary of Warnings

| File | Count | Category |
|------|-------|----------|
| std/io.lisp | 7 | Forward reference / cross-file |
| std/signal_processing.lisp | 2 | Order + unused var |
| std/plotting.lisp | 4 | Bogus `declare (ignore)` |
| spectral.lisp | 5 | Forward reference (same file) |

---

## 1. std/io.lisp

### 1.1 `evaluate` undefined (run-script)
- **Cause**: io.lisp is compiled before spectral.lisp has fully defined `evaluate` in the compiler's view, or the compiler treats cross-file references conservatively.
- **Impact**: None at runtime (evaluate exists when run-script executes).
- **Recommendation**: Add a forward declaration at the top of io.lisp:
  ```lisp
  (declaim (ftype (function (string) t) evaluate))
  ```

### 1.2 `write-float32-le`, `write-float64-le`, `write-int16-le` undefined (write-binary)
- **Cause**: `write-binary` (lines 171–199) calls these helpers, which are defined later in the same file (lines 202–235).
- **Impact**: None at runtime (all in same file, load order is top-to-bottom).
- **Recommendation**: Move `write-float64-le`, `write-float32-le`, `single-to-ieee754-bits`, `write-int16-le` (and any deps) to appear **before** `write-binary`. Logical order: low-level writers first, then `write-binary`.

### 1.3 `single-to-ieee754-bits` undefined (write-float32-le)
- **Cause**: `write-float32-le` (line 216) calls `single-to-ieee754-bits` (defined at 218).
- **Recommendation**: Define `single-to-ieee754-bits` before `write-float32-le` (same reorder as above).

### 1.4 `load-hdf5-fn`, `write-hdf5-fn` undefined (handler-case)
- **Cause**: `register-op` references `#'load-hdf5-fn` and `#'write-hdf5-fn`, which are defined in hdf5-io.lisp. That file is loaded inside the same `handler-case`; on HDF5 failure we never reach `register-op`, but the compiler still sees the reference.
- **Impact**: None when HDF5 fails (we never register). When HDF5 loads, the functions exist.
- **Recommendation**: Define stub functions before the handler-case that signal a clear error, e.g.:
  ```lisp
  (defun load-hdf5-fn (&rest args) (declare (ignore args)) (error "HDF5 not loaded"))
  (defun write-hdf5-fn (&rest args) (declare (ignore args)) (error "HDF5 not loaded"))
  ```
  Then hdf5-io.lisp redefines them. The compiler sees a definition and stops warning.

---

## 2. std/signal_processing.lisp

### 2.1 `convert-to-float` undefined (ensure-double-float-vector)
- **Cause**: `ensure-double-float-vector` (lines 7–9) calls `convert-to-float`, which is defined at 11–19.
- **Recommendation**: Swap order: define `convert-to-float` before `ensure-double-float-vector`.

### 2.2 Unused variable `n` (psd-fn)
- **Cause**: `(let* ((vec ...) (n (length vec)) ...)` — `n` is bound but never used.
- **Recommendation**: Remove `n` from the `let*` if not needed, or use it (e.g. for documentation or a future check). Easiest: `(let* ((vec ...) (_ (length vec)))` and `(declare (ignore _))`, or just drop it if it adds no value.

---

## 3. std/plotting.lisp

### 3.1 "reading an ignored variable" (with-plot-rows-2d, plot-fn, 3D-plot)
- **Cause**: The macro generates `(declare (ignore ,cols-sym))` but the expansion also uses `cols-sym` in `(loop for j below ,cols-sym ...)`. Declaring a variable ignored and then reading it is inconsistent.
- **Recommendation**: Remove the `(declare (ignore ,cols-sym))` line from the macro. `cols-sym` is used and should not be ignored.

---

## 4. spectral.lisp

### 4.1 `is-true` undefined (eval-node)
- **Cause**: `eval-node` (line 297) calls `is-true`, which is defined at 502.
- **Recommendation**: Move `is-true` before `eval-node`, or add:
  ```lisp
  (declaim (ftype (function (t) boolean) is-true))
  ```
  near the top after the package setup.

### 4.2 `parse-expression`, `parse`, `parse-group` undefined
- **Cause**: Mutual recursion: `parse-array` → `parse-expression`; `parse-assignment` → `parse`; `parse-if` → `parse-group`; `parse-group` → `parse-expression`. All are defined in the same compilation unit.
- **Impact**: None at runtime; SBCL compiles the whole file and resolves the refs.
- **Recommendation**: Add forward declarations near the top of spectral.lisp (after eval-node):
  ```lisp
  (declaim (ftype (function (list) (values list t)) parse-expression parse-group))
  (declaim (ftype (function (list) list) parse))
  ```
  Or move `parse`, `parse-expression`, `parse-group` before any function that calls them (harder due to mutual recursion; forward decls are simpler).

---

## Priority

| Priority | Action | Effort |
|----------|--------|--------|
| High | Fix plotting.lisp `declare (ignore)` — incorrect and confusing | Trivial |
| High | Reorder io.lisp so writers come before write-binary | Low |
| High | Reorder signal_processing: convert-to-float before ensure-double-float-vector | Trivial |
| Medium | Remove or use `n` in psd-fn | Trivial |
| Medium | Add forward decls in io.lisp (evaluate) | Trivial |
| Medium | Add HDF5 stubs in io.lisp | Low |
| Low | Add forward decls in spectral.lisp | Low |

---

## Optional: Suppress Warnings

If you prefer not to change code:

- `(declaim (sb-ext:muffle-conditions style-warning))` — muffle **all** style warnings (heavy-handed).
- `(declaim (sb-ext:muffle-conditions sb-int:package-at-variance))` — only specific conditions (narrower).

**Recommendation**: Fix the real causes above rather than muffling; the fixes improve structure and avoid misleading declarations.
