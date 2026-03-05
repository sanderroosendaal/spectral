# Proposal: HDF5 and Binary File Support for Spectral

## Summary

Add binary array I/O and optional HDF5 support to `std/io.lisp`, following Spectral's existing patterns (graceful degradation, minimal API). Implementation target: **~120–180 lines** total.

---

## Part 1: Binary Array I/O

### Options Considered

| Approach | Pros | Cons |
|----------|------|------|
| **Custom minimal format** | Zero deps, ~50 lines, full control | Not interoperable with NumPy/MATLAB |
| **NPY reader/writer** | Interoperable with Python/NumPy, well-specified (NEP 1) | ~100 lines, only common dtypes (float64, int32) |
| **MDA-style format** | Simple, used in neuroscience (MountainLab) | Niche, extra format to maintain |
| **binary-types / flexi-streams** | Library support | Extra dependency, more abstraction |

### Recommendation: Two-Tier Approach

**Tier A — Raw binary (no dependencies)**

A minimal header + raw data format, MDA-inspired. Single file, one array. Use extension `.sdat` (Spectral data) or `.bin`.

**Header (20 bytes fixed):**
- 4 bytes: type code (signed 32-bit): `-1` float64, `-2` float32, `-3` int32, `-4` int16
- 4 bytes: bytes per element (redundant but self-documenting)
- 4 bytes: number of dimensions (1–8)
- 4 bytes × ndims: dimension sizes (little-endian)

Then raw row-major data. **~40–50 lines** for read + write.

**Tier B — NPY support (optional, no external libs)**

Implement a subset of the NPY format (NEP 1): read/write float64 and int32 1D/2D arrays. The format is ASCII header + binary payload; straightforward to parse. Enables `numpy.save`/`numpy.load` interchange. **~60–80 lines** for a minimal implementation.

**Spectral API:**
```spectral
load "data.sdat"        ; raw binary (auto-detect by extension)
write "data.sdat" arr
load-npy "data.npy"     ; or detect .npy and route
write-npy "data.npy" arr
```

Or single ops with format inferred from extension: `load`/`write` already exist; could extend to accept `.sdat`/`.npy` or add explicit `load-binary`/`write-binary`.

---

## Part 2: HDF5 Support

### Libraries

- **hdf5-cffi** (HDFGroup): Official CFFI bindings. Requires `libhdf5` (like FFTW). Not in Quicklisp by default; needs `(ql:quickload :hdf5-cffi)` or local-projects.
- **sb-hdf**: SBCL-specific, smaller. Less maintained.
- **cl-ana hdf-table**: Table-oriented, not raw arrays.

### Recommendation: hdf5-cffi with Graceful Degradation

Follow the FFTW/Magicl pattern: load HDF5 only when available; if not, the ops are undefined and tests skip.

```lisp
;; Optional HDF5 (requires libhdf5 + hdf5-cffi)
(handler-case
    (progn
      (ql:quickload :hdf5-cffi)
      (defun load-hdf5-fn (filename path) ...)
      (defun write-hdf5-fn (filename path data) ...)
      (register-op 'load-hdf5 #'load-hdf5-fn 2)
      (register-op 'write-hdf5 #'write-hdf5-fn 3))
  (error () (warn "HDF5 not available; load-hdf5 and write-hdf5 disabled")))
```

**Scope:** Read/write a single dataset by path. No groups, no attributes, no creation of multiple datasets. Keep it minimal.

**Spectral API:**
```spectral
load-hdf5 "experiment.h5" "/spectrum"
write-hdf5 "output.h5" "/results" data
```

**Implementation:** Wrap hdf5-cffi's dataset read/write. Estimate **~40–60 lines** for the two functions plus registration.

---

## Part 3: File Layout in io.lisp

```
;; std/io.lisp structure (conceptual)

;; 1. Existing: parse-number, load-numbers, write-numbers, load-csv, write-csv, run-script

;; 2. Binary (new) — no conditional, always available
;;    - load-binary, write-binary (raw .sdat format)
;;    - load-npy, write-npy (optional: could be same file or separate)
;;    Helper: read-uint16/32, write-uint16/32 for header parsing

;; 3. HDF5 (new) — conditional
;;    - (handler-case (ql:quickload :hdf5-cffi) ...)
;;    - load-hdf5-fn, write-hdf5-fn

;; 4. register-op for all
```

---

## Part 4: Design Principles

1. **Graceful degradation**: HDF5 optional; binary always available.
2. **Minimal surface**: One load/write per format. No format negotiation in a single op.
3. **Match existing style**: Same `(filename [path] data)` conventions as `load`, `write`, `load-csv`.
4. **Types**: Support double-float (primary) and optionally int32 for compatibility. Coerce to double-float when pushing to stack for uniformity.
5. **Extension-based routing** (optional): `load "x.sdat"` vs `load "x.npy"` could dispatch on extension to keep the language surface small. Document clearly.

---

## Part 5: Implementation Order

1. **Raw binary** (load-binary, write-binary) — zero deps, validates I/O path.
2. **NPY** (load-npy, write-npy) — if interoperability is desired.
3. **HDF5** — conditional on hdf5-cffi and libhdf5.

---

## Part 6: Line Count Estimate

| Component | Lines |
|-----------|-------|
| Raw binary (header + read + write) | 45–55 |
| NPY (read + write, float64/int32 1D/2D) | 60–80 |
| HDF5 (conditional load + 2 fns + register) | 40–60 |
| **Total new** | **~120–180** |

Existing io.lisp is ~125 lines; final would be ~250–300 lines.

---

## References

- [NEP 1 — NPY format](https://numpy.org/neps/nep-0001-npy-format.html)
- [MDA format](http://mountainlab.readthedocs.io/en/latest/mda_file_format.html)
- [hdf5-cffi](https://github.com/HDFGroup/hdf5-cffi)
- Spectral FFTW pattern: `std/fftw-ffi.lisp` + conditional load in `signal_processing.lisp`
