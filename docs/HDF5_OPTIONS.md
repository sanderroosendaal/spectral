# HDF5 Support Options

**Context:** Spectral's HDF5 support relies on `hdf5-cffi`, which is incompatible with HDF5 2.0.0+ (constant `H5I_REFERENCE` removed). On modern Linux (e.g. Manjaro), hdf5-cffi fails to compile.

**Scope:** We need only `load-hdf5` and `write-hdf5` — single-dataset read/write of double arrays. Our usage is minimal (~15 HDF5 API calls).

---

## Option 1: Drop HDF5 Support

**Idea:** Remove HDF5 entirely. Rely on NPY, `.sdat`, and CSV.

| Pros | Cons |
|------|------|
| No maintenance | Users who need HDF5 must convert |
| Simpler codebase | Lose interoperability with HDF5 ecosystem |
| NPY is widely supported | Some domains (e.g. synchrotron, simulation) prefer HDF5 |

**Effort:** Low — remove hdf5-io.lisp, io.lisp loader, tests; update docs.

**Recommendation:** Reasonable for a prototype if HDF5 is not critical for your users.

---

## Option 2: Patch hdf5-cffi for HDF5 2.0

**Idea:** Fork hdf5-cffi, fix the grovel/C code to work with HDF5 2.0, optionally upstream.

| Pros | Cons |
|------|------|
| Keeps using a full binding | Maintenance burden (API drift, new HDF5 releases) |
| Others could benefit | hdf5-cffi is ~2018 vintage; many API changes possible |
| | May uncover more incompatibilities beyond H5I_REFERENCE |

**Effort:** Medium — identify all HDF5 2.0 breakages in hdf5-cffi; patch grovel files; test against 1.14 and 2.0.

**Risks:** HDF5 2.0 changed identifiers, references, and possibly function signatures. Migration may touch more than one constant.

---

## Option 3: Minimal CFFI Binding (Private Implementation)

**Idea:** Bypass hdf5-cffi. Write a small CFFI binding for only the calls we use: `H5Fopen`, `H5Fcreate`, `H5Dopen2`, `H5Dcreate2`, `H5Dread`, `H5Dwrite`, etc.

Our usage is narrow:
- File: open, create, close
- Dataset: open, create, read, write, close  
- Dataspace: get extent, create simple, close
- Types: `H5T_NATIVE_DOUBLE`, `H5S_ALL`

| Pros | Cons |
|------|------|
| No dependency on hdf5-cffi | We own the binding |
| Only the API we need; H5I_REFERENCE not used | Must track HDF5 API ourselves |
| ~80–120 lines of defcfun + constants | Breaking changes in HDF5 3.0 would hit us |
| Can target HDF5 2.0 from the start | |

**Effort:** Low–medium — define CFFI bindings, test on HDF5 1.14 and 2.0. Core dataset I/O has been stable.

**Recommendation:** Best balance for a prototype: minimal, maintainable, no hdf5-cffi.

---

## Option 4: Hybrid (Patch + Minimal Fallback)

**Idea:** Try hdf5-cffi first; if it fails to load, fall back to our minimal binding.

| Pros | Cons |
|------|------|
| Works with old and new HDF5 | More code paths to test |
| Uses full binding when available | Complexity |

**Effort:** Medium.

**Recommendation:** Overkill for a prototype; Option 3 alone is simpler.

---

## Option 5: Shell Out to External Tool

**Idea:** Call `h5dump` / `h5import` or Python `h5py` via `uiop:run-program`, parse output or temporary files.

| Pros | Cons |
|------|------|
| No CFFI, no libhdf5 link | Slow, brittle, requires Python/h5dump |
| | Parsing text/binary from subprocess is messy |

**Recommendation:** Not suitable for production or even a serious prototype.

---

## Option 6: Upstream Contribution

**Idea:** Contribute an HDF5 2.0–compatible patch to hdf5-cffi (HDFGroup).

| Pros | Cons |
|------|------|
| Helps whole CL ecosystem | Project effectively unmaintained since 2018 |
| | No guarantee of review/merge |

**Recommendation:** Worth opening an issue or PR, but don’t depend on it for unblocking.

---

## Recommendation (Prototype Phase)

| Priority | Option | Rationale |
|----------|--------|-----------|
| **1** | **Option 3 — Minimal CFFI binding** | Small, self-contained, no hdf5-cffi, targets HDF5 2.0. We only need ~15 symbols; maintenance is bounded. |
| **2** | **Option 1 — Drop HDF5** | If HDF5 is not important yet, NPY and `.sdat` cover many use cases. |
| **3** | **Option 2 — Patch hdf5-cffi** | Only if you want the full binding and are willing to maintain a fork. |

**Practical path:** Implement Option 3. Keep the current API (`load-hdf5`, `write-hdf5`); replace the hdf5-cffi backend with a minimal `std/hdf5-ffi.lisp` that defines the required CFFI bindings and constants. Remove the `ql:quickload :hdf5-cffi` path. If that works, you can later revisit patching or contributing to hdf5-cffi.
