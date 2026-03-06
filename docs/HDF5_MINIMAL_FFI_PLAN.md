# Plan: Minimal CFFI HDF5 Binding

**Goal:** Replace hdf5-cffi with a minimal, self-contained CFFI binding that supports HDF5 1.14 and 2.0, is isolated from the rest of the std libs, and presents an API consistent with how hdf5-cffi might have evolved for modern HDF5.

**Scope:** No coding yet — file layout, API design, and test strategy only.

---

## 1. File Layout and Isolation

### New File: `std/hdf5-ffi.lisp`

**Purpose:** Pure CFFI bindings — `defcfun`, `defcenum`, `defcvar` / constants. No Spectral-specific logic. No `in-package :spectral`; defines its own package.

**Dependencies:**
- CFFI (from deps.lisp)
- libhdf5 (system library, loaded via `cffi:load-foreign-library`)

**Loaded by:** `std/io.lisp` only, inside the HDF5 `handler-case`, before `hdf5-io.lisp`.

### Existing File: `std/hdf5-io.lisp` (minimal changes)

**Purpose:** Spectral I/O layer — `load-hdf5-inner`, `write-hdf5-inner`, `load-hdf5-fn`, `write-hdf5-fn`. Uses the FFI package.

**Change:** Replace `hdf5:` prefix with whichever package we define in hdf5-ffi (see below). Otherwise logic stays the same.

### Load Order (in io.lisp)

```
handler-case
  1. Load std/hdf5-ffi.lisp  (defines bindings, loads libhdf5)
  2. Load std/hdf5-ffi.lisp  → FAIL if libhdf5 not found or symbols missing
  3. Load std/hdf5-io.lisp   (uses the FFI)
  4. register-op load-hdf5, write-hdf5
  5. setf *hdf5-available-p* t
(error () (warn "HDF5 not available..."))
```

**Isolation:**
- `hdf5-ffi.lisp` is not in `deps.lisp` — it is loaded only when we attempt HDF5.
- No other std lib (`arrays`, `math`, `filters`, etc.) depends on HDF5.
- `hdf5-io.lisp` is the sole consumer of `hdf5-ffi.lisp`.

---

## 2. Package and API Design (Evolved hdf5-cffi Style)

### Package Name Options

| Option | Pros | Cons |
|--------|------|------|
| `:hdf5` | Drop-in replacement; hdf5-io.lisp unchanged | Collision if user loads real hdf5-cffi later |
| `:spectral/hdf5` or `:spectral.hdf5` | No collision; clear ownership | hdf5-io.lisp must `use-package` or qualify |
| `:hdf5` with `:nicknames (:spectral-hdf5)` | Same as :hdf5 | Same collision risk |

**Recommendation:** Use `:hdf5` for now. We are *replacing* hdf5-cffi in this codebase; users who run Spectral don’t load hdf5-cffi. Collision is only a concern if someone explicitly loads both, which we can document as unsupported. Simplest path.

If we prefer isolation: `:spectral.hdf5` — then hdf5-io uses `spectral.hdf5:H5Fopen` etc., or we `(use-package :spectral.hdf5)` in hdf5-io.

### Symbol Names (Evolved API)

An updated hdf5-cffi would likely:

1. **Use H5Dcreate2 instead of H5Dcreate1** — H5Dcreate1 is deprecated in HDF5 2.0.
2. **Keep the rest** — H5Fopen, H5Fcreate, H5Dopen2, H5Dread, H5Dwrite, H5Screate-simple, etc. are stable.
3. **Constants** — Same names: `+H5F-ACC-RDONLY+`, `+H5F-ACC-TRUNC+`, `+H5T-NATIVE-DOUBLE+`, `+H5S-ALL+`.

So our FFI defines:
- Same function names as today, except `H5Dcreate1` → `H5Dcreate2`.
- Same constant names.
- `H5Dcreate2(loc_id, name, type_id, space_id, lcpl_id, dcpl_id, dapl_id)` — full 7-parameter signature. For HDF5 2.0, we resolve `H5P_LST_LINK_CREATE_ID_g` (lcpl_id), `H5P_LST_DATASET_CREATE_ID_g` (dcpl_id), and `H5P_LST_DATASET_ACCESS_ID_g` (dapl_id) at runtime via `foreign-symbol-pointer`; older HDF5 falls back to `H5P_DEFAULT` (0) when these symbols are absent.

### C Type Mapping

- `hid_t` → `:long` or `cffi:foreign-pointer`-like — typically a 64-bit handle. Common Lisp CFFI: `:long` or `(:pointer :void)` depending on how HDF5 defines it. HDF5 uses `typedef int64_t hid_t` or similar; `:int64` or `:long` on 64-bit platforms.
- `hsize_t` → `:unsigned-long` (we already use this for dims in hdf5-io).
- `herr_t` → `:int` (return type for close, etc.).

---

## 3. What hdf5-ffi.lisp Defines

### Functions (CFFI defcfun)

| HDF5 API | Our symbol | Notes |
|----------|------------|-------|
| H5Fopen | H5Fopen | (filename, flags, fapl_id) |
| H5Fcreate | H5Fcreate | (filename, flags, fcpl_id, fapl_id) |
| H5Fclose | H5Fclose | (file_id) |
| H5Dopen2 | H5Dopen2 | (loc_id, name, dapl_id) |
| **H5Dcreate2** | H5Dcreate2 | (loc_id, name, type_id, space_id, lcpl_id, dcpl_id, dapl_id) — full 7-param signature; replaces H5Dcreate1 |
| H5Dget_space | H5Dget-space | (dset_id) |
| H5Dread | H5Dread | (dset_id, mem_type_id, mem_space_id, file_space_id, xfer_id, buf) |
| H5Dwrite | H5Dwrite | (dset_id, mem_type_id, mem_space_id, file_space_id, xfer_id, buf) |
| H5Dclose | H5Dclose | (dset_id) |
| H5Sget_simple_extent_ndims | H5Sget-simple-extent-ndims | (space_id) |
| H5Sget_simple_extent_dims | H5Sget-simple-extent-dims | (space_id, dims, maxdims) |
| H5Screate_simple | H5Screate-simple | (rank, dims, maxdims) |
| H5Sclose | H5Sclose | (space_id) |

### Constants (or equivalent)

| C constant | Our symbol | Typical value |
|------------|------------|---------------|
| H5F_ACC_RDONLY | +H5F-ACC-RDONLY+ | 0 |
| H5F_ACC_TRUNC | +H5F-ACC-TRUNC+ | 2 |
| H5T_NATIVE_DOUBLE | +H5T-NATIVE-DOUBLE+ | From H5T_NATIVE_DOUBLE_g (type id) |
| H5S_ALL | +H5S-ALL+ | From H5S_ALL (dataspace id) |
| H5P_DEFAULT | +H5P-DEFAULT+ | 0 or the real constant |

HDF5 uses special "predefined" ids for H5T_NATIVE_DOUBLE and H5S_ALL; they are usually obtained from API calls like `H5T_NATIVE_DOUBLE` (macro) or similar. We need to ensure we use the correct mechanism — often these are macros that expand to function calls or global variable reads. The grovel in hdf5-cffi would have resolved this; we’ll need to check the HDF5 headers or use the C constants if they’re simple integers.

### Library Loading

- `cffi:load-foreign-library` with standard names: `"libhdf5"` (Linux), `"hdf5"` (Windows?), `"libhdf5.so"` etc.
- Use `cffi:define-foreign-library` and `cffi:use-foreign-library` so CFFI can try platform-specific names.
- If the library fails to load, the handler-case in io.lisp catches it and sets `*hdf5-available-p*` to nil.

---

## 4. hdf5-io.lisp Changes

1. **H5Dcreate1 → H5Dcreate2:** Replace the create call with H5Dcreate2, passing the full 7 args (loc_id, name, type_id, space_id, lcpl_id, dcpl_id, dapl_id). Use `get-h5p-link-create-default()`, `get-h5p-dataset-create-default()`, and `get-h5p-dataset-access-default()` for HDF5 2.0; fall back to `H5P_DEFAULT` (0) when these return 0 (HDF5 1.x).
2. **Package:** Ensure we use the package from hdf5-ffi (`:hdf5` or `:spectral.hdf5`).
3. **Nothing else** — the rest of the logic (allocation, read/write loop, unwind-protect) stays the same.

---

## 5. io.lisp Load Logic Change

**Current:**
```lisp
(ql:quickload :hdf5-cffi)
(load ... "std/hdf5-io.lisp")
```

**New:**
```lisp
(load (merge-pathnames "std/hdf5-ffi.lisp" *spectral-root*))
(load (merge-pathnames "std/hdf5-io.lisp" *spectral-root*))
```

Remove `ql:quickload :hdf5-cffi`. hdf5-ffi.lisp itself loads libhdf5 and defines the symbols. If loading hdf5-ffi fails (library missing, symbol resolution error), the handler-case catches it.

**Note:** CFFI must be available. It’s already in deps.lisp, so we’re fine.

---

## 6. Tests

### Current Tests (tests.lisp)

- HDF5 tests run only when `*hdf5-available-p*` is t (i.e. when our FFI + io load successfully).
- Coverage: 1D roundtrip, 2D roundtrip, shape preservation.

### Do We Need Additional Tests?

| Test type | Needed? | Rationale |
|-----------|---------|-----------|
| **Integration (roundtrip, shape)** | Already present | No change; they exercise the full path. |
| **Error: load nonexistent file** | Optional | Useful to assert we signal an error, not crash. Low priority. |
| **Error: load with invalid path** | Optional | e.g. `load-hdf5 "valid.h5" "/nonexistent"` — similar to above. |
| **Error: HDF5 unavailable** | Implicit | Tests skip when `*hdf5-available-p*` is nil; we already test that path. |
| **FFI unit tests** | No | Overkill for a prototype; integration tests are sufficient. |
| **Cross-version (1.14 vs 2.0)** | Manual / CI | Good to run on a system with HDF5 1.14 and another with 2.0 if possible. Not automated easily without two images. |

**Recommendation:** No new tests required for the initial implementation. Optional: add one or two error-path tests (e.g. `load-hdf5 "nonexistent.h5" "/data"` → error containing "file" or "cannot open") if we want to harden error handling. The existing HDF5 I/O group remains the primary validation.

---

## 7. Compatibility Matrix

| HDF5 version | Our binding | Notes |
|--------------|-------------|-------|
| 1.14.x | Yes | H5Dcreate2 exists; use it. |
| 2.0.x | Yes | Target version; no H5I_REFERENCE in our subset. |
| 1.10.x and older | Maybe | H5Dcreate2 exists since 1.8; likely fine. |

---

## 8. Implementation Order

1. Create `std/hdf5-ffi.lisp` with package, library load, constants, and all defcfun.
2. Update `std/hdf5-io.lisp` to use H5Dcreate2 and the new FFI package.
3. Update `std/io.lisp` to load hdf5-ffi instead of ql:quickload :hdf5-cffi.
4. Run tests on a system with HDF5 2.0 (and 1.14 if available).
5. Update README troubleshooting to reflect the new approach.
6. (Optional) Add error-path test for load-hdf5 on nonexistent file.

---

## 9. Risks and Unknowns

- **H5T_NATIVE_DOUBLE and H5S_ALL:** May require groveling or runtime resolution. Need to inspect HDF5 headers to see how these are exposed.
- **Library naming on Windows:** `libhdf5.dll` vs `hdf5.dll`; pkg-config may affect paths. We may need platform-specific `define-foreign-library` clauses.
- **hid_t size:** On some platforms hid_t could be 32-bit; we assume 64-bit for 64-bit Lisp. Worth a quick header check.
- **Thread safety:** HDF5 has thread-safety considerations; our usage is single-threaded, so likely fine.
