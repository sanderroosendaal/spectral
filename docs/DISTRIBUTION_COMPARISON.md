# Distribution Comparison: Binary Delivery and Installation

Goal: **Easy distribution, simple installation, batteries included** — end users get a binary that works without installing runtimes, package managers, or system libraries.

Compare: **C++**, **Rust**, and **production-grade Common Lisp** (SBCL delivering a standalone executable).

---

## Summary Table

| Criterion | C++ | Rust | Common Lisp (SBCL) |
|-----------|-----|------|--------------------|
| **Single binary** | Possible (static link) | Yes, default | Yes (`save-lisp-and-die`) |
| **Binary size** | 1–5 MB | 3–8 MB | 50–120 MB |
| **Build complexity** | High (toolchain, static deps) | Low (cargo) | Low (SBCL script) |
| **External deps at runtime** | None (if fully static) | None | None |
| **FFTW/LAPACK/HDF5** | Link at build time | Link at build time | Embed via Quicklisp |
| **Installation for user** | Copy binary, run | Copy binary, run | Copy binary, run |
| **Cross-compilation** | Possible but fiddly | `cross` crate, reasonable | SBCL per-platform builds |

---

## C++

**Distribution story:** Static linking yields a single executable with no runtime dependencies. User copies the binary and runs it.

**Challenges:**
- Static linking FFTW, LAPACK, HDF5 is non-trivial (build from source or find static libs).
- Toolchain varies (GCC, Clang, libstdc++ vs libc++).
- musl + static linking gives small, portable binaries but needs extra setup.

**Binary size:** 1–5 MB for core + minimal deps; 10–30 MB if statically linking full BLAS/LAPACK.

**Verdict:** Strong end result (small, fast binary) but build and CI are complex. "Batteries included" requires significant build engineering.

---

## Rust

**Distribution story:** `cargo build --release` produces one binary. By default it links glibc dynamically on Linux; `musl` target gives a fully static binary with no runtime deps.

```bash
rustup target add x86_64-unknown-linux-musl
cargo build --release --target x86_64-unknown-linux-musl
```

**Advantages:**
- Single binary, no external runtime.
- Cargo handles builds; `rust-embed` can bundle data files.
- crates for FFTW, HDF5, etc.; link at build time, ship everything.

**Binary size:** 3–8 MB typical; 10–20 MB with FFTW + ndarray + HDF5.

**Verdict:** Very good for distribution: simple build, single file, no runtime, batteries included.

---

## Common Lisp (Production-Grade SBCL)

**Distribution story:** SBCL supports `sb-ext:save-lisp-and-die` to dump the Lisp image to a native executable. The image includes the full Lisp runtime, your code, and all Quicklisp-loaded libraries.

```lisp
(sb-ext:save-lisp-and-die "spectral"
  :executable t
  :toplevel #'main
  :compression t)  ; optional: compress core
```

**Advantages:**
- One binary: Lisp runtime + Spectral + deps (cl-ppcre, CFFI, FFTW bindings, etc.).
- No external runtime (no JVM, no interpreter).
- Direct path from prototype: evolve existing code instead of porting.
- Quicklisp deps are loaded once at build time and bundled into the image.

**Disadvantages:**
- Large binary: 50–120 MB (full SBCL + heap snapshot).
- Startup: ~0.5–2 s (image load and init).
- Cross-compilation: build on each target OS/arch or use separate SBCL builds.

**Binary size:** 50–120 MB. Compression can reduce this somewhat.

**Verdict:** Easiest path to "batteries included" if you stay on Lisp: no port, reuse prototype, one deployable binary. Trade-off is size and startup.

---

## Side-by-Side: User Experience

| | C++ | Rust | Common Lisp |
|---|-----|------|-------------|
| **User runs** | `./spectral` or `spectral script.spec` | Same | Same |
| **User installs** | Copy binary | Copy binary | Copy binary |
| **User needs** | Nothing | Nothing | Nothing |
| **First run** | Instant | Instant | ~1–2 s startup |
| **On disk** | 1–30 MB | 3–20 MB | 50–120 MB |

All three satisfy "easy distribution, simple installation, batteries included" for end users. Differences are in size, startup, and developer effort.

---

## Recommendation by Priority

| Priority | Best choice | Reason |
|----------|-------------|--------|
| **Smallest binary, fastest startup** | C++ (static) | 1–5 MB, instant start; high build cost |
| **Best build/distribution ergonomics** | Rust | Small binary, simple toolchain, batteries included |
| **Fastest path from prototype** | Common Lisp | No port; extend prototype, deliver with `save-lisp-and-die` |

For **easy distribution + simple installation + batteries included**, Rust leads on balance. Common Lisp is the best fit if you want to extend the current implementation and avoid a full rewrite.
