# Spectral

> A stack-based array language for scientific computing

**⚠️ Early Development Status**: This is currently a toy prototype exploring language design concepts. Not ready for production use.

## Vision

Spectral reimagines how scientists interact with numerical data. Instead of nested function calls and complex syntax, data flows naturally through operations:

```spectral
# Traditional approach (Python/NumPy)
result = np.log10(np.abs(np.fft.fft(data * calibration_factor)))

# Spectral approach 
result = log10 abs fft * calibration_factor data
```

Reading right-to-left, data flows through transformations like a signal processing pipeline.

## Core Philosophy

- **Right-to-left data flow**: Natural reading of processing pipelines
- **Stack-based execution**: Unambiguous semantics, no operator precedence
- **Scientific focus**: Built for detector data, time series, and experimental analysis
- **Performance-oriented**: Designed for large arrays and real-time processing
- **Reproducible research**: Scripts document methodology explicitly

## Why Stack-Based?

Stack-based languages offer unique advantages:

- **Composability**: Operations chain naturally
- **No precedence rules**: Unambiguous execution order
- **Interactive development**: Easy to build complex expressions incrementally
- **Functional style**: Pure data transformations
- **Embedding-friendly**: Simple to integrate in other systems

## Quick Examples

```spectral
;; Constants and basic math
* 2 pi frequency
sin + pi range 10

;; Variables and functions 
threshold = * 3 mean data
Normalize = % /max dup

;; Reduction and scan
/+ [1 2 3 4 5]          ; sum → 15
/+ range 5               ; 0+1+2+3+4 → 10
&+ [1 2 3 4 5]           ; cumulative sum → [1 3 6 10 15]
clean = Normalize raw

;; File I/O and processing
data = load "experiment.dat"
spectrum = log10 abs fft data
above_threshold = where > 3.0 spectrum

;; Plotting
surf reshape [9 9] range 81
format-plot "set xlabel 'X'"
format-plot "set ylabel 'Y'"
format-plot "set zlabel 'Z'"

plot [1 4 6 7 6 4]
```

## Stack-Based Execution

Spectral uses a stack-based execution model (like RPN calculators):

```spectral
# Expression: + 3 5
# Execution:
# 1. Push 5 → stack: 5
# 2. Push 3 → stack: 5 3 
# 3. Pop both, add: 5+3=8 → stack: 8
```

This eliminates parentheses and makes data flow explicit.

## Current Implementation

This repository contains a Lisp prototype (~600 lines core, ~1740 lines std libraries) implementing:

- ✅ Basic arithmetic (`+`, `-`, `*`, `%`)
- ✅ Trigonometry (`sin`, `cos`, `tan`, `asin`, `acos`, `atan`)
- ✅ Constants (`pi`, `e`)
- ✅ Array operations with broadcasting
- ✅ Variables and function definitions
- ✅ File I/O (text files) (`load`, `load-csv`, `write-csv`)
- ✅ Binary array I/O (`.sdat`) (`load-binary`, `write-binary`)
- ✅ NPY format (`.npy`) (`load-npy`, `write-npy`) — float64/int32, 1D/2D
- ✅ HDF5 (`.h5`) (`load-hdf5`, `write-hdf5`) — optional, requires libhdf5 + hdf5-cffi
- ✅ Stack operations (`pop` to pop, `dup` or `d` to duplicate, `swap` to swap, `peek`)
- ✅ Stack literals `[[1 2 3][4 5 6]]`, can contain variable refs: `[[1 2 x][3 4 y]]` or
     `[A B]` where `A` and `B` are user-defined variables which can be arrays.
- ✅ Conditional (`if`)
- ✅ Reduction (`/+`, `/*`, `/max`, `/min`) — collapse array to single value or reduce along axis
- ✅ Scan (`&+`, `&*`) — prefix scan (cumulative sum, product, etc.)
- ✅ Nested groups `((sin % 2 pi) (sin))` - currently only used in combination with `if`
- ✅ FFT (need FFTW installed)
- ✅ Signal filtering: `bandpass`, `lowpass`, `highpass`, `bandstop`, `smooth`, `savgol`, `find-peaks`, `find-valleys`, `psd`, `detrend`, `differentiate`
- ✅ Matrix operations (need LAPACK installed: `mmult` or `@` for matrix multiplication,
     `det`, `trace`, `triu` and `tril`, `dagger`, `conjugate-transpose`,
     `transpose`, `inv`, `eig`)
- ✅ Script execution
- ✅ Simple plotting (depending on gnuplot)

### Current Limitations

- No loops; composition and `run` only

### Prerequisites

- [SBCL](http://sbcl.org/) (or another Common Lisp implementation)
- [Quicklisp](https://www.quicklisp.org/) for dependencies
- **Optional** (Spectral runs without these, but features are disabled):
  - [FFTW3](http://www.fftw.org/) — for FFT
  - [LAPACK](https://netlib.org/lapack/) (via Magicl) — for matrix operations
  - [HDF5](https://www.hdfgroup.org/solutions/hdf5/) (libhdf5) — for `load-hdf5` and `write-hdf5` ⚠️ **See troubleshooting below**
  - [gnuplot](http://gnuplot.info/) — for plotting

  **Platform note**: FFTW, LAPACK, and HDF5 use Unix-style toolchains (headers, pkg-config). On Linux they install via package managers (`apt install libfftw3-dev liblapack-dev libhdf5-dev`). On Windows, setup is manual and may require MSYS2, pkg-config, and custom configuration; these features often degrade gracefully with a clear message when unavailable.

### Run Tests

```bash
sbcl --noinform --load tests.lisp
```

Binary I/O tests use fixtures in `testdata/`. To regenerate them:

```bash
sbcl --noinform --script write-fixtures-minimal.lisp
```

### Try It

```bash
git clone https://github.com/sanderroosendaal/spectral.git
cd spectral
sbcl --load load-spectral.lisp
```

Run scripts from the project root so paths resolve correctly. The REPL catches errors, prints a message, and stays alive for the next input:

```spectral
ΣpectraΛ > load "examples/numbers.dat"
ΣpectraΛ > sin * 2 * pi % 20 range 20
ΣpectraΛ > AddFive = + 5
ΣpectraΛ > AddFive 10
ΣpectraΛ > run "examples/tests.spec"
ΣpectraΛ > run "examples/sombrero.spec"
ΣpectraΛ > /+ 1 2 3
Error: Reduction (e.g. /+) expects an array, got 1. Use /+ [1 2 3] to sum values.
ΣpectraΛ > exit
* (exit)
```

![Sombrero chart](https://github.com/sanderroosendaal/spectral/blob/main/examples/sombrero.png)

### More Syntax Examples

```spectral
ΣpectraΛ > load-csv "examples/example.csv"
ΣpectraΛ > fft load "examples/signal.dat"
ΣpectraΛ > ifft load "examples/spectrum.dat"
ΣpectraΛ > bandpass [5 15 200] load "examples/signal.dat"
ΣpectraΛ > find-peaks [1 3 5 4 2]
ΣpectraΛ > size [[1 2][3 4]]
ΣpectraΛ > shape [[1 2][3 4]]
ΣpectraΛ > /+ [1 2 3 4 5]
ΣpectraΛ > &+ range 5
ΣpectraΛ > complex 1 1
ΣpectraΛ > re complex 2 3
ΣpectraΛ > write-csv "spectrum.csv" fft load "examples/signal.dat"
ΣpectraΛ > * d 2
ΣpectraΛ > (*2|%2) if det d matrix
ΣpectraΛ > ((%2)(%2)) if det d matrix
```

See [REFERENCE](documentation.md) for more functions implemented.

## Roadmap

### Phase 1: Language Design (Current)

- [X] Core syntax and semantics
- [X] Basic mathematical operations
- [X] Reduction operators (`/+`, `/*`, `/max`, `/min`)
- [X] Scan operators (`&+`, `&*`) — prefix scan
- [X] Array manipulation (`dup`, `swap`, `transpose`, `take`, `drop`, `pick`)
- [X] Array literals referencing user-defined variables
- [X] Simple masking/filtering (`>`, `<`, `>=`, `<=`, `eq`, `neq`)
- [X] Control flow and conditionals (just `if`)

### Phase 2: Scientific Computing

- [X] Linear algebra (LAPACK integration)
- [X] Signal processing (FFT)
- [X] Signal filtering (`bandpass`, `lowpass`, `highpass`, `bandstop`, `smooth`, `savgol`, `find-peaks`, `find-valleys`, `psd`, `detrend`, `differentiate`)
- [X] Statistics (mean, std, correlation)
- [X] File formats (CSV load/save)
- [X] File formats (HDF5, binary .sdat, NPY)
- [X] Plotting and visualization

### Phase 3: Performance

- [ ] Typed arrays and columnar storage
- [ ] SIMD vectorization
- [ ] Parallel processing
- [ ] Memory-mapped file I/O
- [ ] C++ implementation for production

## Domain Applications

Spectral is designed for:

- **Signal processing**: Time-frequency analysis, filtering, spectroscopy
- **Experimental physics**: Detector data, sensor arrays, instrument control
- **Data analysis**: Statistical processing, machine learning pipelines
- **Embedded systems**: Expression language for data platforms

## Troubleshooting

| Message | Cause |
|---------|-------|
| "Linear Algebra Not Loaded" | Magicl/LAPACK not installed or failed to load (common on Windows) |
| "HDF5 not available" | **Known issue**: hdf5-cffi (the only HDF5 binding in Quicklisp) is incompatible with HDF5 2.0.0+. See detailed troubleshooting below. |
| FFT errors | FFTW3 library not found (CFFI); common on Windows |
| Plot commands fail | gnuplot not installed or not on PATH |
| "Bug in readtable iterators or concurrent access" | SBCL + outdated named-readtables. Install the patched version: `cd ~/quicklisp/local-projects && git clone https://github.com/melisgl/named-readtables.git` (or `%USERPROFILE%\quicklisp\local-projects` on Windows). Then retry. |
| "Reduction expects an array" | `/+` and friends require an array operand. Use `/+ [1 2 3]`, not `/+ 1 2 3`. |

### HDF5 Compatibility Issues

**Problem**: On modern Linux systems (e.g., Manjaro with HDF5 2.0.0), hdf5-cffi fails to compile with:
```
error: 'H5I_REFERENCE' undeclared (first use in this function)
```

**Root cause**: The only HDF5 binding available in Quicklisp (`hdf5-cffi`, last updated 2018) is incompatible with HDF5 2.0.0. The constant `H5I_REFERENCE` was removed in HDF5 2.0.

**Workarounds**:

1. **Accept graceful degradation** (recommended for most users):
   - Spectral detects the compilation failure and disables `load-hdf5` and `write-hdf5` with a warning
   - All other functionality works normally
   - Tests run successfully, skipping HDF5-specific tests

2. **Downgrade HDF5 to 1.14.x** (if you need HDF5 support):
   ```bash
   sudo pacman -U https://archive.archlinux.org/packages/h/hdf5/hdf5-1.14.3-1-x86_64.pkg.tar.zst
   rm -rf ~/.cache/common-lisp/sbcl-*
   sbcl --load tests.lisp
   ```
   Note: This may not work on all systems due to other API changes in hdf5-cffi.

3. **Use alternative file formats**:
   - NPY format (`.npy`) via `load-npy` / `write-npy` — works reliably
   - CSV format (`.csv`) via `load-csv` / `write-csv` — works reliably
   - Binary format (`.sdat`) via `load-binary` / `write-binary` — works reliably

**Status**: A modern HDF5 binding for Common Lisp compatible with HDF5 2.0.0+ would be needed to fully resolve this. Contributions welcome!

## Contributing

This is an experimental language exploring new ideas. Contributions welcome:

- **Language design**: Syntax suggestions, semantic discussions
- **Scientific use cases**: What operations do you need?
- **Implementation**: Help with Lisp prototype or future C++ version
- **Documentation**: Examples, tutorials, comparisons

### Discussion Topics

- What's the right balance of built-in vs user-defined functions?
- Integration strategies with existing scientific libraries?
- postfix/RPN notation and left-to-right or prefix notation and right-to-left code execution?

## Inspiration

Spectral draws inspiration from:

- **APL/J/K/uiua**: Dense array notation and mathematical thinking
- **Forth/Factor**: Stack-based execution and composability
- **The HP 15-C Calculator**: Stack-based execution, mathematical function selection. See also [SwissMicros DM15C](https://www.swissmicros.com/product/dm15c)
- **MATLAB/NumPy**: Scientific computing workflows
- **Unix pipes**: Data flowing through transformations

## Documentation

- [REFERENCE](documentation.md) — language reference
- [REFACTORING_OPPORTUNITIES](docs/REFACTORING_OPPORTUNITIES.md) — completed and planned refactorings

## License

MIT License - See [LICENSE](LICENSE) for details.

-----

*"The best way to predict the future is to invent it."* — Alan Kay

**Status**: Exploring the intersection of language design and scientific computing. Join the experiment!

**Note**: Readme written with help of LLM, approved by "roosendaalsander". However, secretly, I am just here having fun with developing and playing with my own array manipulation language.
