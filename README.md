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

This repository contains a Lisp prototype (~580 lines core, ~1170 lines std libraries) implementing:

- ✅ Basic arithmetic (`+`, `-`, `*`, `%`)
- ✅ Trigonometry (`sin`, `cos`, `tan`, `asin`, `acos`, `atan`)
- ✅ Constants (`pi`, `e`)
- ✅ Array operations with broadcasting
- ✅ Variables and function definitions
- ✅ File I/O (text files) (`load`, `load-csv`, `write-csv`)
- ✅ Stack operations (`pop` to pop, `dup` or `d` to duplicate, `swap` to swap, `peek`)
- ✅ Stack literals `[[1 2 3][4 5 6]]`, can contain variable refs: `[[1 2 x][3 4 y]]` or
     `[A B]` where `A` and `B` are user-defined variables which can be arrays.
- ✅ Conditional (`if`)
- ✅ Reduction (`/+`, `/*`, `/max`, `/min`) — collapse array to single value or reduce along axis
- ✅ Scan (`&+`, `&*`) — prefix scan (cumulative sum, product, etc.)
- ✅ Nested groups `((sin % 2 pi) (sin))` - currently only used in combination with `if`
- ✅ FFT (need FFTW installed)
- ✅ Matrix operations (need LAPACK installed: `mmult` or `@` for matrix multiplication,
     `det`, `trace`, `triu` and `tril`, `dagger`, `conjugate-transpose`,
     `transpose`, `inv`, `eig`)
- ✅ Script execution
- ✅ Simple plotting (depending on gnuplot)

### Current Limitations

- No loops; composition and `run` only
- No `bandpass`, `find-peaks`, or other signal-analysis helpers
- HDF5 and binary file I/O not yet supported

### Prerequisites

- [SBCL](http://sbcl.org/) (or another Common Lisp implementation)
- [Quicklisp](https://www.quicklisp.org/) for dependencies
- **Optional** (Spectral runs without these, but features are disabled):
  - [FFTW3](http://www.fftw.org/) — for FFT
  - [LAPACK](https://netlib.org/lapack/) (via Magicl) — for matrix operations
  - [gnuplot](http://gnuplot.info/) — for plotting

### Run Tests

```bash
sbcl --noinform --load tests.lisp
```

### Try It

```bash
git clone https://github.com/sanderroosendaal/spectral.git
cd spectral
sbcl --load load-spectral.lisp
```

Run scripts from the project root so paths resolve correctly:

```spectral
ΣpectraΛ > load "examples/numbers.dat"
ΣpectraΛ > sin * 2 * pi % 20 range 20
ΣpectraΛ > AddFive = + 5
ΣpectraΛ > AddFive 10
ΣpectraΛ > run "examples/tests.spec"
ΣpectraΛ > run "examples/sombrero.spec"
ΣpectraΛ > exit
* (exit)
```

![Sombrero chart](https://github.com/sanderroosendaal/spectral/blob/main/examples/sombrero.png)

### More Syntax Examples

```spectral
ΣpectraΛ > load-csv "examples/example.csv"
ΣpectraΛ > fft load "examples/signal.dat"
ΣpectraΛ > ifft load "examples/spectrum.dat"
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
- [X] Simple masking/filtering (`>`, `<`, `>=`, `<=`, `eq`)
- [X] Control flow and conditionals (just `if`)

### Phase 2: Scientific Computing

- [X] Linear algebra (LAPACK integration)
- [X] Signal processing (FFT)
- [ ] Signal filtering (`bandpass`, `find-peaks`, etc.)
- [X] Statistics (mean, std, correlation)
- [X] File formats (CSV load/save)
- [ ] File formats (HDF5, binary)
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
| "Linear Algebra Not Loaded" | Magicl/LAPACK not installed or failed to load |
| FFT errors | FFTW3 library not found (CFFI) |
| Plot commands fail | gnuplot not installed or not on PATH |
| "Bug in readtable iterators or concurrent access" | SBCL + outdated named-readtables. Install the patched version: `cd ~/quicklisp/local-projects && git clone https://github.com/melisgl/named-readtables.git` (or `%USERPROFILE%\quicklisp\local-projects` on Windows). Then retry. |

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

See [REFERENCE](documentation.md).

## License

MIT License - See [LICENSE](LICENSE) for details.

-----

*"The best way to predict the future is to invent it."* — Alan Kay

**Status**: Exploring the intersection of language design and scientific computing. Join the experiment!

**Note**: Readme written with help of LLM, approved by "roosendaalsander". However, secretly, I am just here having fun with developing and playing with my own array manipulation language.
