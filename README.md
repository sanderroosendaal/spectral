# Spectral

> A stack-based array language for scientific computing

**⚠️ Early Development Status**: This is currently a toy prototype exploring language design concepts. Not ready for production use.

## Vision

Spectral reimagines how scientists interact with numerical data. Instead of nested function calls and complex syntax, data flows naturally through operations:

```spectral
# Traditional approach (Python/NumPy)
result = np.log10(np.abs(np.fft.fft(data * calibration_factor)))

# Spectral approach 
result = log10 magnitude fft * calibration_factor data
```

Reading right-to-left, data flows through transformations like a signal processing pipeline.

## Core Philosophy

- **Right-to-left data flow**: Natural reading of processing pipelines
- **Stack-based execution**: Unambiguous semantics, no operator precedence
- **Scientific focus**: Built for detector data, time series, and experimental analysis
- **Performance-oriented**: Designed for large arrays and real-time processing
- **Reproducible research**: Scripts document methodology explicitly

## Quick Examples

```spectral
# Constants and basic math
* 2 pi frequency # Convert to angular frequency
sin + pi range 10 # Sine wave with phase shift

;; Variables and functions 
threshold = * 3 mean data
Normalize = % /max dup # Function: divide by maximum
clean = Normalize raw # Apply function

;; File I/O and processing
data = load "experiment.dat"
spectrum = log10 magnitude fft bandpass 10Hz 1kHz data
peaks = find-peaks 3.0 spectrum

;; Plotting
surf reshape 9 9 range 81
format-plot "set xlabel 'X'"
format-plot "set ylabel 'Y'"
format-plot "set ylabel 'Z'"

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

This repository contains a Lisp prototype (<1000 lines) implementing:

- ✅ Basic arithmetic (`+`, `-`, `*`, `%`)
- ✅ Trigonometry (`sin`, `cos`, `tan`, `asin`, `acos`, `atan`)
- ✅ Constants (`pi`, `e`)
- ✅ Array operations with broadcasting
- ✅ Variables and function definitions
- ✅ File I/O (text files) (`load`, `load-csv`, `write-csv`)
- ✅ Stack operations (`pop` to pop, `dup` or `d` to duplicate, `swap` to swap, `peek`)
- ✅ Conditional (`if`)
- ✅ Nested groups `((sin % 2 pi) (sin))` - currently only used in combination with `if`
- ✅ FFT (need FFTW installed)
- ✅ Matrix operations (need LAPACK installed: `mmult` or `@` for matrix multiplication,
     `det`, `trace`, `triu` and `tril`, `dagger`, `conjugate-transpose`,
     `transpose`, `inv`, `eig`)
- ✅ Script execution
- ✅ Simple plotting (depending on gnuplot)

### Try It

```bash
git clone https://github.com/sanderroosendaal/spectral.git
cd spectral
sbcl --load load-spectral.lisp
```

```spectral
ΣpectraΛ > load "numbers.dat" ; load a list of numbers
ΣpectraΛ > sin * 2 * pi % 20 range 20 ; Sine wave
ΣpectraΛ > AddFive = + 5 ; Define function
ΣpectraΛ > AddFive 10 ; Use function → 15
ΣpectraΛ > run "examples/tests.spec" ; run a script
ΣpectraΛ > exit ; to exit
* (exit) ; to exit from lisp
```

### More Syntax examples

```spectral
ΣpectraΛ > load-csv "examples/example.csv" ; load a list of numbers
ΣpectraΛ > fft load "signal.dat" ; fourier transform
ΣpectraΛ > ifft load "spectrum.dat" ; inverse fourier transform
ΣpectraΛ > size [[1 2][3 4]] ; number of elements: 4
ΣpectraΛ > shape [[1 2][3 4]] ; [2 2]
ΣpectraΛ > complex 1 1 ; 1+i
ΣpectraΛ > re complex 2 3 ; 2 - real part of 2+3i
ΣpectraΛ > write-csv "spectrum.csv" fft load "signal.dat" ; write calculated spectrum to file
ΣpectraΛ > * d 2
ΣpectraΛ > (*2|%2) if det d matrix ; multiply by 2 if determinant is not zero, else divide by 2
ΣpectraΛ > ((%2)(%2)) if det d matrix ; same as above, alternative notation
```

See [REFERENCE](https://github.com/sanderroosendaal/spectral/blob/main/documentation.md) for
more functions implemented.

## Roadmap

### Phase 1: Language Design (Current)

- [x] Core syntax and semantics
- [x] Basic mathematical operations
- [X] Reduction operators (`/+`, `/*`, `/max`)
- [X] Array manipulation (`dup`, `swap`, `transpose`, `take`, `drop`, `pick`)
- [X] Simple masking/filtering (`>`, `<`, `>=`, `<=`, `eq`)
- [X] Control flow and conditionals (just `if`)

### Phase 2: Scientific Computing

- [X] Linear algebra (LAPACK integration)
- [ ] Signal processing (FFT, filtering)
- [ ] Statistics (mean, std, correlation)
- [ ] File formats (HDF5, CSV, binary)
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

## Why Stack-Based?

Stack-based languages offer unique advantages:

- **Composability**: Operations chain naturally
- **No precedence rules**: Unambiguous execution order
- **Interactive development**: Easy to build complex expressions incrementally
- **Functional style**: Pure data transformations
- **Embedding-friendly**: Simple to integrate in other systems

## Contributing

This is an experimental language exploring new ideas. Contributions welcome:

- **Language design**: Syntax suggestions, semantic discussions
- **Scientific use cases**: What operations do you need?
- **Implementation**: Help with Lisp prototype or future C++ version
- **Documentation**: Examples, tutorials, comparisons

### Discussion Topics

- What’s the right balance of built-in vs user-defined functions?
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

See [REFERENCE](https://github.com/sanderroosendaal/spectral/blob/main/documentation.md).

## License

MIT License - See [LICENSE](https://github.com/sanderroosendaal/spectral/blob/main/LICENSE "Standard MIT License") for details.

-----

*“The best way to predict the future is to invent it.”* - Alan Kay

**Status**: Exploring the intersection of language design and scientific computing. Join the experiment!

**Note**: Readme written with help of LLM, approved by "roosendaalsander". However, secretly, I am just here having fun with
developing and playing with my own array manipulation language.