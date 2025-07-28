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

# Variables and functions 
threshold = * 3 mean data
Normalize = % /max dup # Function: divide by maximum
clean = Normalize raw # Apply function

# File I/O and processing
data = load "experiment.dat"
spectrum = log10 magnitude fft bandpass 10Hz 1kHz data
peaks = find-peaks 3.0 spectrum
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

This repository contains a Lisp prototype (<500 lines) implementing:

- ✅ Basic arithmetic (`+`, `-`, `*`, `%`)
- ✅ Trigonometry (`sin`, `cos`, `tan`)
- ✅ Constants (`pi`, `e`)
- ✅ Array operations with broadcasting
- ✅ Variables and function definitions
- ✅ File I/O (text files)
- ✅ Script execution

### Try It

```bash
git clone https://github.com/sanderroosendaal/spectral.git
cd spectral
sbcl --load spectral.lisp
```

```lisp
* (evaluate "sin * 2 * pi % 20 range 20") ; Sine wave
* (evaluate "AddFive = + 5") ; Define function
* (evaluate "AddFive 10") ; Use function → 15
```

## Roadmap

### Phase 1: Language Design (Current)

- [x] Core syntax and semantics
- [x] Basic mathematical operations
- [ ] Reduction operators (`/+`, `/*`, `/max`)
- [ ] Array manipulation (`dup`, `swap`, `transpose`)
- [ ] Control flow and conditionals

### Phase 2: Scientific Computing

- [ ] Linear algebra (LAPACK integration)
- [ ] Signal processing (FFT, filtering)
- [ ] Statistics (mean, std, correlation)
- [ ] File formats (HDF5, CSV, binary)
- [ ] Plotting and visualization

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

## Inspiration

Spectral draws inspiration from:

- **APL/J/K/uiua**: Dense array notation and mathematical thinking
- **Forth/Factor**: Stack-based execution and composability
- **MATLAB/NumPy**: Scientific computing workflows
- **Unix pipes**: Data flowing through transformations

## License

MIT License - See [LICENSE](https://github.com/sanderroosendaal/spectral/blob/main/LICENSE "Standard MIT License") for details.

-----

*“The best way to predict the future is to invent it.”* - Alan Kay

**Status**: Exploring the intersection of language design and scientific computing. Join the experiment!

**Note**: Readme written with help of LLM, approved by roosendaalsander
