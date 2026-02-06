# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

### Load and test the system
```bash
# Run FiveAM tests (preferred if FiveAM is available)
sbcl --eval "(require :asdf)" --eval "(asdf:load-system :cl-suffix-array)" --eval "(cl-suffix-array-test:run-tests)" --quit

# Run simple tests (without FiveAM dependency)
sbcl --load package.lisp --load cl-suffix-array.lisp --load cl-suffix-array-test.lisp --eval "(cl-suffix-array-test:run-simple-tests)" --quit
```

### Build suffix array from command line
```bash
# Using the bash wrapper script
./suffix-array.sh input.txt output-sa.txt

# With verbose output
./suffix-array.sh --verbose input.txt output-sa.txt

# With custom dynamic space size
./suffix-array.sh --dynamic-space-size 16GB large-file.txt sa-output.txt
```

### Manual Lisp usage
```lisp
;; Load the system
(asdf:load-system :cl-suffix-array)

;; Build a suffix array
(cl-suffix-array:build-suffix-array "input.txt" "output.sa")

;; Open suffix array object and search
(defparameter obj (cl-suffix-array:open-suffix-array "input.txt" "output.sa"))
(cl-suffix-array:contains obj "pattern")
(cl-suffix-array:find-pattern obj "pattern")
(cl-suffix-array:find-lines-with-pattern obj "pattern")
```

## Architecture Overview

### System Structure
- **package.lisp**: Defines the `cl-suffix-array` package with 15 exports
- **cl-suffix-array.lisp**: Core implementation (~385 lines)
- **cl-suffix-array-test.lisp**: Test suite (~290 lines)
- **cl-suffix-array.asd**: ASDF system definition with UIOP dependency

### Algorithm Implementation
The library implements a simplified version of the **pSAscan algorithm** (Parallel External Memory Suffix Array Construction):

1. **Block Processing Phase**: Input file is split into chunks that fit in memory. Each block's suffixes are sorted and written to temporary files.

2. **Merge Phase**: Block results are merged using an external merge sort approach. The `perform-psascan-merge` function combines partial suffix arrays.

3. **Memory Management**: Uses `+chunk-size+` (default 10MB) and `memory-limit` parameters to control memory usage for large files.

### Key Data Structures
- **`suffix-array` struct**: Holds `original-text-pathname` and `suffix-array-pathname` for search operations

### Core Functions
| Function | Purpose |
|----------|---------|
| `build-suffix-array` | Construct suffix array from text file using pSAscan approach |
| `open-suffix-array` | Create suffix-array object for repeated searches |
| `contains` | Check if pattern exists in text (line-by-line reading) |
| `find-pattern` | Find all occurrences with character positions |
| `find-lines-with-pattern` | Find lines containing pattern |

### External Dependencies
- **UIOP**: Used in ASDF system definition
- **pSAscan C++ code**: Reference implementation in `psascan/` directory (requires libdivsufsort)

### Important Notes for Development
- The current implementation is a **simplified version** of pSAscan - the merge phase is a basic concatenation rather than full lexicographic merging
- The actual pSAscan algorithm is in the `psascan/` submodule (C++ with OpenMP support)
- UTF-8 support via `:external-format :utf-8` in file operations
- Uses temporary directory `./temp-psascan/` for intermediate files
