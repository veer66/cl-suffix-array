# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

### Fix format

This must be run everytime after the code is modified.

```bash
parfix cl-suffix-array.lisp > /tmp/$$ && mv /tmcl-suffix-array.lisp
```

### Run tests
```bash
./run-tests.sh
```

### Build suffix array from command line
```bash
# Basic usage
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
- **package.lisp**: Defines the `cl-suffix-array` package with exports for main functions
- **cl-suffix-array.lisp**: Core implementation (~385 lines) with SAScan/pSAscan algorithm
- **tests/cl-suffix-array-tests.lisp**: Test suite with 9 tests
- **cl-suffix-array.asd**: ASDF system definition with UIOP dependency

### Algorithm Implementation
The library implements a simplified version of the **pSAscan algorithm** (Parallel External Memory Suffix Array Construction):

1. **Block Processing Phase**: Input file is split into chunks that fit in memory. Each block's suffixes are sorted and written to temporary files using `process-text-block`.

2. **Merge Phase**: Block results are merged using `perform-psascan-merge` which combines partial suffix arrays.

3. **Memory Management**: Uses `+chunk-size+` (default 10MB) and `memory-limit` parameters to control memory usage for large files.

### Key Data Structures
- **`suffix-array` struct**: Holds `original-text-pathname` and `suffix-array-pathname` for search operations

### Core Functions
| Function | Purpose |
|----------|---------|
| `build-suffix-array` | Construct suffix array from text file |
| `open-suffix-array` | Create suffix-array object for repeated searches |
| `contains` | Check if pattern exists in text (line-by-line reading) |
| `find-pattern` | Find all occurrences with character positions |
| `find-lines-with-pattern` | Find lines containing pattern |
| `sufsort` | Internal memory suffix sorting routine |

### External Dependencies
- **UIOP**: Used for portability (run-program, path manipulation)
- **SBCL**: Required runtime (uses `sb-ext:run-program` for shell commands)

### Important Notes for Development
- The current implementation uses external sorting via the system `sort` command for large files
- For files < 50MB, uses in-memory approach with proper UTF-8 handling
- For larger files, uses line-based approach with temp files and external `sort`
- UTF-8 support via `:external-format :utf-8` in file operations
- The `read-text` function properly handles UTF-8 encoding
