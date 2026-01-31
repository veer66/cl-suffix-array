# CL-Suffix-Array

A Common Lisp library for building suffix arrays using the SAScan algorithm, designed to handle files that exceed available RAM capacity.

## Overview

This library implements a simplified version of the SAScan algorithm for constructing suffix arrays for large text files. The SAScan algorithm is an external memory algorithm that can handle files much larger than available RAM by processing the text in chunks and using temporary storage.

## Features

- Handles large UTF-8 text files that exceed available RAM
- Memory-efficient chunked processing
- External memory approach using temporary files
- Proper UTF-8 encoding support

## Usage

```lisp
(cl-suffix-array:build-suffix-array "input-file.txt" "output-suffix-array.txt")
```

The function accepts the following parameters:

- `input-file-path`: Path to the input text file
- `output-file-path`: Path where the suffix array will be saved
- `chunk-size`: Size of chunks to process at a time (optional, defaults to 10MB)

The output file contains the suffix array as a sequence of integers representing the starting positions of the sorted suffixes.

## Algorithm

The implementation uses a simplified version of the SAScan algorithm:

1. The input file is divided into chunks that fit in memory
2. Each chunk is processed independently to generate partial results
3. Results are merged using an external merge sort approach
4. Temporary files are used for intermediate storage

## Original SAScan Authors

The original SAScan was developed by:
- Juha Kärkkäinen
- Dominik Kempa

## Dependencies

- UIOP (for path manipulation - though the implementation avoids direct UIOP dependencies for portability)

## Testing

To run the FiveAM tests:

```bash
sbcl --eval "(require :asdf)" --eval "(asdf:load-system :cl-suffix-array)" --eval "(cl-suffix-array-test:run-tests)" --quit
```

To run the simple tests (without FiveAM dependency):

```bash
sbcl --load package.lisp --load cl-suffix-array.lisp --load cl-suffix-array-test.lisp --eval "(cl-suffix-array-test:run-simple-tests)" --quit
```

Or for manual testing:

```bash
sbcl --load package.lisp --load cl-suffix-array.lisp --eval "(cl-suffix-array:build-suffix-array \"test-input.txt\" \"output-sa.txt\")"
```

## License

MIT License

---
*This implementation was developed with the assistance of Qwen AI.*
