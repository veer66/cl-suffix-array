#!/bin/bash

# cl-suffix-array-wrapper.sh - Bash wrapper for creating suffix arrays using cl-suffix-array
#
# Usage: ./cl-suffix-array-wrapper.sh [OPTIONS] <input-file> <output-file>
#
# Options:
#   -h, --help              Show this help message
#   -v, --verbose           Verbose output
#   --sbcl-path PATH        Path to SBCL executable (default: sbcl)
#   --lisp-path PATH        Path to cl-suffix-array.lisp (default: ./cl-suffix-array.lisp)
#   --package-path PATH     Path to package.lisp (default: ./package.lisp)
#   --dynamic-space-size SIZE  Dynamic space size for SBCL (default: 8GB)

# Default values
SBCL_PATH="sbcl"
LISP_PATH="./cl-suffix-array.lisp"
PACKAGE_PATH="./package.lisp"
DYNAMIC_SPACE_SIZE=8192
VERBOSE=false
DYNAMIC_SPACE_SIZE=8192  # Default 8GB heap
CHUNK_SIZE=10485760  # Default 10MB chunk size (10 * 1024 * 1024)

# Function to display usage
usage() {
    echo "Usage: $0 [OPTIONS] <input-file> <output-file>"
    echo ""
    echo "Options:"
    echo "  -h, --help              Show this help message"
    echo "  -v, --verbose           Verbose output"
    echo "  --sbcl-path PATH        Path to SBCL executable (default: sbcl)"
    echo "  --lisp-path PATH        Path to cl-suffix-array.lisp (default: ./cl-suffix-array.lisp)"
    echo "  --package-path PATH     Path to package.lisp (default: ./package.lisp)"
    echo "  --dynamic-space-size SIZE  Dynamic space size for SBCL (default: 8GB)"
    echo ""
    echo "Example:"
    echo "  $0 input.txt output-sa.txt"
    echo "  $0 --verbose large-file.txt sa-output.txt"
    echo "  $0 --dynamic-space-size 16GB huge-file.txt sa-output.txt"
    exit 1
}

# Parse command line options
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        --sbcl-path)
            SBCL_PATH="$2"
            shift 2
            ;;
        --lisp-path)
            LISP_PATH="$2"
            shift 2
            ;;
        --package-path)
            PACKAGE_PATH="$2"
            shift 2
            ;;
        --dynamic-space-size)
            DYNAMIC_SPACE_SIZE="$2"
            shift 2
            ;;
        -*)
            echo "Unknown option: $1" >&2
            usage
            ;;
        *)
            break
            ;;
    esac
done

# Check if we have the required arguments
if [ $# -ne 2 ]; then
    echo "Error: Missing required arguments" >&2
    usage
fi

INPUT_FILE="$1"
OUTPUT_FILE="$2"

# Validate input file exists
if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: Input file '$INPUT_FILE' does not exist" >&2
    exit 1
fi

# Check if required files exist
if [ ! -f "$PACKAGE_PATH" ]; then
    echo "Error: Package file '$PACKAGE_PATH' does not exist" >&2
    exit 1
fi

if [ ! -f "$LISP_PATH" ]; then
    echo "Error: Lisp file '$LISP_PATH' does not exist" >&2
    exit 1
fi

# Print verbose information if requested
if [ "$VERBOSE" = true ]; then
    echo "SBCL path: $SBCL_PATH"
    echo "Package path: $PACKAGE_PATH"
    echo "Lisp path: $LISP_PATH"
    echo "Dynamic space size: $DYNAMIC_SPACE_SIZE"
    echo "Input file: $INPUT_FILE"
    echo "Output file: $OUTPUT_FILE"
    echo "Starting suffix array construction..."
fi

# Run the SBCL command to build the suffix array with dynamic space size
if [ "$VERBOSE" = true ]; then
    "$SBCL_PATH" \
        --dynamic-space-size "$DYNAMIC_SPACE_SIZE" \
        --load "$PACKAGE_PATH" \
        --load "$LISP_PATH" \
        --eval "(format t \"Building suffix array for ~a...~%\" \"$INPUT_FILE\")" \
        --eval "(cl-suffix-array:build-suffix-array \"$INPUT_FILE\" \"$OUTPUT_FILE\" :chunk-size $CHUNK_SIZE)" \
        --eval "(format t \"Suffix array saved to ~a~%\" \"$OUTPUT_FILE\")" \
        --quit
else
    "$SBCL_PATH" \
        --dynamic-space-size "$DYNAMIC_SPACE_SIZE" \
        --load "$PACKAGE_PATH" \
        --load "$LISP_PATH" \
        --eval "(cl-suffix-array:build-suffix-array \"$INPUT_FILE\" \"$OUTPUT_FILE\" :chunk-size $CHUNK_SIZE)" \
        --quit
fi

# Check if the command was successful
if [ $? -eq 0 ]; then
    if [ "$VERBOSE" = true ]; then
        echo "Suffix array successfully created: $OUTPUT_FILE"
    else
        echo "Suffix array created: $OUTPUT_FILE"
    fi
    exit 0
else
    echo "Error: Failed to create suffix array" >&2
    exit 1
fi
