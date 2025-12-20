#!/bin/bash
# Sync tree-sitter-pascal corpus files into our repository
#
# This script copies the corpus files from tree-sitter-pascal into our
# repository so we can run tests in CI without needing submodules.
#
# Usage:
#   ./sync_corpus.sh [source_dir]
#
# If source_dir is not provided, it defaults to ../../../../tree-sitter-pascal/test/corpus

set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORPUS_DIR="${SCRIPT_DIR}/corpus"

# Source directory (default to tree-sitter-pascal location)
SOURCE_DIR="${1:-${SCRIPT_DIR}/../../../../tree-sitter-pascal/test/corpus}"

echo "Syncing corpus files from: ${SOURCE_DIR}"
echo "Destination: ${CORPUS_DIR}"

# Create corpus directory if it doesn't exist
mkdir -p "${CORPUS_DIR}"

# Check if source directory exists
if [ ! -d "${SOURCE_DIR}" ]; then
    echo "Error: Source directory does not exist: ${SOURCE_DIR}"
    echo ""
    echo "Please provide the path to tree-sitter-pascal/test/corpus"
    echo "Example: ./sync_corpus.sh /path/to/tree-sitter-pascal/test/corpus"
    exit 1
fi

# Copy all .txt files from source to destination
echo "Copying corpus files..."
cp -v "${SOURCE_DIR}"/*.txt "${CORPUS_DIR}/"

# Count files copied
FILE_COUNT=$(ls -1 "${CORPUS_DIR}"/*.txt 2>/dev/null | wc -l | tr -d ' ')
echo ""
echo "Successfully synced ${FILE_COUNT} corpus files"

# Create a README in the corpus directory documenting the source
cat > "${CORPUS_DIR}/README.md" <<EOF
# Tree-Sitter-Pascal Corpus Files

This directory contains test corpus files copied from the \`tree-sitter-pascal\` project.

## Source

These files are from: \`tree-sitter-pascal/test/corpus\`

Original repository: https://github.com/Isopod/tree-sitter-pascal

License: MIT (same as tree-sitter-pascal)

## Updating

To update these files, run:

\`\`\`bash
./sync_corpus.sh [path/to/tree-sitter-pascal/test/corpus]
\`\`\`

Or if tree-sitter-pascal is in the expected location:

\`\`\`bash
./sync_corpus.sh
\`\`\`

## Files

EOF

# List all files in the corpus directory
ls -1 "${CORPUS_DIR}"/*.txt 2>/dev/null | while read -r file; do
    filename=$(basename "$file")
    echo "- \`${filename}\`" >> "${CORPUS_DIR}/README.md"
done

echo ""
echo "Corpus files synced successfully!"
echo "Files are now available at: ${CORPUS_DIR}"

