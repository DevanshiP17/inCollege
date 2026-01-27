#!/bin/bash
# InCollege Single Test Runner
# Usage: ./run_single_test.sh <test-name>
# Example: ./run_single_test.sh Epic1-Story1-Test1

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
TEST_CASES_DIR="$SCRIPT_DIR/test_cases"
TEST_OUTPUT_DIR="$SCRIPT_DIR/test_cases_output"

if [ $# -eq 0 ]; then
    echo "Usage: $0 <test-name>"
    echo ""
    echo "Example: $0 Epic1-Story1-Test1"
    echo ""
    echo "Available tests:"
    ls "$TEST_CASES_DIR"/Epic1-*-Input.txt 2>/dev/null | xargs -n1 basename | sed 's/-Input.txt//' | sed 's/^/  /'
    exit 1
fi

TEST_NAME=$1
INPUT_FILE="$TEST_CASES_DIR/${TEST_NAME}-Input.txt"
EXPECTED_OUTPUT="$TEST_OUTPUT_DIR/${TEST_NAME}-Output.txt"

# Check if test files exist
if [ ! -f "$INPUT_FILE" ]; then
    echo -e "${RED}Error: Input file not found: $INPUT_FILE${NC}"
    exit 1
fi

if [ ! -f "$EXPECTED_OUTPUT" ]; then
    echo -e "${RED}Error: Expected output file not found: $EXPECTED_OUTPUT${NC}"
    exit 1
fi

# Check if InCollege executable exists
if [ ! -f "$PROJECT_ROOT/InCollege" ]; then
    echo -e "${RED}Error: InCollege executable not found in $PROJECT_ROOT${NC}"
    echo "Please compile the program first:"
    echo "  cd $PROJECT_ROOT"
    echo "  cobc -x -free InCollege.cob"
    exit 1
fi

echo "======================================"
echo "  Running Test: $TEST_NAME"
echo "======================================"
echo ""
echo "Project Root: $PROJECT_ROOT"
echo "Input File: $INPUT_FILE"
echo "Expected Output: $EXPECTED_OUTPUT"
echo ""

# Clean state
echo -e "${BLUE}Cleaning state...${NC}"

# Clear accounts.dat contents instead of removing
if [ -f "$PROJECT_ROOT/accounts.dat" ]; then
    > "$PROJECT_ROOT/accounts.dat"  # Truncate file to zero length
    echo "Cleared accounts.dat"
else
    touch "$PROJECT_ROOT/accounts.dat"  # Create empty file if it doesn't exist
    echo "Created empty accounts.dat"
fi

rm -f "$PROJECT_ROOT/InCollege-Output.txt"

# Show input
echo -e "${BLUE}Test Input:${NC}"
echo "--------------------------------------"
cat "$INPUT_FILE"
echo "--------------------------------------"
echo ""

# Copy input file to project root
cp "$INPUT_FILE" "$PROJECT_ROOT/InCollege-Input.txt"

# Run program from project root
echo -e "${BLUE}Executing program...${NC}"
cd "$PROJECT_ROOT"
./InCollege
cd "$SCRIPT_DIR"

echo ""
echo -e "${BLUE}Program Output:${NC}"
echo "--------------------------------------"
cat "$PROJECT_ROOT/InCollege-Output.txt"
echo "--------------------------------------"
echo ""

# Compare with expected
echo -e "${BLUE}Comparing with expected output...${NC}"
if diff -q "$PROJECT_ROOT/InCollege-Output.txt" "$EXPECTED_OUTPUT" > /dev/null 2>&1; then
    echo -e "${GREEN}✅ TEST PASSED${NC}"
    echo "Output matches expected result exactly."
    
    # Clean up
    rm -f "$PROJECT_ROOT/InCollege-Input.txt"
    rm -f "$PROJECT_ROOT/InCollege-Output.txt"
    exit 0
else
    echo -e "${RED}❌ TEST FAILED${NC}"
    echo ""
    echo -e "${YELLOW}Differences:${NC}"
    echo "--------------------------------------"
    diff --color=always -u "$EXPECTED_OUTPUT" "$PROJECT_ROOT/InCollege-Output.txt" || true
    echo "--------------------------------------"
    echo ""
    echo -e "${YELLOW}Legend:${NC}"
    echo "  ${RED}- Lines in red${NC} = Expected but not in actual output"
    echo "  ${GREEN}+ Lines in green${NC} = In actual output but not expected"
    
    # Keep files for debugging
    echo ""
    echo -e "${BLUE}Debug files preserved:${NC}"
    echo "  Input: $PROJECT_ROOT/InCollege-Input.txt"
    echo "  Output: $PROJECT_ROOT/InCollege-Output.txt"
    exit 1
fi
