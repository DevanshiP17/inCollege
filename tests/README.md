# InCollege Test Suite - Epic 1

This directory contains a comprehensive test suite for InCollege Epic #1: Log In, Part 1.

## Directory Structure

```
InCollege/
├── InCollege.cob          # Main COBOL program
├── accounts.dat           # Account persistence file (generated)
└── tests/                 # Test suite directory
    ├── run_all_tests.sh          # Automated test runner
    ├── run_single_test.sh        # Single test debugger
    ├── README.md                 # This file
    ├── TEST_CASES_DOCUMENTATION.md
    ├── TEST_QUICK_REFERENCE.md
    ├── INDEX.md
    ├── test_cases/               # Test input files
    │   ├── Epic1-Story1-Test1-Input.txt
    │   ├── Epic1-Story1-Test2-Input.txt
    │   └── ... (all input files)
    ├── test_cases_output/        # Expected output files
    │   ├── Epic1-Story1-Test1-Output.txt
    │   ├── Epic1-Story1-Test2-Output.txt
    │   └── ... (all output files)
    └── reports/                  # Test execution reports (generated)
        ├── test_report_YYYY-MM-DD_HH-MM-SS.md
        └── latest_summary.txt
```

## Quick Start

### Option 1: Run All Tests (Automated)
```bash
cd tests
./run_all_tests.sh
```

### Option 2: Run Single Test (Debug)
```bash
cd tests
./run_single_test.sh Epic1-Story1-Test1
```

### Option 3: Manual Testing
```bash
# From the InCollege root directory
cp tests/test_cases/Epic1-Story1-Test1-Input.txt InCollege-Input.txt
./InCollege
diff InCollege-Output.txt tests/test_cases_output/Epic1-Story1-Test1-Output.txt
```

## Test Files Structure

Each test case consists of two files:
- **test_cases/Epic1-[Story]-Test[N]-Input.txt** - The input file
- **test_cases_output/Epic1-[Story]-Test[N]-Output.txt** - The expected output

## Test Cases by User Story

### Account Creation (Story 1)
- Epic1-Story1-Test1: Valid 8-character password (minimum)
- Epic1-Story1-Test2: Valid 12-character password (maximum)

### Password Validation (Story 2)
- Epic1-Story2-Test1: Password too short (7 chars) - NEGATIVE
- Epic1-Story2-Test2: Password too long (13 chars) - NEGATIVE
- Epic1-Story2-Test3: Missing capital letter - NEGATIVE
- Epic1-Story2-Test4: Missing digit - NEGATIVE
- Epic1-Story2-Test5: Missing special character - NEGATIVE

### Account Limit (Story 3)
- Epic1-Story3-Test1: Create 5 accounts, reject 6th

### Login Success (Story 4)
- Epic1-Story4-Test1: Successful login with valid credentials

### Login Failure (Story 5)
- Epic1-Story5-Test1: Wrong password with retry
- Epic1-Story5-Test2: Wrong username with retry

### Post-Login Navigation (Story 6)
- Epic1-Story6-Test1: Job search under construction
- Epic1-Story6-Test2: Find someone under construction

### Learn a Skill (Story 7)
- Epic1-Story7-Test1: Select skill and go back

### Skill Navigation (Story 8)
- Epic1-Story8-Test1: Multiple skill selections

### Input Validation (Story 9)
- Epic1-Story9-Test1: Invalid top-level menu choices
- Epic1-Story9-Test2: Invalid post-login menu choices

### Username Uniqueness (Story 10)
- Epic1-Story10-Test1: Duplicate username rejection

### Comprehensive Tests
- Epic1-Comprehensive-Test1: Full workflow test

### Edge Cases
- Epic1-Edge-Test1: Login with no accounts
- Epic1-Edge-Test2: Multiple failed login attempts
- Epic1-Edge-Test3: Text-based menu navigation

## Running a Test Example

```bash
# From the tests directory:
./run_single_test.sh Epic1-Story1-Test1

# Or manually from InCollege root:
cp tests/test_cases/Epic1-Story1-Test1-Input.txt InCollege-Input.txt
./InCollege
diff InCollege-Output.txt tests/test_cases_output/Epic1-Story1-Test1-Output.txt

# No output from diff means the test passed!
```

## Test Execution Best Practices

1. **Clean State**: The scripts automatically clear `accounts.dat` contents before each test (file is preserved, just emptied)
2. **Run from tests directory**: Execute scripts from the `tests/` directory
3. **Exact Matching**: Output must match character-by-character, including whitespace
4. **Input Echo**: Remember that all input is echoed to output as per requirements
5. **Review Reports**: Check the `reports/` directory for detailed execution reports

## Automated Test Script

The `run_all_tests.sh` script automatically:
- Locates the InCollege executable in the parent directory
- Finds all test input files in `test_cases/`
- Matches them with expected outputs in `test_cases_output/`
- Clears contents of `accounts.dat` between tests (preserves the file)
- Reports pass/fail for each test
- **Generates detailed reports in `reports/` directory**
- Provides a summary

Usage:
```bash
cd tests
./run_all_tests.sh
```

### Generated Reports

After running tests, check the `reports/` directory:

1. **test_report_YYYY-MM-DD_HH-MM-SS.md** - Detailed markdown report with:
   - Summary table with pass/fail counts
   - List of failed tests (if any)
   - For each test:
     - Test status (PASSED/FAILED)
     - Input used
     - Expected vs actual output
     - Diff of differences (for failed tests)

2. **latest_summary.txt** - Quick text summary with:
   - Total/passed/failed counts
   - Pass rate percentage
   - List of failed tests
   - Path to full report

Example report structure:
```
reports/
├── test_report_2024-01-26_14-30-45.md  ← Full detailed report
├── test_report_2024-01-26_15-22-10.md
└── latest_summary.txt                   ← Quick summary of last run
```

## Requirements Coverage

This test suite provides complete coverage of:
- ✅ Account creation with password validation
- ✅ Password requirements (length, capital, digit, special char)
- ✅ Account limit (max 5 accounts)
- ✅ Login success and failure scenarios
- ✅ Unlimited login retry attempts
- ✅ Post-login menu navigation
- ✅ Under construction messages
- ✅ Learn a skill feature with 5 skills
- ✅ Go back functionality
- ✅ Logout functionality
- ✅ Invalid input handling
- ✅ Username uniqueness
- ✅ Input file reading
- ✅ Output file writing (matching screen output)
- ✅ Account persistence

## Total Test Cases

- **22 test cases** covering all requirements
- **Positive tests**: 10
- **Negative tests**: 7
- **Edge cases**: 3
- **Integration tests**: 2

## File Naming Convention

- **Story tests**: Epic1-Story[N]-Test[M]-[Input|Output].txt
- **Comprehensive tests**: Epic1-Comprehensive-Test[N]-[Input|Output].txt
- **Edge case tests**: Epic1-Edge-Test[N]-[Input|Output].txt

Where:
- N = Story number (1-10)
- M = Test number within that story

## Need Help?

See TEST_CASES_DOCUMENTATION.md for detailed information about each test case, including:
- Test objectives
- Expected results
- Error messages
- Coverage matrix
