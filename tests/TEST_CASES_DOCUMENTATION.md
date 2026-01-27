# InCollege Epic 1 - Comprehensive Test Case Suite
## Test Coverage Matrix

This test suite provides comprehensive coverage of all requirements specified in the Software Requirements Document for Epic #1: Log In, Part 1.

---

## TEST CASE SUMMARY

### User Story 1: Account Creation with Valid Password
**Epic1-Story1-Test1** - Minimum valid password (8 characters)
- Input File: Epic1-Story1-Test1-Input.txt
- Output File: Epic1-Story1-Test1-Output.txt
- Tests: Create account with 8-character password meeting all requirements

**Epic1-Story1-Test2** - Maximum valid password (12 characters)
- Input File: Epic1-Story1-Test2-Input.txt
- Output File: Epic1-Story1-Test2-Output.txt
- Tests: Create account with 12-character password meeting all requirements

### User Story 2: Password Validation Requirements
**Epic1-Story2-Test1** - Password too short (7 characters)
- Input File: Epic1-Story2-Test1-Input.txt
- Output File: Epic1-Story2-Test1-Output.txt
- Tests: Rejection of password with less than 8 characters
- Expected Error: "Password must be between 8 and 12 characters."

**Epic1-Story2-Test2** - Password too long (13 characters)
- Input File: Epic1-Story2-Test2-Input.txt
- Output File: Epic1-Story2-Test2-Output.txt
- Tests: Rejection of password with more than 12 characters
- Expected Error: "Password must be between 8 and 12 characters."

**Epic1-Story2-Test3** - Password missing capital letter
- Input File: Epic1-Story2-Test3-Input.txt
- Output File: Epic1-Story2-Test3-Output.txt
- Tests: Rejection of password without uppercase letter
- Expected Error: "Password must contain at least one capital letter."

**Epic1-Story2-Test4** - Password missing digit
- Input File: Epic1-Story2-Test4-Input.txt
- Output File: Epic1-Story2-Test4-Output.txt
- Tests: Rejection of password without numeric digit
- Expected Error: "Password must contain at least one digit."

**Epic1-Story2-Test5** - Password missing special character
- Input File: Epic1-Story2-Test5-Input.txt
- Output File: Epic1-Story2-Test5-Output.txt
- Tests: Rejection of password without special character
- Expected Error: "Password must contain at least a special character"

### User Story 3: Account Limit Enforcement
**Epic1-Story3-Test1** - Create 5 accounts then attempt 6th
- Input File: Epic1-Story3-Test1-Input.txt
- Output File: Epic1-Story3-Test1-Output.txt
- Tests: System allows exactly 5 accounts, rejects 6th attempt
- Expected Message: "All permitted accounts have been created, please come back later"

### User Story 4: Successful Login
**Epic1-Story4-Test1** - Login with valid credentials
- Input File: Epic1-Story4-Test1-Input.txt
- Output File: Epic1-Story4-Test1-Output.txt
- Tests: Successful login after account creation
- Expected Message: "You have successfully logged in."

### User Story 5: Failed Login Handling
**Epic1-Story5-Test1** - Incorrect password, then successful retry
- Input File: Epic1-Story5-Test1-Input.txt
- Output File: Epic1-Story5-Test1-Output.txt
- Tests: System rejects incorrect password, allows retry
- Expected Error: "Incorrect username/password, please try again"

**Epic1-Story5-Test2** - Incorrect username, then successful retry
- Input File: Epic1-Story5-Test2-Input.txt
- Output File: Epic1-Story5-Test2-Output.txt
- Tests: System rejects incorrect username, allows retry
- Expected Error: "Incorrect username/password, please try again"

### User Story 6: Post-Login Navigation Options
**Epic1-Story6-Test1** - Job Search navigation
- Input File: Epic1-Story6-Test1-Input.txt
- Output File: Epic1-Story6-Test1-Output.txt
- Tests: Job search displays "under construction" message
- Expected Message: "Job search/internship is under construction."

**Epic1-Story6-Test2** - Find Someone navigation
- Input File: Epic1-Story6-Test2-Input.txt
- Output File: Epic1-Story6-Test2-Output.txt
- Tests: Find someone displays "under construction" message
- Expected Message: "Find someone you know is under construction."

### User Story 7: Learn a Skill Feature
**Epic1-Story7-Test1** - Select skill and return to menu
- Input File: Epic1-Story7-Test1-Input.txt
- Output File: Epic1-Story7-Test1-Output.txt
- Tests: Selecting a skill shows "under construction", can return to main menu
- Expected Message: "This skill is under construction."

### User Story 8: Skill Selection and Navigation
**Epic1-Story8-Test1** - Select multiple skills then go back
- Input File: Epic1-Story8-Test1-Input.txt
- Output File: Epic1-Story8-Test1-Output.txt
- Tests: Can select multiple skills sequentially, then return to main menu
- Expected: Shows "under construction" for each skill, returns on "Go Back"

### User Story 9: Input Validation
**Epic1-Story9-Test1** - Invalid top-level menu choices
- Input File: Epic1-Story9-Test1-Input.txt
- Output File: Epic1-Story9-Test1-Output.txt
- Tests: System handles invalid menu selections at top level
- Expected Message: "Invalid choice."

**Epic1-Story9-Test2** - Invalid post-login menu choices
- Input File: Epic1-Story9-Test2-Input.txt
- Output File: Epic1-Story9-Test2-Output.txt
- Tests: System handles invalid menu selections after login
- Expected Message: "Invalid choice."

### User Story 10: Username Uniqueness
**Epic1-Story10-Test1** - Duplicate username attempt
- Input File: Epic1-Story10-Test1-Input.txt
- Output File: Epic1-Story10-Test1-Output.txt
- Tests: System rejects duplicate username
- Expected Error: "Username already exists. Please choose a different username."

### Comprehensive Integration Tests
**Epic1-Comprehensive-Test1** - Full user workflow
- Input File: Epic1-Comprehensive-Test1-Input.txt
- Output File: Epic1-Comprehensive-Test1-Output.txt
- Tests: Create account, logout, login, navigate all menu options

### Edge Case Tests
**Epic1-Edge-Test1** - Login attempt with no accounts
- Input File: Epic1-Edge-Test1-Input.txt
- Output File: Epic1-Edge-Test1-Output.txt
- Tests: System handles login attempt when no accounts exist
- Expected Error: "Incorrect username/password, please try again"

**Epic1-Edge-Test2** - Multiple failed login attempts (unlimited)
- Input File: Epic1-Edge-Test2-Input.txt
- Output File: Epic1-Edge-Test2-Output.txt
- Tests: System allows unlimited login retry attempts

**Epic1-Edge-Test3** - Text-based menu choices
- Input File: Epic1-Edge-Test3-Input.txt
- Output File: Epic1-Edge-Test3-Output.txt
- Tests: System accepts text commands like "Log In", "Create New Account"

---

## TEST EXECUTION INSTRUCTIONS

1. **Setup**: Delete accounts.dat file before each test to ensure clean state (except for persistence tests)

2. **Run Test**: 
   - Copy the corresponding input file to "InCollege-Input.txt"
   - Execute the COBOL program: `./InCollege`
   - Program reads from InCollege-Input.txt and writes to InCollege-Output.txt

3. **Verify**: 
   - Compare InCollege-Output.txt with the expected output file
   - Files should match exactly (character-by-character)

4. **Important Notes**:
   - All input from the file is echoed to the output
   - Console display and output file must be identical
   - Test files are designed to work sequentially or independently

---

## REQUIREMENTS COVERAGE

✅ Account Creation (up to 5 accounts)
✅ Password Validation (8-12 chars, capital, digit, special)
✅ Account Limit Enforcement
✅ Username Uniqueness
✅ Successful Login
✅ Failed Login with Retry (unlimited attempts)
✅ Post-Login Menu Options
✅ Job Search (under construction)
✅ Find Someone (under construction)
✅ Learn a Skill with 5 skill options
✅ Go Back functionality
✅ Logout functionality
✅ Invalid input handling
✅ Input file reading
✅ Output file writing (identical to screen output)
✅ Account persistence across sessions

---

## PASSWORD REQUIREMENTS TESTED

- Minimum length: 8 characters ✅
- Maximum length: 12 characters ✅
- At least one capital letter ✅
- At least one digit ✅
- At least one special character ✅
- Edge case: Exactly 8 characters ✅
- Edge case: Exactly 12 characters ✅

---

## TEST DATA SUMMARY

Valid Passwords Used:
- Passw0rd! (8 chars)
- Passw0rd!@#$ (12 chars)
- Test123!, Valid1@pw, Secure9#, Good8$Pass
- Super8@Pass, Pass123!, Valid88!, Test456!, etc.

Invalid Passwords Tested:
- Pass0rd! (too short - 7 chars)
- Passw0rd!@#$% (too long - 13 chars)
- passw0rd! (no capital)
- Password! (no digit)
- Passw0rd (no special char)

---

## AUTOMATION NOTES

All tests are designed for automated execution:
1. Each test is self-contained with its own input file
2. Expected outputs are provided for assertion/comparison
3. Tests can be run in any order (except persistence tests)
4. No manual intervention required
5. All user input is pre-scripted in input files

---

## BUG REPORTING

When actual output differs from expected:
1. Note the test case ID
2. Identify the line number where divergence occurs
3. Document expected vs actual output
4. Log as bug ticket in Jira with:
   - Test case reference
   - Input file used
   - Expected output
   - Actual output
   - Steps to reproduce

