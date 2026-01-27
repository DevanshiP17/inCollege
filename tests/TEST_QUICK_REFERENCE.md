# Test Case Quick Reference

| Test ID | Category | Test Scenario | Type | Expected Result |
|---------|----------|---------------|------|-----------------|
| Epic1-Story1-Test1 | Account Creation | Create account with 8-char password | Positive | Account created successfully |
| Epic1-Story1-Test2 | Account Creation | Create account with 12-char password | Positive | Account created successfully |
| Epic1-Story2-Test1 | Password Validation | Password too short (7 chars) | Negative | Error: "Password must be between 8 and 12 characters." |
| Epic1-Story2-Test2 | Password Validation | Password too long (13 chars) | Negative | Error: "Password must be between 8 and 12 characters." |
| Epic1-Story2-Test3 | Password Validation | Password missing capital letter | Negative | Error: "Password must contain at least one capital letter." |
| Epic1-Story2-Test4 | Password Validation | Password missing digit | Negative | Error: "Password must contain at least one digit." |
| Epic1-Story2-Test5 | Password Validation | Password missing special character | Negative | Error: "Password must contain at least a special character" |
| Epic1-Story3-Test1 | Account Limit | Create 5 accounts then attempt 6th | Negative | Error: "All permitted accounts have been created, please come back later" |
| Epic1-Story4-Test1 | Login | Successful login with valid credentials | Positive | Success: "You have successfully logged in." |
| Epic1-Story5-Test1 | Login Failure | Wrong password, then correct retry | Negative/Positive | Error then success with retry |
| Epic1-Story5-Test2 | Login Failure | Wrong username, then correct retry | Negative/Positive | Error then success with retry |
| Epic1-Story6-Test1 | Post-Login Nav | Select Job Search option | Positive | "Job search/internship is under construction." |
| Epic1-Story6-Test2 | Post-Login Nav | Select Find Someone option | Positive | "Find someone you know is under construction." |
| Epic1-Story7-Test1 | Learn Skill | Select skill, see message, go back | Positive | "This skill is under construction." + return |
| Epic1-Story8-Test1 | Learn Skill | Select multiple skills then go back | Positive | Multiple "under construction" messages |
| Epic1-Story9-Test1 | Input Validation | Invalid top-level menu choice | Negative | Error: "Invalid choice." |
| Epic1-Story9-Test2 | Input Validation | Invalid post-login menu choice | Negative | Error: "Invalid choice." |
| Epic1-Story10-Test1 | Username | Duplicate username attempt | Negative | Error: "Username already exists. Please choose a different username." |
| Epic1-Comprehensive-Test1 | Integration | Full workflow: create, logout, login, navigate | Positive | Complete successful flow |
| Epic1-Edge-Test1 | Edge Case | Login attempt with no accounts | Negative | Error: "Incorrect username/password, please try again" |
| Epic1-Edge-Test2 | Edge Case | Multiple failed login attempts (3+) | Positive | Unlimited retry allowed |
| Epic1-Edge-Test3 | Edge Case | Text-based menu choices | Positive | Text commands accepted ("Log In", "Logout") |

## Test Type Legend
- **Positive**: Tests expected to succeed
- **Negative**: Tests expected to fail with specific error message
- **Edge Case**: Tests boundary conditions and unusual inputs
- **Integration**: Tests complete workflows across multiple features

## Password Test Data

### Valid Passwords (8-12 chars, 1 capital, 1 digit, 1 special)
- `Passw0rd!` (8 characters - minimum boundary)
- `Passw0rd!@#$` (12 characters - maximum boundary)
- `Test123!`
- `Valid1@pw`
- `Secure9#`
- `Good8$Pass`
- `Super8@Pass`
- `Valid88!`
- `Test456!`
- `Test789!`
- `Pass123!`
- `Valid77!`
- `Test999!`

### Invalid Passwords
- `Pass0rd!` (7 characters - too short)
- `Passw0rd!@#$%` (13 characters - too long)
- `passw0rd!` (no capital letter)
- `Password!` (no digit)
- `Passw0rd` (no special character)

## Test Coverage Matrix

| Requirement | Test Cases | Coverage |
|-------------|------------|----------|
| Account Creation | Story1-Test1, Story1-Test2 | ✅ Complete |
| Password Length (8-12) | Story2-Test1, Story2-Test2 | ✅ Boundaries |
| Password Capital Letter | Story2-Test3 | ✅ Complete |
| Password Digit | Story2-Test4 | ✅ Complete |
| Password Special Char | Story2-Test5 | ✅ Complete |
| Account Limit (5 max) | Story3-Test1 | ✅ Complete |
| Username Uniqueness | Story10-Test1 | ✅ Complete |
| Successful Login | Story4-Test1, Story5-Test1, Story5-Test2 | ✅ Complete |
| Failed Login | Story5-Test1, Story5-Test2, Edge-Test1, Edge-Test2 | ✅ Complete |
| Unlimited Retry | Edge-Test2 | ✅ Complete |
| Job Search Nav | Story6-Test1 | ✅ Complete |
| Find Someone Nav | Story6-Test2 | ✅ Complete |
| Learn Skill Feature | Story7-Test1, Story8-Test1 | ✅ Complete |
| Go Back Option | Story7-Test1, Story8-Test1 | ✅ Complete |
| Logout | Comprehensive-Test1, Edge-Test3 | ✅ Complete |
| Invalid Input | Story9-Test1, Story9-Test2 | ✅ Complete |
| Text Menu Choices | Edge-Test3 | ✅ Complete |
| Input File Reading | All tests | ✅ Complete |
| Output File Writing | All tests | ✅ Complete |

## Execution Time Estimates

| Test Type | Estimated Time |
|-----------|----------------|
| Single test execution | ~1 second |
| Full test suite (22 tests) | ~30 seconds |
| Manual verification per test | ~2 minutes |
| Automated verification | ~30 seconds total |

## Critical Test Cases (Must Pass)

These test cases are critical for basic functionality:
1. ✅ Epic1-Story1-Test1 - Basic account creation
2. ✅ Epic1-Story2-Test1 through Test5 - All password validation
3. ✅ Epic1-Story3-Test1 - Account limit enforcement
4. ✅ Epic1-Story4-Test1 - Successful login
5. ✅ Epic1-Story5-Test1 - Login failure and retry
6. ✅ Epic1-Comprehensive-Test1 - Full workflow

## Test Execution Order Recommendation

For manual testing, execute in this order:
1. Edge-Test1 (no accounts scenario)
2. Story1-Test1 (first account)
3. Story4-Test1 (login test)
4. Story2-Test1 through Test5 (all password validations)
5. Story3-Test1 (account limit)
6. Story5-Test1, Test2 (login failures)
7. Story6-Test1, Test2 (navigation)
8. Story7-Test1, Story8-Test1 (skills)
9. Story9-Test1, Test2 (invalid inputs)
10. Story10-Test1 (duplicate username)
11. Edge-Test2, Test3 (edge cases)
12. Comprehensive-Test1 (full integration)

## Bug Priority Guidelines

When logging bugs from failed tests:
- **P1 (Critical)**: Story 1-5 failures (core authentication)
- **P2 (High)**: Story 6-8 failures (navigation)
- **P3 (Medium)**: Story 9-10 failures (validation)
- **P4 (Low)**: Edge case failures (cosmetic/UX)
