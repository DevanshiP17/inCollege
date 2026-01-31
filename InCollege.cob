      >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STAT.

           SELECT OUT-FILE
               ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STAT.

           SELECT OPTIONAL ACCT-FILE
               ASSIGN TO "accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-STAT.

       DATA DIVISION.
       FILE SECTION.

       FD IN-FILE.
       01 IN-REC PIC X(256).

       FD OUT-FILE.
       01 OUT-REC PIC X(256).

       FD ACCT-FILE.
       01 ACCT-REC PIC X(256).

       WORKING-STORAGE SECTION.

      *> file status
       01 WS-IN-STAT PIC XX.
       01 WS-OUT-STAT PIC XX.
       01 WS-ACCT-STAT PIC XX.


      *> input handling
       01 WS-INLINE PIC X(256).
       01 WS-OUTLINE PIC X(256).

       01 WS-EOF-FLAG PIC X VALUE "N".
          88 EOF-YES VALUE "Y".
          88 EOF-NO  VALUE "N".

       01 WS-EXIT-FLAG PIC X VALUE "N".
          88 EXIT-YES VALUE "Y".
          88 EXIT-NO VALUE "N".

       01 WS-CHOICE PIC X(64).
      *> trimming will be used to account for weird EOF behavior
       01 WS-TRIMMED PIC X(64).

      *> Account storage - supports up to 5 accounts

       01 WS-ACCOUNTS.
          05 WS-ACCOUNT OCCURS 5 TIMES.
             10 WS-USERNAME PIC X(20).
             10 WS-PASSWORD PIC X(12).

       01 WS-ACCOUNT-COUNT PIC 9 VALUE 0.

      *> Current account being processed
       01 WS-CURRENT-USERNAME       PIC X(20).
       01 WS-CURRENT-PASSWORD       PIC X(12).
       01 WS-INPUT-PASSWORD         PIC X(64).

       01 WS-FOUND                  PIC X VALUE "N".
          88 USERNAME-FOUND         VALUE "Y".
          88 USERNAME-NOT-FOUND     VALUE "N".

      *> password validation flags
       01 WS-PASS-FLAG PIC X VALUE "N".
          88 PASSWORD-VALID VALUE "Y".
          88 PASSWORD-INVALID VALUE "N".

       01 WS-HAS-UPPER PIC X VALUE "N".
       01 WS-HAS-DIGIT PIC X VALUE "N".
       01 WS-HAS-SPECIAL PIC X VALUE "N".

       01 WS-PASS-LEN PIC 99 VALUE 0.
       01 WS-CHAR-IDX PIC 99 VALUE 0.
       01 WS-CHAR PIC X.

      *> persistence parsing/writing
       01 WS-ACCT-LINE              PIC X(256).
       01 WS-TMP-USER               PIC X(20).
       01 WS-TMP-PASS               PIC X(12).
       01 WS-I                      PIC 9 VALUE 0.
      
      *>*********************************************
      *> PROFILE VARIABLES                          *
      *>*********************************************
      
      *> Profile set-up
       01 WS-PROFILE.
          05 WS-P-NAME.
             10 WS-P-FNAME  PIC X(20) VALUE SPACES.
             10 WS-P-LNAME PIC X(20) VALUE SPACES. 
          05 WS-P-MAJOR       PIC X(40)  VALUE SPACES.
          05 WS-P-UNIVERSITY  PIC X(40)  VALUE SPACES.
          05 WS-P-GRAD-YEAR   PIC X(4)   VALUE SPACES.
          05 WS-P-ABOUT       PIC X(200) VALUE SPACES.
      *> Education related information
          05 WS-P-EDU.
             10 WS-EDU OCCURS 3 TIMES.
                15 WS-EDU-DEGREE  PIC X(40) VALUE SPACES.
                15 WS-EDU-SCHOOL  PIC X(40) VALUE SPACES.
                15 WS-EDU-YEAR    PIC X(4)  VALUE SPACES.
      *> Work related information
          05 WS-P-WORK.
             10 WS-WORK OCCURS 3 TIMES.
                15 WS-WORK-TITLE     PIC X(40) VALUE SPACES.
                15 WS-WORK-EMPLOYER  PIC X(40) VALUE SPACES.
                15 WS-WORK-DATES     PIC X(40) VALUE SPACES.
                15 WS-WORK-DESC      PIC X(100) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM INIT-FILES
           PERFORM LOAD-ACCOUNTS-FROM-FILE

           PERFORM TOP-LEVEL-MENU
               UNTIL EXIT-YES OR EOF-YES

           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO WS-OUTLINE
           PERFORM PRINT-LINE

           PERFORM CLOSE-FILES
           STOP RUN.

      *> File Input-Output and input buffering

       INIT-FILES.
           OPEN INPUT IN-FILE
           IF WS-IN-STAT NOT = "00"
                   DISPLAY 
                   "Cannot open InCollege-Input.txt. Status=" 
                   WS-IN-STAT
               STOP RUN
           END-IF

           OPEN OUTPUT OUT-FILE
           IF WS-OUT-STAT NOT = "00"
                   DISPLAY 
                   "Cannot create InCollege-Output.txt. Status=" 
                   WS-OUT-STAT
               STOP RUN
           END-IF

           OPEN INPUT ACCT-FILE
           IF WS-ACCT-STAT = "00"
      *> File exists, already open for input
               CONTINUE
           ELSE
      *> File does not exist, create it
               OPEN OUTPUT ACCT-FILE
               IF WS-ACCT-STAT NOT = "00"
                       DISPLAY 
                       "Cannot create accounts.dat. Status=" 
                       WS-ACCT-STAT
                   STOP RUN
               END-IF
           END-IF.

       CLOSE-FILES.
           CLOSE IN-FILE
           CLOSE OUT-FILE
           CLOSE ACCT-FILE.

      *> get next input from file, if at end, void input line
      *> and set EOF flag yes flag to true
       GET-NEXT-INPUT.
           READ IN-FILE
               AT END
                   SET EOF-YES TO TRUE
                   MOVE SPACES TO WS-INLINE
               NOT AT END
                   MOVE IN-REC TO WS-INLINE
           END-READ.

       ECHO-INPUT.
      *> echo user input line into outfile
           MOVE WS-INLINE TO WS-OUTLINE
           PERFORM PRINT-LINE.

       REQUIRE-INPUT.
           PERFORM GET-NEXT-INPUT
           IF EOF-YES
                   SET EXIT-YES TO TRUE
                   EXIT PARAGRAPH
           END-IF
           PERFORM ECHO-INPUT.

       PRINT-LINE.
           DISPLAY WS-OUTLINE
           WRITE OUT-REC FROM WS-OUTLINE
           IF WS-OUT-STAT NOT = "00"
                   DISPLAY "ERR: Failed writing to output file. Status=" 
                   WS-OUT-STAT
               STOP RUN
           END-IF.

      *> menu for user interaction

       TOP-LEVEL-MENU.
           MOVE "Welcome to InCollege!" TO WS-OUTLINE
           PERFORM PRINT-LINE

           MOVE "Log In" TO WS-OUTLINE
           PERFORM PRINT-LINE

           MOVE "Create New Account" TO WS-OUTLINE
           PERFORM PRINT-LINE

           MOVE "Logout" TO WS-OUTLINE
           PERFORM PRINT-LINE

           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-LINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
              EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED

      *> Support both number and text input
           EVALUATE TRUE
               WHEN WS-TRIMMED = "1"
                 OR WS-TRIMMED = "Log In"
                 OR WS-TRIMMED = "LogIn"
                 OR WS-TRIMMED = "login"
                 OR WS-TRIMMED = "LOG IN"
                   PERFORM AUTHENTICATE-USER

               WHEN WS-TRIMMED = "2"
                 OR WS-TRIMMED = "Create New Account"
                 OR WS-TRIMMED = "CreateNewAccount"
                 OR WS-TRIMMED = "create"
                   PERFORM CREATE-NEW-ACCOUNT

               WHEN WS-TRIMMED = "3"
                 OR WS-TRIMMED = "Logout"
                 OR WS-TRIMMED = "logout"
                   SET EXIT-YES TO TRUE

               WHEN OTHER
                   MOVE "Invalid choice." TO WS-OUTLINE
                   PERFORM PRINT-LINE
           END-EVALUATE.

      *> account persistence

       LOAD-ACCOUNTS-FROM-FILE.
      *> no file yet
           IF WS-ACCT-STAT = "35"
               EXIT PARAGRAPH
           END-IF

           SET EOF-NO TO TRUE
           MOVE 0 TO WS-ACCOUNT-COUNT

           PERFORM UNTIL EOF-YES
               READ ACCT-FILE
                   AT END
                       SET EOF-YES TO TRUE
                   NOT AT END
                       MOVE ACCT-REC TO WS-ACCT-LINE
                       PERFORM PARSE-ACCOUNT-LINE
               END-READ
           END-PERFORM

      *> Reset EOF for normal input reading
           SET EOF-NO TO TRUE.

       PARSE-ACCOUNT-LINE.
           IF WS-ACCOUNT-COUNT >= 5
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-TMP-USER
           MOVE SPACES TO WS-TMP-PASS

           UNSTRING WS-ACCT-LINE
               DELIMITED BY "|"
               INTO WS-TMP-USER WS-TMP-PASS
           END-UNSTRING

           IF FUNCTION TRIM(WS-TMP-USER) = SPACES
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-ACCOUNT-COUNT
           MOVE FUNCTION TRIM(WS-TMP-USER) 
           TO WS-USERNAME(WS-ACCOUNT-COUNT)
           MOVE FUNCTION TRIM(WS-TMP-PASS) 
           TO WS-PASSWORD(WS-ACCOUNT-COUNT).

       SAVE-ACCOUNTS-TO-FILE.
           CLOSE ACCT-FILE
           OPEN OUTPUT ACCT-FILE
           IF WS-ACCT-STAT NOT = "00"
                   MOVE "ERR: Cannot write accounts.dat. Status=" 
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               STOP RUN
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ACCOUNT-COUNT
               MOVE SPACES TO WS-ACCT-LINE
               STRING
                   FUNCTION TRIM(WS-USERNAME(WS-I)) DELIMITED BY SIZE
                   "|" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PASSWORD(WS-I)) DELIMITED BY SIZE
                   INTO WS-ACCT-LINE
               END-STRING
               WRITE ACCT-REC FROM WS-ACCT-LINE
           END-PERFORM

           CLOSE ACCT-FILE
           OPEN INPUT ACCT-FILE.

      *> account creation procedures
       CREATE-NEW-ACCOUNT.
           IF WS-ACCOUNT-COUNT >= 5
                   MOVE 
                   "All permitted accounts have been created, please come back later"
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF
           
           MOVE "Please enter your username:" TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM REQUIRE-INPUT
            IF EXIT-YES OR EOF-YES
              EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-CURRENT-USERNAME

           PERFORM CHECK-USERNAME-EXISTS
           IF USERNAME-FOUND
                   MOVE 
                   "Username already exists. Please choose a different username."
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "Please enter your password:" TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM REQUIRE-INPUT
            IF EXIT-YES OR EOF-YES
              EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-INPUT-PASSWORD

           PERFORM VALIDATE-PASSWORD
           IF PASSWORD-INVALID
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-ACCOUNT-COUNT
           MOVE WS-CURRENT-USERNAME TO WS-USERNAME(WS-ACCOUNT-COUNT)
           MOVE WS-CURRENT-PASSWORD TO WS-PASSWORD(WS-ACCOUNT-COUNT)

           PERFORM SAVE-ACCOUNTS-TO-FILE

           MOVE "Account created successfully!" TO WS-OUTLINE
           PERFORM PRINT-LINE.

       CHECK-USERNAME-EXISTS.
           SET USERNAME-NOT-FOUND TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ACCOUNT-COUNT OR USERNAME-FOUND
               IF FUNCTION TRIM(WS-USERNAME(WS-I)) =
                  FUNCTION TRIM(WS-CURRENT-USERNAME)
                   SET USERNAME-FOUND TO TRUE
               END-IF
           END-PERFORM.

       VALIDATE-PASSWORD.
           MOVE "N" TO WS-HAS-UPPER
           MOVE "N" TO WS-HAS-DIGIT
           MOVE "N" TO WS-HAS-SPECIAL
           SET PASSWORD-INVALID TO TRUE

      *> Calculate actual password length using STORED-CHAR-LENGTH
           COMPUTE WS-PASS-LEN = 
               FUNCTION STORED-CHAR-LENGTH(WS-INPUT-PASSWORD)

           IF WS-PASS-LEN < 8 OR WS-PASS-LEN > 12
                   MOVE "Password must be between 8 and 12 characters." 
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-CHAR-IDX FROM 1 BY 1
               UNTIL WS-CHAR-IDX > WS-PASS-LEN
               MOVE WS-INPUT-PASSWORD(WS-CHAR-IDX:1) TO WS-CHAR

               IF WS-CHAR >= "A" AND WS-CHAR <= "Z"
                   MOVE "Y" TO WS-HAS-UPPER
               END-IF

               IF WS-CHAR >= "0" AND WS-CHAR <= "9"
                   MOVE "Y" TO WS-HAS-DIGIT
               END-IF

               IF (WS-CHAR < "0" OR WS-CHAR > "9") AND
                  (WS-CHAR < "A" OR WS-CHAR > "Z") AND
                  (WS-CHAR < "a" OR WS-CHAR > "z")
                   MOVE "Y" TO WS-HAS-SPECIAL
               END-IF
           END-PERFORM

           IF WS-HAS-UPPER = "N"
                   MOVE 
                   "Password must contain at least one capital letter." 
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF WS-HAS-DIGIT = "N"
                   MOVE "Password must contain at least one digit." 
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF WS-HAS-SPECIAL = "N"
                   MOVE 
                   "Password must contain at least a special character" 
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-INPUT-PASSWORD TO WS-CURRENT-PASSWORD
           SET PASSWORD-VALID TO TRUE.

      *> user authentication with unlimited attempts

       AUTHENTICATE-USER.
           IF WS-ACCOUNT-COUNT = 0
                   MOVE 
                   "Incorrect username/password, please try again" 
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL USERNAME-FOUND
               MOVE "Please enter your username:" TO WS-OUTLINE
               PERFORM PRINT-LINE
               PERFORM REQUIRE-INPUT
               IF EXIT-YES OR EOF-YES
                  EXIT PARAGRAPH
               END-IF
               MOVE FUNCTION TRIM(WS-INLINE) TO WS-CURRENT-USERNAME

               MOVE "Please enter your password:" TO WS-OUTLINE
               PERFORM PRINT-LINE
               PERFORM REQUIRE-INPUT
               IF EXIT-YES OR EOF-YES
                  EXIT PARAGRAPH
               END-IF
               MOVE FUNCTION TRIM(WS-INLINE) TO WS-CURRENT-PASSWORD

               PERFORM CHECK-CREDENTIALS

               IF USERNAME-FOUND
                   MOVE "You have successfully logged in" TO WS-OUTLINE
                   PERFORM PRINT-LINE
                   PERFORM AFTER-LOGIN
               ELSE
                       MOVE 
                       "Incorrect username/password, please try again" 
                       TO WS-OUTLINE
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM

      *> reset found flag for future operations
           SET USERNAME-NOT-FOUND TO TRUE.

       CHECK-CREDENTIALS.
           SET USERNAME-NOT-FOUND TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ACCOUNT-COUNT OR USERNAME-FOUND
               IF FUNCTION TRIM(WS-USERNAME(WS-I)) =
                  FUNCTION TRIM(WS-CURRENT-USERNAME)
                  AND
                  FUNCTION TRIM(WS-PASSWORD(WS-I)) =
                  FUNCTION TRIM(WS-CURRENT-PASSWORD)
                   SET USERNAME-FOUND TO TRUE
               END-IF
           END-PERFORM.

      *> core profile setup with menus. STILL NEED ACCOUNT PERSISTENCE STUFF
       CORE-PROFILE-ROUTINE.
      *>   PERFORM LOAD-PROFILE

           MOVE "First Name: " TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-P-FNAME

           MOVE "Last Name: " TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-P-LNAME

           MOVE "University/College Attended: " TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-P-UNIVERSITY
           
           MOVE "Major: " TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-P-MAJOR
           
           PERFORM UNTIL (FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(WS-P-GRAD-YEAR)) = 4) 
           AND FUNCTION TRIM(WS-P-GRAD-YEAR) IS NUMERIC
              MOVE "Graduation Year (YYYY): " TO WS-OUTLINE
              PERFORM PRINT-LINE
              PERFORM REQUIRE-INPUT
              IF EXIT-YES OR EOF-YES
                  EXIT PARAGRAPH
              END-IF
              MOVE FUNCTION TRIM(WS-INLINE) TO WS-P-GRAD-YEAR
              IF NOT (FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(WS-P-GRAD-YEAR)) = 4)
              OR NOT (FUNCTION TRIM(WS-P-GRAD-YEAR) IS NUMERIC)
                 MOVE "Invalid year." TO WS-OUTLINE
                 PERFORM PRINT-LINE
                 MOVE SPACES TO WS-P-GRAD-YEAR
              END-IF
            END-PERFORM.
        *> BELOW STILL UNDER CONSTRUCTION, AFTER ACCOUNT PERSISTENCE ADDED, WILL NEED TO MODIFY
           MOVE "About Me(optional, max 200 chars, Enter blank line to skip): " 
           TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-P-ABOUT

           MOVE 1 TO WS-I

           PERFORM UNTIL WS-I > 3
              MOVE "Experience (optional, max 3 entries. Enter 'DONE' to finish):" 
              TO WS-OUTLINE
              PERFORM PRINT-LINE
              MOVE "Title: " TO WS-OUTLINE
              PERFORM PRINT-LINE
              PERFORM REQUIRE-INPUT
          *> only check if user is "done" at beginning
              MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
              IF FUNCTION UPPER-CASE(WS-TRIMMED) = "DONE"
                 EXIT PERFORM
              END-IF
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-WORK-TITLE(WS-I)

              MOVE "Company: " TO WS-OUTLINE
              PERFORM PRINT-LINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-WORK-EMPLOYER(WS-I)

              MOVE "Dates: " TO WS-OUTLINE
              PERFORM PRINT-LINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-WORK-DATES(WS-I)

              MOVE "Description: " TO WS-OUTLINE
              PERFORM PRINT-LINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:100) TO WS-WORK-DESC(Ws-I)
              ADD 1 TO WS-I
           END-PERFORM.

          *> EDUCATION, STILL UNDER CONSTRUCTION TO MATCH PROJECT SPECS
          *> MOVE 1 TO WS-I
          *>MOVE "Education: " TO WS-OUTLINE
          *> PERFORM PRINT-LINE
          *> PERFORM UNTIL WS-I > 3
          *>    MOVE "Degree: " TO WS-OUTLINE
          *>    PERFORM PRINT-LINE
          *>    PERFORM REQUIRE-INPUT
          *>    IF FUNCTION TRIM(WS-INLINE) = "DONE"
          *>       EXIT PARAGRAPH
          *>    END-IF
           
          *>    MOVE "University: " TO WS-OUTLINE
          *>    PERFORM PRINT-LINE
          *>   PERFORM REQUIRE-INPUT
          *>    IF FUNCTION TRIM(WS-INLINE) = "DONE"
          *>       EXIT PARAGRAPH
          *>    END-IF
          *> modify this to match format
          *>    MOVE "Years: " TO WS-OUTLINE
          *>    PERFORM PRINT-LINE
          *>    PERFORM REQUIRE-INPUT
          *>    IF FUNCTION TRIM(WS-INLINE) = "DONE"
          *>       EXIT PARAGRAPH
          *>    END-IF
          *>    PERFORM SAVE-PROFILE.
          *>    DISPLAY "

           

       

      *> what happens after login 

       AFTER-LOGIN.
           MOVE SPACES TO WS-OUTLINE
           STRING
           "Welcome, " DELIMITED BY SIZE 
           FUNCTION TRIM(WS-CURRENT-USERNAME)
           DELIMITED BY SIZE 
           "!" DELIMITED BY SIZE
           INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE

           PERFORM AFTER-LOGIN-MENU UNTIL EXIT-YES OR EOF-YES.
          *> line below resets EXIT-YES flag used by top-level menu and main
          *> so that whole program does not end when log out is done at 
          *> after log in menu
           IF EXIT-YES AND NOT EOF-YES
               SET EXIT-NO TO true
           END-IF.

       AFTER-LOGIN-MENU.
           MOVE "1. Create/Edit My Profile" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "2. View My Profile" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "3. Search for a job" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "4. Find someone you know" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "5. Learn a New Skill" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-LINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
              EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED

          *> THIS IS UNDER CONSTRUCTION RIGHT NOW, CORE PROFILE ROUTINE WIP
           
           EVALUATE WS-TRIMMED
               WHEN "1"
                   PERFORM CORE-PROFILE-ROUTINE
               WHEN "2"
                   MOVE "Will need to implement this later... "
                   TO WS-OUTLINE
                   PERFORM PRINT-LINE
               WHEN "3"
                   MOVE "This section is currently under construction"
                   TO WS-OUTLINE
                   PERFORM PRINT-LINE
                WHEN "4" 
                   MOVE "This section is currently under construction"
                   TO WS-OUTLINE
                   PERFORM PRINT-LINE
               WHEN "5"
                   PERFORM LEARN-A-SKILL
               WHEN "Logout"
                   SET EXIT-YES TO TRUE
               WHEN "log out"
                   SET EXIT-YES TO TRUE
               WHEN "logout"
                   SET EXIT-YES TO TRUE
               WHEN "Log out"
                   SET EXIT-YES TO TRUE
               WHEN "Log Out" 
                   SET EXIT-YES TO TRUE
               WHEN OTHER
                   MOVE "Invalid choice." TO WS-OUTLINE
                   PERFORM PRINT-LINE
           END-EVALUATE.

       LEARN-A-SKILL.
           MOVE "Learn a New Skill:" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Skill 1" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Skill 2" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Skill 3" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Skill 4" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Skill 5" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Go Back" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-LINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
              EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
            IF WS-TRIMMED = "Logout" 
               OR WS-TRIMMED = "logout" OR
               WS-TRIMMED = "log out" OR
               WS-TRIMMED = "Log out" OR
               WS-TRIMMED = "Log Out"
                   SET EXIT-YES TO TRUE
                   EXIT PARAGRAPH
           END-IF
           IF WS-TRIMMED = "Go Back" OR WS-TRIMMED = "6" 
                   OR WS-TRIMMED = "back"
               EXIT PARAGRAPH
           END-IF

           MOVE "This skill is under construction." TO WS-OUTLINE
           PERFORM PRINT-LINE

      *> show the skill menu again after user chooses
           PERFORM LEARN-A-SKILL.

