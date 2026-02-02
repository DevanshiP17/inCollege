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
           SELECT PROFILES-FILE
               ASSIGN TO "profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PROFILES-STATUS.
           SELECT TEMP-PROFILES-FILE
               ASSIGN TO "temp-profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TEMP-PROFILES-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD IN-FILE.
       01 IN-REC PIC X(256).

       FD OUT-FILE.
       01 OUT-REC PIC X(256).

       FD ACCT-FILE.
       01 ACCT-REC PIC X(256).

       *>*********************************************
       *> PROFILE FILE DESCRIPTORS                   *
       *>*********************************************
       FD PROFILES-FILE.
       01 PROFILE-REC PIC X(512).
       
       FD TEMP-PROFILES-FILE.
       01 TEMP-PROFILE-REC PIC X(512).

       WORKING-STORAGE SECTION.

      *> file status
       01 WS-IN-STAT PIC XX.
       01 WS-OUT-STAT PIC XX.
       01 WS-ACCT-STAT PIC XX.

       *>*********************************************
       *> PROFILE FILE STATUS CODES                  *
       *>*********************************************
       01 PROFILES-STATUS PIC XX.
       01 TEMP-PROFILES-STATUS PIC XX.


      *> input handling
       01 WS-INLINE PIC X(256).
       01 WS-OUTLINE PIC X(256).
       01 WS-PREV-PROMPT PIC X(256).

       01 WS-EOF-FLAG PIC X VALUE "N".
          88 EOF-YES VALUE "Y".
          88 EOF-NO  VALUE "N".
      *> had to add a dedicated eof flag for accounts file due to issue
      *> with WS-EOF-FLAG reuse between paragraphs
       01 WS-ACCT_EOF-FLAG PIC X VALUE "N".
          88 ACCT-EOF-YES VALUE "Y".
          88 ACCT-EOF-NO  VALUE "N".

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
       *> PROFILE PERSISTENCE VARIABLES              *
       *>*********************************************
       
       *> Profile line buffer for reading/writing
       01 WS-PROFILE-LINE PIC X(512).
       
       *> Temporary profile storage during rewrite
       01 WS-TEMP-PROFILE-LINE PIC X(512).
       
       *> Parsed profile fields from file
       01 WS-PARSED-PROFILE.
          05 WS-PARSED-USERNAME    PIC X(20).
          05 WS-PARSED-FNAME       PIC X(20).
          05 WS-PARSED-LNAME       PIC X(20).
          05 WS-PARSED-MAJOR       PIC X(40).
          05 WS-PARSED-UNIVERSITY  PIC X(40).
          05 WS-PARSED-GRAD-YEAR   PIC X(4).
          05 WS-PARSED-ABOUT       PIC X(200).
          05 WS-PARSED-WORK.
             10 WS-PARSED-WORK-ENTRY OCCURS 3 TIMES.
                15 WS-PARSED-WORK-TITLE     PIC X(40).
                15 WS-PARSED-WORK-EMPLOYER  PIC X(40).
                15 WS-PARSED-WORK-DATES     PIC X(40).
                15 WS-PARSED-WORK-DESC      PIC X(100).
          05 WS-PARSED-EDU.
             10 WS-PARSED-EDU-ENTRY OCCURS 3 TIMES.
                15 WS-PARSED-EDU-DEGREE  PIC X(40).
                15 WS-PARSED-EDU-SCHOOL  PIC X(40).
                15 WS-PARSED-EDU-YEAR    PIC X(20).
       
       *> Profile processing flags
       01 WS-PROFILE-FOUND PIC X VALUE "N".
          88 PROFILE-EXISTS VALUE "Y".
          88 PROFILE-NOT-FOUND VALUE "N".
       
       01 WS-PROFILE-EOF PIC X VALUE "N".
          88 PROFILE-EOF-YES VALUE "Y".
          88 PROFILE-EOF-NO VALUE "N".
       
       *> UNSTRING pointer for parsing
       01 WS-UNSTRING-PTR PIC 999 VALUE 0.
       
       *> Counter for profile operations
       01 WS-PROFILE-COUNT PIC 999 VALUE 0.
      
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
                15 WS-EDU-YEAR    PIC X(20)  VALUE SPACES.
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
               CONTINUE
           ELSE  
      *> create accounts file if it does not exit
      *> noticed bugs when program had no accounts.dat file present
      *> in directory
               CLOSE ACCT-FILE
               IF WS-ACCT-STAT = "05" OR WS-ACCT-STAT = "35"

                  OPEN OUTPUT ACCT-FILE
                
                  IF WS-ACCT-STAT NOT = "00" 
                     DISPLAY "Cannot create accounts.dat. Status = " WS-ACCT-STAT
                     STOP RUN  
                  END-IF 
               CLOSE  ACCT-FILE 
               OPEN INPUT ACCT-FILE 
               IF WS-ACCT-STAT NOT = "00" 
                   DISPLAY "Cannot create accounts.dat. Status = " WS-ACCT-STAT
                   STOP RUN  
               END-IF
               ELSE
                   DISPLAY "Cannot create accounts.dat. Status = " WS-ACCT-STAT
                   STOP RUN
               END-IF 
           END-IF
           *> Initialize profiles.dat (create if doesn't exist)
           OPEN INPUT PROFILES-FILE
           IF PROFILES-STATUS = "00"
               *> File exists, already open for input
               CLOSE PROFILES-FILE
           ELSE
               *> File does not exist, create it
               OPEN OUTPUT PROFILES-FILE
               IF PROFILES-STATUS NOT = "00"
                   DISPLAY "ERROR: Cannot create profiles.dat. Status=" 
                       PROFILES-STATUS
               END-IF
               CLOSE PROFILES-FILE
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
      *> echo user input line into outfile, EDITED TO ALLOW FOR INLINE INPUT
           MOVE SPACES TO WS-OUTLINE
           STRING
              FUNCTION TRIM(WS-PREV-PROMPT) DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              FUNCTION TRIM(WS-INLINE) DELIMITED BY SIZE
              INTO WS-OUTLINE
           END-STRING
     *>instead of printing the line, just write in echo input
           WRITE OUT-REC FROM WS-OUTLINE
           IF WS-OUT-STAT NOT = "00"
              DISPLAY "ERR: failed writing to outfile. Status= " WS-OUT-STAT
              STOP RUN
           END-IF.

       REQUIRE-INPUT.
           PERFORM GET-NEXT-INPUT
           IF EOF-YES
                   SET EXIT-YES TO TRUE
                   EXIT PARAGRAPH
           END-IF
           DISPLAY FUNCTION TRIM(WS-INLINE)
      *> newline for formatting
           DISPLAY SPACE
           PERFORM ECHO-INPUT.

       PRINT-INLINE.
           MOVE WS-OUTLINE TO WS-PREV-PROMPT
           DISPLAY FUNCTION TRIM(WS-OUTLINE) WITH NO ADVANCING.
           DISPLAY " " WITH NO ADVANCING.
       
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

           MOVE "1. Log In" TO WS-OUTLINE
           PERFORM PRINT-LINE

           MOVE "2. Create New Account" TO WS-OUTLINE
           PERFORM PRINT-LINE
       *> removed to match specs of project   
       *>    MOVE "3. Logout" TO WS-OUTLINE
       *>    PERFORM PRINT-LINE

           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-INLINE

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
      *> MODIFIED THIS TO USE ACCT-EOF* instead of just EOF* and clearing account # memory
       LOAD-ACCOUNTS-FROM-FILE.
           MOVE 0 TO WS-ACCOUNT-COUNT
           MOVE SPACES TO WS-ACCOUNTS
           IF WS-ACCT-STAT = "35"
               EXIT PARAGRAPH
           END-IF

           SET ACCT-EOF-NO TO TRUE
           MOVE 0 TO WS-ACCOUNT-COUNT

           PERFORM UNTIL ACCT-EOF-YES
               READ ACCT-FILE
                   AT END
                       SET ACCT-EOF-YES TO TRUE
                   NOT AT END
                       MOVE ACCT-REC TO WS-ACCT-LINE
                       PERFORM PARSE-ACCOUNT-LINE
               END-READ
           END-PERFORM

      *> Reset EOF for normal input reading
           SET ACCT-EOF-NO TO TRUE.

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
           PERFORM PRINT-INLINE
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
           PERFORM PRINT-INLINE
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
               PERFORM PRINT-INLINE
               PERFORM REQUIRE-INPUT
               IF EXIT-YES OR EOF-YES
                  EXIT PARAGRAPH
               END-IF
               MOVE FUNCTION TRIM(WS-INLINE) TO WS-CURRENT-USERNAME

               MOVE "Please enter your password:" TO WS-OUTLINE
               PERFORM PRINT-INLINE
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

      *> core profile setup with menus and updated account persistence info
       CORE-PROFILE-ROUTINE.
      *>   PERFORM LOAD-PROFILE
           PERFORM LOAD-PROFILE
           MOVE "--- Create/Edit Profile ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter First Name: " TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-P-FNAME

           MOVE "Enter Last Name: " TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-P-LNAME

           MOVE "Enter University/College Attended: " TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-P-UNIVERSITY
           
           MOVE "Enter Major: " TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
            END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-P-MAJOR
           
           PERFORM WITH TEST AFTER UNTIL (FUNCTION STORED-CHAR-LENGTH(FUNCTION TRIM(WS-P-GRAD-YEAR)) = 4) 
           AND FUNCTION TRIM(WS-P-GRAD-YEAR) IS NUMERIC
              MOVE "Enter Graduation Year (YYYY): " TO WS-OUTLINE
              PERFORM PRINT-INLINE
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
       
           MOVE "About Me (optional, max 200 chars, Enter blank line to skip): " 
           TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-P-ABOUT

           MOVE 1 TO WS-I

           PERFORM UNTIL WS-I > 3
              MOVE "Experience (optional, max 3 entries. Enter 'DONE' to finish):" 
              TO WS-OUTLINE
              PERFORM PRINT-INLINE
              MOVE SPACES TO WS-OUTLINE
              STRING 
              "Experience #" DELIMITED BY SIZE 
              WS-I DELIMITED BY SIZE 
              " - Title: " DELIMITED BY SIZE
              INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
          *> only check if user is "done" at beginning
              MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
              IF FUNCTION UPPER-CASE(WS-TRIMMED) = "DONE"
                 EXIT PERFORM
              END-IF
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-WORK-TITLE(WS-I)

              MOVE SPACES TO WS-OUTLINE
              STRING 
              "Experience #" DELIMITED BY SIZE 
              WS-I DELIMITED BY SIZE 
              " - Company/Organization: " DELIMITED BY SIZE
              INTO WS-OUTLINE 
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-WORK-EMPLOYER(WS-I)
 
              MOVE SPACES TO WS-OUTLINE
              STRING 
              "Experience #" DELIMITED BY SIZE 
              WS-I DELIMITED BY SIZE 
              " - Dates (e.g., Summer 2024): " DELIMITED BY SIZE
              INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-WORK-DATES(WS-I)
              
              MOVE SPACES TO WS-OUTLINE
              STRING
              "Experience #" DELIMITED BY SIZE 
              WS-I DELIMITED BY SIZE 
              " - Description  (optional, max 100 chars, blank to skip): " 
              DELIMITED BY SIZE
              INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:100) TO WS-WORK-DESC(WS-I)
              ADD 1 TO WS-I
           END-PERFORM.

           MOVE 1 TO WS-I

           PERFORM UNTIL WS-I > 3
              MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" 
              TO WS-OUTLINE
              PERFORM PRINT-INLINE
              MOVE SPACES TO WS-OUTLINE
              STRING 
              "Education #" DELIMITED BY SIZE 
              WS-I DELIMITED BY SIZE 
              " - Degree: " DELIMITED BY SIZE
              INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT

          *> only check if user is "done" at beginning
              MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
              IF FUNCTION UPPER-CASE(WS-TRIMMED) = "DONE"
                 EXIT PERFORM
              END-IF
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-EDU-DEGREE(WS-I)
              
              MOVE SPACES TO WS-OUTLINE
              STRING 
              "Education #" DELIMITED BY SIZE 
              WS-I DELIMITED BY SIZE 
              " - University/College: " DELIMITED BY SIZE 
              INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-EDU-SCHOOL(WS-I)
 
              MOVE SPACES TO WS-OUTLINE
              STRING 
              "Education #" DELIMITED BY SIZE 
              WS-I DELIMITED BY SIZE 
              " - Years Attended (e.g., 2023-2025): " DELIMITED BY SIZE 
              INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT

              MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-EDU-YEAR(WS-I)
              ADD 1 TO WS-I
           END-PERFORM.

           *> Save profile to persistent storage
           PERFORM SAVE-PROFILE
           MOVE "Profile saved successfully!" TO WS-OUTLINE
           PERFORM PRINT-LINE.
       

      *> what happens after login 

       AFTER-LOGIN.
           *> Load user's profile from persistent storage
           PERFORM LOAD-PROFILE

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
           *> Clear profile data for security
           PERFORM CLEAR-PROFILE-DATA
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
           PERFORM PRINT-INLINE

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
                   PERFORM VIEW-PROFILE
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
           PERFORM PRINT-INLINE
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
      
      *>*********************************************
      *> PROFILE PERSISTENCE ROUTINES               *
      *>*********************************************

       *>---------------------------------------------
       *> LOAD-PROFILE                                
       *> Purpose: Load profile for logged-in user    
       *> Called: After successful login              
       *>---------------------------------------------
       LOAD-PROFILE.
           *> Reset profile found flag
           SET PROFILE-NOT-FOUND TO TRUE
           SET PROFILE-EOF-NO TO TRUE
       
           *> Open profiles.dat for reading
           OPEN INPUT PROFILES-FILE
           IF PROFILES-STATUS = "00"
               *> File exists, scan for user's profile
               PERFORM READ-PROFILE-FILE
                   UNTIL PROFILE-EOF-YES OR PROFILE-EXISTS
               
               IF PROFILE-EXISTS
                   *> Copy parsed data into WS-PROFILE variables
                   MOVE WS-PARSED-FNAME TO WS-P-FNAME
                   MOVE WS-PARSED-LNAME TO WS-P-LNAME
                   MOVE WS-PARSED-MAJOR TO WS-P-MAJOR
                   MOVE WS-PARSED-UNIVERSITY TO WS-P-UNIVERSITY
                   MOVE WS-PARSED-GRAD-YEAR TO WS-P-GRAD-YEAR
                   MOVE WS-PARSED-ABOUT TO WS-P-ABOUT
                   *> Copy work experience entries
                   MOVE WS-PARSED-WORK-TITLE(1) TO WS-WORK-TITLE(1)
                   MOVE WS-PARSED-WORK-EMPLOYER(1) TO WS-WORK-EMPLOYER(1)
                   MOVE WS-PARSED-WORK-DATES(1) TO WS-WORK-DATES(1)
                   MOVE WS-PARSED-WORK-DESC(1) TO WS-WORK-DESC(1)
                   MOVE WS-PARSED-WORK-TITLE(2) TO WS-WORK-TITLE(2)
                   MOVE WS-PARSED-WORK-EMPLOYER(2) TO WS-WORK-EMPLOYER(2)
                   MOVE WS-PARSED-WORK-DATES(2) TO WS-WORK-DATES(2)
                   MOVE WS-PARSED-WORK-DESC(2) TO WS-WORK-DESC(2)
                   MOVE WS-PARSED-WORK-TITLE(3) TO WS-WORK-TITLE(3)
                   MOVE WS-PARSED-WORK-EMPLOYER(3) TO WS-WORK-EMPLOYER(3)
                   MOVE WS-PARSED-WORK-DATES(3) TO WS-WORK-DATES(3)
                   MOVE WS-PARSED-WORK-DESC(3) TO WS-WORK-DESC(3)
                   
                   *> Copy education entries
                   MOVE WS-PARSED-EDU-DEGREE(1) TO WS-EDU-DEGREE(1)
                   MOVE WS-PARSED-EDU-SCHOOL(1) TO WS-EDU-SCHOOL(1)
                   MOVE WS-PARSED-EDU-YEAR(1) TO WS-EDU-YEAR(1)
                   MOVE WS-PARSED-EDU-DEGREE(2) TO WS-EDU-DEGREE(2)
                   MOVE WS-PARSED-EDU-SCHOOL(2) TO WS-EDU-SCHOOL(2)
                   MOVE WS-PARSED-EDU-YEAR(2) TO WS-EDU-YEAR(2)
                   MOVE WS-PARSED-EDU-DEGREE(3) TO WS-EDU-DEGREE(3)
                   MOVE WS-PARSED-EDU-SCHOOL(3) TO WS-EDU-SCHOOL(3)
                   MOVE WS-PARSED-EDU-YEAR(3) TO WS-EDU-YEAR(3)
               END-IF
               
               CLOSE PROFILES-FILE
           ELSE
               *> File does not exist yet (first user), this is normal
               CONTINUE
           END-IF
           
           *> Reset EOF flag for next operation
           SET PROFILE-EOF-NO TO TRUE.
       
       *>---------------------------------------------
       *> READ-PROFILE-FILE                           
       *> Purpose: Read and parse one profile line    
       *> Called: By LOAD-PROFILE                     
       *>---------------------------------------------
       READ-PROFILE-FILE.
           READ PROFILES-FILE INTO WS-PROFILE-LINE
               AT END
                   SET PROFILE-EOF-YES TO TRUE
               NOT AT END
                   *> Parse the line and check if username matches
                   PERFORM PARSE-PROFILE-LINE
                   IF WS-PARSED-USERNAME = WS-CURRENT-USERNAME
                       SET PROFILE-EXISTS TO TRUE
                   END-IF
           END-READ.
       
       *>---------------------------------------------
       *> PARSE-PROFILE-LINE                          
       *> Purpose: Parse pipe-delimited profile data  
       *> Format: username|fname|lname|major|uni|year|about|work1-4fields|work2-4fields|work3-4fields|edu1-3fields|edu2-3fields|edu3-3fields (28 fields total)
       *>---------------------------------------------
       PARSE-PROFILE-LINE.
           *> Clear parsed fields first
           MOVE SPACES TO WS-PARSED-PROFILE
           
           *> Use UNSTRING to split by pipe delimiter
           UNSTRING WS-PROFILE-LINE
               DELIMITED BY "|"
               INTO
                   WS-PARSED-USERNAME
                   WS-PARSED-FNAME
                   WS-PARSED-LNAME
                   WS-PARSED-MAJOR
                   WS-PARSED-UNIVERSITY
                   WS-PARSED-GRAD-YEAR
                   WS-PARSED-ABOUT
                   WS-PARSED-WORK-TITLE(1)
                   WS-PARSED-WORK-EMPLOYER(1)
                   WS-PARSED-WORK-DATES(1)
                   WS-PARSED-WORK-DESC(1)
                   WS-PARSED-WORK-TITLE(2)
                   WS-PARSED-WORK-EMPLOYER(2)
                   WS-PARSED-WORK-DATES(2)
                   WS-PARSED-WORK-DESC(2)
                   WS-PARSED-WORK-TITLE(3)
                   WS-PARSED-WORK-EMPLOYER(3)
                   WS-PARSED-WORK-DATES(3)
                   WS-PARSED-WORK-DESC(3)
                   WS-PARSED-EDU-DEGREE(1)
                   WS-PARSED-EDU-SCHOOL(1)
                   WS-PARSED-EDU-YEAR(1)
                   WS-PARSED-EDU-DEGREE(2)
                   WS-PARSED-EDU-SCHOOL(2)
                   WS-PARSED-EDU-YEAR(2)
                   WS-PARSED-EDU-DEGREE(3)
                   WS-PARSED-EDU-SCHOOL(3)
                   WS-PARSED-EDU-YEAR(3)
           END-UNSTRING.
       
       *>---------------------------------------------
       *> SAVE-PROFILE                                
       *> Purpose: Persist current user's profile     
       *> Called: After profile editing complete      
       *> Strategy: Rewrite entire file               
       *>---------------------------------------------
       SAVE-PROFILE.
           *> Reset flags
           SET PROFILE-NOT-FOUND TO TRUE
           SET PROFILE-EOF-NO TO TRUE
           
           *> Try to open existing profiles.dat
           OPEN INPUT PROFILES-FILE
           
           IF PROFILES-STATUS = "00"
               *> File exists - need to rewrite it
               PERFORM REWRITE-PROFILE-FILE
               CLOSE PROFILES-FILE
           ELSE
               *> File does not exist - create it with first profile
               OPEN OUTPUT PROFILES-FILE
               IF PROFILES-STATUS = "00"
                   PERFORM WRITE-CURRENT-PROFILE
                   CLOSE PROFILES-FILE
               ELSE
                   DISPLAY "ERROR: Cannot create profiles.dat. Status=" 
                       PROFILES-STATUS
               END-IF
           END-IF
           
           *> Reset EOF flag
           SET PROFILE-EOF-NO TO TRUE.
       
       *>---------------------------------------------
       *> REWRITE-PROFILE-FILE                        
       *> Purpose: Update existing profiles.dat       
       *> Strategy: Copy all profiles to temp file    
       *>           Update or append current user     
       *>           Replace original with temp         
       *>---------------------------------------------
       REWRITE-PROFILE-FILE.
           *> Open temp file for output
           OPEN OUTPUT TEMP-PROFILES-FILE
           IF TEMP-PROFILES-STATUS NOT = "00"
               DISPLAY "ERROR: Cannot create temp profile file. Status=" 
                   TEMP-PROFILES-STATUS
               EXIT PARAGRAPH
           END-IF
           
           *> Read existing profiles and copy to temp
           PERFORM UNTIL PROFILE-EOF-YES
               READ PROFILES-FILE INTO WS-PROFILE-LINE
                   AT END
                       SET PROFILE-EOF-YES TO TRUE
                   NOT AT END
                       *> Parse to check username
                       PERFORM PARSE-PROFILE-LINE
                       
                       IF WS-PARSED-USERNAME = WS-CURRENT-USERNAME
                           *> Found existing profile - replace it
                           SET PROFILE-EXISTS TO TRUE
                           PERFORM WRITE-CURRENT-PROFILE
                       ELSE
                           *> Different user - copy their profile unchanged
                           WRITE TEMP-PROFILE-REC FROM WS-PROFILE-LINE
                       END-IF
               END-READ
           END-PERFORM
           
           *> If profile was not found, append it
           IF PROFILE-NOT-FOUND
               PERFORM WRITE-CURRENT-PROFILE
           END-IF
           
           CLOSE TEMP-PROFILES-FILE
           
           *> Reset EOF for file operations
           SET PROFILE-EOF-NO TO TRUE
           
           *> Replace original file with temp file
           PERFORM REPLACE-PROFILE-FILE.
       
       *>---------------------------------------------
       *> WRITE-CURRENT-PROFILE                       
       *> Purpose: Format and write current user profile
       *> Format: username|fname|lname|major|uni|year|about
       *>---------------------------------------------
       WRITE-CURRENT-PROFILE.
           MOVE SPACES TO WS-PROFILE-LINE
           
           *> Build pipe-delimited string
           STRING
               FUNCTION TRIM(WS-CURRENT-USERNAME)
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-FNAME)
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-LNAME)
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-MAJOR)
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-UNIVERSITY)
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-GRAD-YEAR)
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-ABOUT)
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(1))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(1))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(1))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(1))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(2))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(2))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(2))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(2))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(3))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(3))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(3))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(3))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(1))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(1))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(1))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(2))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(2))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(2))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(3))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(3))
               DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(3))
               DELIMITED BY SIZE
               INTO WS-PROFILE-LINE
           END-STRING
           
           *> Write to appropriate file
           *> (PROFILES-FILE if new file, TEMP-PROFILES-FILE if rewriting)
           IF TEMP-PROFILES-STATUS = "00"
               *> Temp file is open - we're in rewrite mode
               WRITE TEMP-PROFILE-REC FROM WS-PROFILE-LINE
           ELSE
               *> Writing to new profiles.dat
               WRITE PROFILE-REC FROM WS-PROFILE-LINE
           END-IF.
       
       *>---------------------------------------------
       *> REPLACE-PROFILE-FILE                        
       *> Purpose: Replace profiles.dat with temp file
       *> Note: Uses system commands                  
       *>---------------------------------------------
       REPLACE-PROFILE-FILE.
           *> Delete old profiles.dat
           CALL "SYSTEM" USING "rm -f profiles.dat"
           END-CALL
           
           *> Rename temp file to profiles.dat
           CALL "SYSTEM" USING "mv temp-profiles.dat profiles.dat"
           END-CALL.
       
       *>---------------------------------------------
       *> CLEAR-PROFILE-DATA                          
       *> Purpose: Clear profile from memory on logout
       *> Security: Prevents data leakage             
       *>---------------------------------------------
       CLEAR-PROFILE-DATA.
           MOVE SPACES TO WS-PROFILE.
       
       *>---------------------------------------------
       *> VIEW-PROFILE                                
       *> Purpose: Display current user's profile     
       *> Called: From AFTER-LOGIN-MENU option 2      
       *>---------------------------------------------
       VIEW-PROFILE.
           *> Display header
           MOVE "--- Your Profile ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
       
           *> Display name
           MOVE SPACES TO WS-OUTLINE
           STRING
               "Name: "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-FNAME)
               DELIMITED BY SIZE
               " "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-LNAME)
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
       
           *> Display university
           MOVE SPACES TO WS-OUTLINE
           STRING
               "University: "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-UNIVERSITY)
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
       
           *> Display major
           MOVE SPACES TO WS-OUTLINE
           STRING
               "Major: "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-MAJOR)
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
       
           *> Display graduation year
           MOVE SPACES TO WS-OUTLINE
           STRING
               "Graduation Year: "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-GRAD-YEAR)
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE

           *> Display about me
           MOVE SPACES TO WS-OUTLINE
           STRING
               "About Me: "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-ABOUT)
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
       
        *>    *> Display about section if it exists
        *>    IF WS-P-ABOUT NOT = SPACES
        *>        MOVE SPACES TO WS-OUTLINE
        *>        PERFORM PRINT-LINE
        *>        MOVE "About Me:" TO WS-OUTLINE
        *>        PERFORM PRINT-LINE
        *>        MOVE FUNCTION TRIM(WS-P-ABOUT) TO WS-OUTLINE
        *>        PERFORM PRINT-LINE
        *>    END-IF
       
           *> Display work experience section
           MOVE SPACES TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Work Experience:" TO WS-OUTLINE
           PERFORM PRINT-LINE
       
           *> Loop through work experiences
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               IF WS-WORK-TITLE(WS-I) NOT = SPACES
                   *> Display title
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "  Title: "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-WORK-TITLE(WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
       
                   *> Display company
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "  Company: "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-WORK-EMPLOYER(WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
       
                   *> Display dates
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "  Dates: "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-WORK-DATES(WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
       
                   *> Display description if exists
                   IF WS-WORK-DESC(WS-I) NOT = SPACES
                       MOVE SPACES TO WS-OUTLINE
                       STRING
                           "  Description: "
                           DELIMITED BY SIZE
                           FUNCTION TRIM(WS-WORK-DESC(WS-I))
                           DELIMITED BY SIZE
                           INTO WS-OUTLINE
                       END-STRING
                       PERFORM PRINT-LINE
                   END-IF
       
                   MOVE SPACES TO WS-OUTLINE
                   PERFORM PRINT-LINE
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
       
           *> If no work experience
           IF WS-WORK-TITLE(1) = SPACES
               MOVE "  (No work experience added yet)" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE SPACES TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF

           *> Display education section
           MOVE "Education:" TO WS-OUTLINE
           PERFORM PRINT-LINE
       
           *> Loop through education entries
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               IF WS-EDU-DEGREE(WS-I) NOT = SPACES
                   *> Display education entry
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "  Degree: "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-EDU-DEGREE(WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
       
                   *> Display university
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "  University: "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-EDU-SCHOOL(WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
       
                   *> Display years
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "  Years: "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-EDU-YEAR(WS-I))
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
       
                   MOVE SPACES TO WS-OUTLINE
                   PERFORM PRINT-LINE
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
       
           *> If no education
           IF WS-EDU-DEGREE(1) = SPACES
               MOVE "  (No education added yet)" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE SPACES TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF
       
           *> Display footer
           MOVE "--------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.
