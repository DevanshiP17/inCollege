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

           *> Connection requests file
           SELECT OPTIONAL CONN-FILE
               ASSIGN TO "connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONN-STAT.

           *> Jobs/Internships file
           SELECT OPTIONAL JOBS-FILE
               ASSIGN TO "jobs.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOBS-STAT.

           SELECT OPTIONAL APPS-FILE
               ASSIGN TO "applications.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APPS-STAT.

           *> WEEK 8: Messages file
           SELECT OPTIONAL MSGS-FILE
               ASSIGN TO "messages.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MSGS-STAT.

       DATA DIVISION.
       FILE SECTION.

       FD IN-FILE.
       01 IN-REC PIC X(256).

       FD OUT-FILE.
       01 OUT-REC PIC X(256).

       FD ACCT-FILE.
       01 ACCT-REC PIC X(256).

       FD PROFILES-FILE.
       01 PROFILE-REC PIC X(1024).
       
       FD TEMP-PROFILES-FILE.
       01 TEMP-PROFILE-REC PIC X(1024).

       FD CONN-FILE.
       01 CONN-REC PIC X(256).

       FD JOBS-FILE.
       01 JOBS-REC PIC X(512).

       FD APPS-FILE.
       01 APPS-REC PIC X(256).

       *>*********************************************
       *> MESSAGES FILE DESCRIPTOR (WEEK 8)          *
       *>*********************************************
       FD MSGS-FILE.
       01 MSGS-REC PIC X(512).

       WORKING-STORAGE SECTION.

       01 WS-IN-STAT PIC XX.
       01 WS-OUT-STAT PIC XX.
       01 WS-ACCT-STAT PIC XX.
       01 PROFILES-STATUS PIC XX.
       01 TEMP-PROFILES-STATUS PIC XX.
       01 WS-CONN-STAT PIC XX.
       01 WS-JOBS-STAT PIC XX.

       01 WS-APPS-STAT PIC XX.
       01 WS-APPS-LINE PIC X(256).
       01 WS-APPS-EOF PIC X VALUE "N".
          88 APPS-EOF-YES VALUE "Y".
          88 APPS-EOF-NO VALUE "N".

       *> WEEK 8: Messages file status
       01 WS-MSGS-STAT PIC XX.

       01 WS-APP-PARSE-USER    PIC X(20).
       01 WS-APP-PARSE-TITLE   PIC X(40).
       01 WS-APP-PARSE-EMP     PIC X(40).
       01 WS-APP-PARSE-LOC     PIC X(40).
       01 WS-APP-PARSE-JOBNUM  PIC 99.
       01 WS-APPS-COUNT-NUM    PIC 99 VALUE 0.
       01 WS-APPS-COUNT        PIC Z9.

       01 WS-INLINE PIC X(256).
       01 WS-OUTLINE PIC X(256).
       01 WS-PREV-PROMPT PIC X(256).

       01 WS-EOF-FLAG PIC X VALUE "N".
          88 EOF-YES VALUE "Y".
          88 EOF-NO  VALUE "N".

       01 WS-ACCT_EOF-FLAG PIC X VALUE "N".
          88 ACCT-EOF-YES VALUE "Y".
          88 ACCT-EOF-NO  VALUE "N".

       01 WS-EXIT-FLAG PIC X VALUE "N".
          88 EXIT-YES VALUE "Y".
          88 EXIT-NO VALUE "N".

       01 WS-CHOICE PIC X(64).
       01 WS-TRIMMED PIC X(64).

       01 WS-ACCOUNTS.
          05 WS-ACCOUNT OCCURS 5 TIMES.
             10 WS-USERNAME PIC X(20).
             10 WS-PASSWORD PIC X(12).

       01 WS-ACCOUNT-COUNT PIC 9 VALUE 0.

       01 WS-CURRENT-USERNAME       PIC X(20).
       01 WS-CURRENT-PASSWORD       PIC X(12).
       01 WS-INPUT-PASSWORD         PIC X(64).

       01 WS-FOUND                  PIC X VALUE "N".
          88 USERNAME-FOUND         VALUE "Y".
          88 USERNAME-NOT-FOUND     VALUE "N".

       01 WS-PASS-FLAG PIC X VALUE "N".
          88 PASSWORD-VALID VALUE "Y".
          88 PASSWORD-INVALID VALUE "N".

       01 WS-HAS-UPPER PIC X VALUE "N".
       01 WS-HAS-DIGIT PIC X VALUE "N".
       01 WS-HAS-SPECIAL PIC X VALUE "N".

       01 WS-PASS-LEN PIC 99 VALUE 0.
       01 WS-CHAR-IDX PIC 99 VALUE 0.
       01 WS-CHAR PIC X.

       01 WS-ACCT-LINE              PIC X(256).
       01 WS-TMP-USER               PIC X(20).
       01 WS-TMP-PASS               PIC X(12).
       01 WS-I                      PIC 9 VALUE 0.

       01 WS-PROFILE-LINE PIC X(1024).
       01 WS-TEMP-PROFILE-LINE PIC X(512).
       
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
       
       01 WS-PROFILE-FOUND PIC X VALUE "N".
          88 PROFILE-EXISTS VALUE "Y".
          88 PROFILE-NOT-FOUND VALUE "N".
       
       01 WS-PROFILE-EOF PIC X VALUE "N".
          88 PROFILE-EOF-YES VALUE "Y".
          88 PROFILE-EOF-NO VALUE "N".
       
       01 WS-UNSTRING-PTR PIC 999 VALUE 0.
       01 WS-PROFILE-COUNT PIC 999 VALUE 0.
     
       01 WS-SEARCH-INPUT PIC X(256).
       01 WS-SEARCH-FNAME PIC X(20).
       01 WS-SEARCH-LNAME PIC X(20).
       01 WS-SEARCH-FULL-NAME PIC X(41).
       01 WS-CURRENT-FULL-NAME PIC X(41).

       01 WS-USER-FOUND PIC X VALUE "N".
          88 USER-SEARCH-FOUND VALUE "Y".
          88 USER-SEARCH-NOT-FOUND VALUE "N".

       01 WS-SEARCH-EOF PIC X VALUE "N".
          88 SEARCH-EOF-YES VALUE "Y".
          88 SEARCH-EOF-NO VALUE "N".

       01 WS-CONN-LINE PIC X(256).
       01 WS-CONN-SENDER-PARSE PIC X(20).
       01 WS-CONN-RECIP-PARSE PIC X(20).
       01 WS-CONN-STATUS-PARSE PIC X(20).
       01 WS-CONN-RECIPIENT PIC X(20).
       01 WS-CONN-INVALID PIC X VALUE "N".
       
       01 WS-CONN-EOF PIC X VALUE "N".
          88 CONN-EOF-YES VALUE "Y".
          88 CONN-EOF-NO VALUE "N".

       01 WS-PROFILE.
          05 WS-P-NAME.
             10 WS-P-FNAME  PIC X(20) VALUE SPACES.
             10 WS-P-LNAME  PIC X(20) VALUE SPACES.
          05 WS-P-MAJOR       PIC X(40)  VALUE SPACES.
          05 WS-P-UNIVERSITY  PIC X(40)  VALUE SPACES.
          05 WS-P-GRAD-YEAR   PIC X(4)   VALUE SPACES.
          05 WS-P-ABOUT       PIC X(200) VALUE SPACES.
          05 WS-P-EDU.
             10 WS-EDU OCCURS 3 TIMES.
                15 WS-EDU-DEGREE  PIC X(40) VALUE SPACES.
                15 WS-EDU-SCHOOL  PIC X(40) VALUE SPACES.
                15 WS-EDU-YEAR    PIC X(20) VALUE SPACES.
          05 WS-P-WORK.
             10 WS-WORK OCCURS 3 TIMES.
                15 WS-WORK-TITLE     PIC X(40) VALUE SPACES.
                15 WS-WORK-EMPLOYER  PIC X(40) VALUE SPACES.
                15 WS-WORK-DATES     PIC X(40) VALUE SPACES.
                15 WS-WORK-DESC      PIC X(100) VALUE SPACES.

       01 WS-CONN-TOTAL PIC 99 VALUE 0.
       01 WS-CONN-IDX PIC 99 VALUE 0.
       01 WS-CONN-ARRAY.
          05 WS-CONN-ITEM OCCURS 25 TIMES.
             10 WS-CA-SENDER PIC X(20).
             10 WS-CA-RECIP  PIC X(20).
             10 WS-CA-STATUS PIC X(20).

       01 WS-NETWORK-DISPLAY-USER PIC X(20).
       01 WS-NET-FNAME PIC X(20).
       01 WS-NET-LNAME PIC X(20).
       01 WS-NET-UNIVERSITY PIC X(40).
       01 WS-NET-MAJOR PIC X(40).

       01 WS-NET-PARSED-USERNAME PIC X(20).
       01 WS-NET-PARSED-FNAME PIC X(20).
       01 WS-NET-PARSED-LNAME PIC X(20).
       01 WS-NET-PARSED-MAJOR PIC X(40).
       01 WS-NET-PARSED-UNIVERSITY PIC X(40).

       01 WS-NETWORK-PROFILE-FLAG PIC X VALUE "N".
          88 NETWORK-PROFILE-FOUND VALUE "Y".
          88 NETWORK-PROFILE-NOT-FOUND VALUE "N".

       01 WS-JOB-TITLE PIC X(40) VALUE SPACES.
       01 WS-JOB-DESC  PIC X(200) VALUE SPACES.
       01 WS-JOB-EMPLOYER  PIC X(40) VALUE SPACES.
       01 WS-JOB-LOCATION  PIC X(40) VALUE SPACES.
       01 WS-JOB-SALARY    PIC X(40) VALUE SPACES.
       01 WS-JOB-LINE PIC X(512) VALUE SPACES.

       01 WS-JOBS-EOF PIC X VALUE "N".
          88 JOBS-EOF-YES VALUE "Y".
          88 JOBS-EOF-NO VALUE "N".

       01 WS-JOB-CNT            PIC 999 VALUE 0.
       01 WS-JOB-ID             PIC 999 VALUE 0.
       01 WS-JOB-SELECT         PIC 9(5) VALUE 0.

       01 WS-JOB-PARSE-LINE     PIC X(512).
       01 WS-JOB-PARSE-POSTER   PIC X(20).
       01 WS-JOB-PARSE-TITLE    PIC X(40).
       01 WS-JOB-PARSE-DESC     PIC X(200).
       01 WS-JOB-PARSE-EMP      PIC X(40).
       01 WS-JOB-PARSE-LOC      PIC X(40).
       01 WS-JOB-PARSE-SAL      PIC X(40).

      *>*********************************************
      *> MESSAGING VARIABLES (WEEK 8)               *
      *>*********************************************
       01 WS-MSG-RECIPIENT      PIC X(20) VALUE SPACES.
       01 WS-MSG-CONTENT        PIC X(200) VALUE SPACES.
       01 WS-MSG-LINE           PIC X(512) VALUE SPACES.

       *> FUNCTION CURRENT-DATE returns 21 chars: YYYYMMDDHHMMSSCC+HHMM
       01 WS-MSG-TIMESTAMP-RAW  PIC X(21).
       01 FILLER REDEFINES WS-MSG-TIMESTAMP-RAW.
           05 WS-MSG-TS-YEAR    PIC X(4).
           05 WS-MSG-TS-MONTH   PIC X(2).
           05 WS-MSG-TS-DAY     PIC X(2).
           05 WS-MSG-TS-HOUR    PIC X(2).
           05 WS-MSG-TS-MIN     PIC X(2).
           05 WS-MSG-TS-SEC     PIC X(2).
           05 FILLER            PIC X(7).
       01 WS-MSG-TIMESTAMP      PIC X(20) VALUE SPACES.

       *> Recipient validation flags
       01 WS-MSG-RECIP-FLAG     PIC X VALUE "N".
          88 MSG-RECIP-FOUND    VALUE "Y".
          88 MSG-RECIP-NOT-FOUND VALUE "N".

       *> Connection validation flags
       01 WS-MSG-CONN-FLAG      PIC X VALUE "N".
          88 MSG-IS-CONNECTED   VALUE "Y".
          88 MSG-NOT-CONNECTED  VALUE "N".

       *> View-messages EOF flag
       01 WS-MSG-VIEW-EOF       PIC X VALUE "N".
          88 MSG-VIEW-EOF-YES   VALUE "Y".
          88 MSG-VIEW-EOF-NO    VALUE "N".

       *> Message count for VIEW-MY-MESSAGES
       01 WS-MSG-COUNT-NUM      PIC 99 VALUE 0.
       01 WS-MSG-COUNT-DISP     PIC Z9.

       *> Parsed fields from a single message record
       01 WS-MSG-PARSE-SENDER   PIC X(20).
       01 WS-MSG-PARSE-RECIP    PIC X(20).
       01 WS-MSG-PARSE-TS       PIC X(20).
       01 WS-MSG-PARSE-BODY     PIC X(200).

       *> Local exit flag for MESSAGES-MENU only.
       *> Using a separate flag prevents "Back" from
       *> triggering the top-level program exit.
       01 WS-MSG-MENU-EXIT      PIC X VALUE "N".
          88 MSG-MENU-EXIT-YES  VALUE "Y".
          88 MSG-MENU-EXIT-NO   VALUE "N".

       *> Local exit flag for JOB-SEARCH-MENU only.
       01 WS-JOB-MENU-EXIT      PIC X VALUE "N".
          88 JOB-MENU-EXIT-YES  VALUE "Y".
          88 JOB-MENU-EXIT-NO   VALUE "N".

       *> Local exit flag for LEARN-A-SKILL only.
       01 WS-LEARN-MENU-EXIT    PIC X VALUE "N".
          88 LEARN-MENU-EXIT-YES VALUE "Y".
          88 LEARN-MENU-EXIT-NO  VALUE "N".


       PROCEDURE DIVISION.
       MAIN.
           PERFORM INIT-FILES
           PERFORM LOAD-ACCOUNTS-FROM-FILE
           PERFORM LOAD-CONNECTIONS-FROM-FILE

           PERFORM TOP-LEVEL-MENU
               UNTIL EXIT-YES OR EOF-YES

           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO WS-OUTLINE
           PERFORM PRINT-LINE

           PERFORM CLOSE-FILES
           STOP RUN.

       INIT-FILES.
           OPEN INPUT IN-FILE
           IF WS-IN-STAT NOT = "00"
               DISPLAY "Cannot open InCollege-Input.txt. Status="
                   WS-IN-STAT
               STOP RUN
           END-IF

           OPEN OUTPUT OUT-FILE
           IF WS-OUT-STAT NOT = "00"
               DISPLAY "Cannot create InCollege-Output.txt. Status="
                   WS-OUT-STAT
               STOP RUN
           END-IF
           
           OPEN INPUT ACCT-FILE
               IF WS-ACCT-STAT = "00"
                  CONTINUE
               ELSE
                  IF WS-ACCT-STAT = "05" OR WS-ACCT-STAT = "35"
                     CLOSE ACCT-FILE
                     OPEN OUTPUT ACCT-FILE
                     IF WS-ACCT-STAT NOT = "00" AND WS-ACCT-STAT NOT = "05"
                        DISPLAY "Cannot create accounts.dat. Status = "
                            WS-ACCT-STAT
                        STOP RUN
                     END-IF
                     CLOSE ACCT-FILE
                     OPEN INPUT ACCT-FILE
                     IF WS-ACCT-STAT NOT = "00"
                        DISPLAY "Cannot create accounts.dat. Status = "
                            WS-ACCT-STAT
                        STOP RUN
                     END-IF
                  ELSE
                     DISPLAY "Cannot create accounts.dat. Status = "
                         WS-ACCT-STAT
                     STOP RUN
                  END-IF
               END-IF

           OPEN INPUT PROFILES-FILE
           IF PROFILES-STATUS = "00"
               CLOSE PROFILES-FILE
           ELSE
               OPEN OUTPUT PROFILES-FILE
               IF PROFILES-STATUS NOT = "00"
                   DISPLAY "ERROR: Cannot create profiles.dat. Status="
                       PROFILES-STATUS
               END-IF
               CLOSE PROFILES-FILE
           END-IF

           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STAT = "00"
               CLOSE JOBS-FILE
           ELSE
               IF WS-JOBS-STAT = "05" OR WS-JOBS-STAT = "35"
                   OPEN OUTPUT JOBS-FILE
                   IF WS-JOBS-STAT = "00" OR WS-JOBS-STAT = "05"
                       CLOSE JOBS-FILE
                   ELSE
                       DISPLAY "ERROR: Cannot create jobs.dat. Status="
                           WS-JOBS-STAT
                   END-IF
               ELSE
                   DISPLAY "ERROR: Cannot open jobs.dat. Status="
                       WS-JOBS-STAT
               END-IF
           END-IF.

           OPEN INPUT APPS-FILE
           IF WS-APPS-STAT = "00"
               CLOSE APPS-FILE
           ELSE
               IF WS-APPS-STAT = "05" OR WS-APPS-STAT = "35"
                   OPEN OUTPUT APPS-FILE
                   IF WS-APPS-STAT = "00" OR WS-APPS-STAT = "05"
                       CLOSE APPS-FILE
                   ELSE
                       DISPLAY "ERROR: Cannot create applications.dat. Status="
                           WS-APPS-STAT
                   END-IF
               ELSE
                   DISPLAY "ERROR: Cannot open applications.dat. Status="
                       WS-APPS-STAT
               END-IF
           END-IF.

           *> messages.dat is OPTIONAL - created on first write by SAVE-MESSAGE.
           *> CLOSE on any open outcome so file is not left in virtual-open
           *> state before SAVE-MESSAGE later calls OPEN EXTEND.
           OPEN INPUT MSGS-FILE
           IF WS-MSGS-STAT = "00" OR WS-MSGS-STAT = "05"
               OR WS-MSGS-STAT = "35"
               CLOSE MSGS-FILE
           ELSE
               DISPLAY "ERROR: Cannot access messages.dat. Status="
                   WS-MSGS-STAT
           END-IF.

       CLOSE-FILES.
           CLOSE IN-FILE
           CLOSE OUT-FILE
           CLOSE ACCT-FILE
           CLOSE PROFILES-FILE
           CLOSE TEMP-PROFILES-FILE
           CLOSE CONN-FILE
           CLOSE JOBS-FILE
           CLOSE APPS-FILE
           CLOSE MSGS-FILE.

       GET-NEXT-INPUT.
           READ IN-FILE
               AT END
                   SET EOF-YES TO TRUE
                   MOVE SPACES TO WS-INLINE
               NOT AT END
                   MOVE IN-REC TO WS-INLINE
           END-READ.

       ECHO-INPUT.
           MOVE SPACES TO WS-OUTLINE
           STRING
              FUNCTION TRIM(WS-PREV-PROMPT) DELIMITED BY SIZE
              " " DELIMITED BY SIZE
              FUNCTION TRIM(WS-INLINE) DELIMITED BY SIZE
              INTO WS-OUTLINE
           END-STRING
           WRITE OUT-REC FROM WS-OUTLINE
           IF WS-OUT-STAT NOT = "00"
              DISPLAY "ERR: failed writing to outfile. Status= "
                  WS-OUT-STAT
              STOP RUN
           END-IF.

       REQUIRE-INPUT.
           PERFORM GET-NEXT-INPUT
           IF EOF-YES
               SET EXIT-YES TO TRUE
               EXIT PARAGRAPH
           END-IF
           DISPLAY FUNCTION TRIM(WS-INLINE)
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

       TOP-LEVEL-MENU.
           MOVE "Welcome to InCollege!" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "---------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "1. Log In" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "2. Create New Account" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-INLINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
              EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED

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
           MOVE FUNCTION TRIM(WS-TMP-USER) TO WS-USERNAME(WS-ACCOUNT-COUNT)
           MOVE FUNCTION TRIM(WS-TMP-PASS) TO WS-PASSWORD(WS-ACCOUNT-COUNT).

       SAVE-ACCOUNTS-TO-FILE.
           CLOSE ACCT-FILE
           OPEN OUTPUT ACCT-FILE
           IF WS-ACCT-STAT NOT = "00"
               MOVE "ERR: Cannot write accounts.dat. Status=" TO WS-OUTLINE
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

       CREATE-NEW-ACCOUNT.
           IF WS-ACCOUNT-COUNT >= 5
               MOVE "All permitted accounts have been created, please come back later"
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
               MOVE "Username already exists. Please choose a different username."
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
               MOVE "Password must contain at least one capital letter."
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
               MOVE "Password must contain at least a special character"
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-INPUT-PASSWORD TO WS-CURRENT-PASSWORD
           SET PASSWORD-VALID TO TRUE.

       AUTHENTICATE-USER.
           IF WS-ACCOUNT-COUNT = 0
               MOVE "Incorrect username/password, please try again"
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
                   MOVE "You have successfully logged in." TO WS-OUTLINE
                   PERFORM PRINT-LINE
                   PERFORM AFTER-LOGIN
               ELSE
                   MOVE "Incorrect username/password, please try again"
                       TO WS-OUTLINE
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM
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

       CORE-PROFILE-ROUTINE.
           MOVE SPACES TO WS-P-FNAME
           MOVE SPACES TO WS-P-LNAME
           MOVE SPACES TO WS-P-UNIVERSITY
           MOVE SPACES TO WS-P-MAJOR
           MOVE SPACES TO WS-P-GRAD-YEAR
           MOVE SPACES TO WS-P-ABOUT
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               MOVE SPACES TO WS-WORK-TITLE(WS-I)
               MOVE SPACES TO WS-WORK-EMPLOYER(WS-I)
               MOVE SPACES TO WS-WORK-DATES(WS-I)
               MOVE SPACES TO WS-WORK-DESC(WS-I)
               ADD 1 TO WS-I
           END-PERFORM
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               MOVE SPACES TO WS-EDU-DEGREE(WS-I)
               MOVE SPACES TO WS-EDU-SCHOOL(WS-I)
               MOVE SPACES TO WS-EDU-YEAR(WS-I)
               ADD 1 TO WS-I
           END-PERFORM
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
           PERFORM WITH TEST AFTER UNTIL
               (FUNCTION STORED-CHAR-LENGTH(
                FUNCTION TRIM(WS-P-GRAD-YEAR)) = 4)
               AND FUNCTION TRIM(WS-P-GRAD-YEAR) IS NUMERIC
              MOVE "Enter Graduation Year (YYYY): " TO WS-OUTLINE
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
              IF EXIT-YES OR EOF-YES
                  EXIT PARAGRAPH
              END-IF
              MOVE FUNCTION TRIM(WS-INLINE) TO WS-P-GRAD-YEAR
              IF NOT (FUNCTION STORED-CHAR-LENGTH(
                      FUNCTION TRIM(WS-P-GRAD-YEAR)) = 4)
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
              DISPLAY SPACE
              MOVE SPACES TO WS-OUTLINE
              STRING "Experience #" DELIMITED BY SIZE
                     WS-I DELIMITED BY SIZE
                     " - Title: " DELIMITED BY SIZE
                     INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
              MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
              IF FUNCTION UPPER-CASE(WS-TRIMMED) = "DONE"
                 EXIT PERFORM
              END-IF
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-WORK-TITLE(WS-I)
              MOVE SPACES TO WS-OUTLINE
              STRING "Experience #" DELIMITED BY SIZE
                     WS-I DELIMITED BY SIZE
                     " - Company/Organization: " DELIMITED BY SIZE
                     INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-WORK-EMPLOYER(WS-I)
              MOVE SPACES TO WS-OUTLINE
              STRING "Experience #" DELIMITED BY SIZE
                     WS-I DELIMITED BY SIZE
                     " - Dates (e.g., Summer 2024): " DELIMITED BY SIZE
                     INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-WORK-DATES(WS-I)
              MOVE SPACES TO WS-OUTLINE
              STRING "Experience #" DELIMITED BY SIZE
                     WS-I DELIMITED BY SIZE
                     " - Description (optional, max 100 chars, blank to skip): "
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
              DISPLAY SPACE
              MOVE SPACES TO WS-OUTLINE
              STRING "Education #" DELIMITED BY SIZE
                     WS-I DELIMITED BY SIZE
                     " - Degree: " DELIMITED BY SIZE
                     INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
              MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
              IF FUNCTION UPPER-CASE(WS-TRIMMED) = "DONE"
                 EXIT PERFORM
              END-IF
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-EDU-DEGREE(WS-I)
              MOVE SPACES TO WS-OUTLINE
              STRING "Education #" DELIMITED BY SIZE
                     WS-I DELIMITED BY SIZE
                     " - University/College: " DELIMITED BY SIZE
                     INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
              MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-EDU-SCHOOL(WS-I)
              MOVE SPACES TO WS-OUTLINE
              STRING "Education #" DELIMITED BY SIZE
                     WS-I DELIMITED BY SIZE
                     " - Years Attended (e.g., 2023-2025): " DELIMITED BY SIZE
                     INTO WS-OUTLINE
              END-STRING
              PERFORM PRINT-INLINE
              PERFORM REQUIRE-INPUT
              MOVE FUNCTION TRIM(WS-INLINE)(1:20) TO WS-EDU-YEAR(WS-I)
              ADD 1 TO WS-I
           END-PERFORM.
           PERFORM SAVE-PROFILE
           MOVE "Profile saved successfully!" TO WS-OUTLINE
           PERFORM PRINT-LINE.

       AFTER-LOGIN.
           PERFORM LOAD-PROFILE
           MOVE SPACES TO WS-OUTLINE
           STRING "Welcome, " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
                  "!" DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           PERFORM AFTER-LOGIN-MENU UNTIL EXIT-YES OR EOF-YES.
           PERFORM CLEAR-PROFILE-DATA
           IF EXIT-YES AND NOT EOF-YES
               SET EXIT-NO TO TRUE
           END-IF.

       AFTER-LOGIN-MENU.
           MOVE "1. Create/Edit My Profile" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "2. View My Profile" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "3. Search for User" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "4. View My Pending Connection Requests" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "5. Learn a New Skill" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "6. View My Network" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "7. Go Back" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "8. Job Search/Internship" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "9. Messages" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-INLINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
              EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED

           EVALUATE WS-TRIMMED
               WHEN "1"
                   PERFORM CORE-PROFILE-ROUTINE
               WHEN "2"
                   PERFORM VIEW-PROFILE
               WHEN "3"
                   PERFORM FIND-USER
               WHEN "4"
                   PERFORM MANAGE-PENDING-REQUESTS
               WHEN "5"
                   PERFORM LEARN-A-SKILL
               WHEN "6"
                   PERFORM VIEW-MY-NETWORK
               WHEN "7"
                   SET EXIT-YES TO TRUE
                   EXIT PARAGRAPH
               WHEN "8"
                   PERFORM JOB-SEARCH-MENU
               WHEN "9"
                   PERFORM MESSAGES-MENU
               WHEN "Logout"
               WHEN "log out"
               WHEN "logout"
               WHEN "Log out"
               WHEN "Log Out"
                   SET EXIT-YES TO TRUE
               WHEN OTHER
                   MOVE "Invalid choice." TO WS-OUTLINE
                   PERFORM PRINT-LINE
           END-EVALUATE.

       LEARN-A-SKILL.
           SET LEARN-MENU-EXIT-NO TO TRUE

           PERFORM UNTIL LEARN-MENU-EXIT-YES OR EOF-YES

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
               IF EOF-YES
                   SET EXIT-YES TO TRUE
                   EXIT PERFORM
               END-IF
               IF EXIT-YES
                   EXIT PERFORM
               END-IF
               MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
               IF WS-TRIMMED = "Logout" OR WS-TRIMMED = "logout"
                   OR WS-TRIMMED = "log out" OR WS-TRIMMED = "Log out"
                   OR WS-TRIMMED = "Log Out"
                   SET EXIT-YES TO TRUE
                   EXIT PERFORM
               END-IF
               IF WS-TRIMMED = "Go Back" OR WS-TRIMMED = "6"
                       OR WS-TRIMMED = "back"
                   SET LEARN-MENU-EXIT-YES TO TRUE
               ELSE
                   MOVE "This skill is under construction." TO WS-OUTLINE
                   PERFORM PRINT-LINE
               END-IF

           END-PERFORM.

       FIND-USER.
           MOVE "Enter the full name of the person you are looking for:"
               TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-SEARCH-INPUT
           PERFORM PARSE-SEARCH-NAME
           PERFORM SEARCH-FOR-USER
           IF USER-SEARCH-FOUND
               PERFORM DISPLAY-FOUND-USER-PROFILE
           ELSE
               MOVE "No one by that name could be found." TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF.

       PARSE-SEARCH-NAME.
           MOVE SPACES TO WS-SEARCH-FNAME
           MOVE SPACES TO WS-SEARCH-LNAME
           UNSTRING WS-SEARCH-INPUT
               DELIMITED BY " " OR "  "
               INTO WS-SEARCH-FNAME WS-SEARCH-LNAME
           END-UNSTRING
           MOVE SPACES TO WS-SEARCH-FULL-NAME
           STRING
               FUNCTION TRIM(WS-SEARCH-FNAME) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(WS-SEARCH-LNAME) DELIMITED BY SIZE
               INTO WS-SEARCH-FULL-NAME
           END-STRING.

       SEARCH-FOR-USER.
           SET USER-SEARCH-NOT-FOUND TO TRUE
           SET SEARCH-EOF-NO TO TRUE
           OPEN INPUT PROFILES-FILE
           IF PROFILES-STATUS = "00"
               PERFORM READ-AND-COMPARE-PROFILE
                   UNTIL SEARCH-EOF-YES OR USER-SEARCH-FOUND
               CLOSE PROFILES-FILE
           ELSE
               SET USER-SEARCH-NOT-FOUND TO TRUE
           END-IF
           SET SEARCH-EOF-NO TO TRUE.

       READ-AND-COMPARE-PROFILE.
           READ PROFILES-FILE INTO WS-PROFILE-LINE
               AT END
                   SET SEARCH-EOF-YES TO TRUE
               NOT AT END
                   MOVE SPACES TO WS-PARSED-FNAME
                   MOVE SPACES TO WS-PARSED-LNAME
                   UNSTRING WS-PROFILE-LINE
                       DELIMITED BY "|"
                       INTO WS-TMP-USER WS-PARSED-FNAME WS-PARSED-LNAME
                   END-UNSTRING
                   MOVE SPACES TO WS-CURRENT-FULL-NAME
                   STRING
                       FUNCTION TRIM(WS-PARSED-FNAME) DELIMITED BY SIZE
                       " " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PARSED-LNAME) DELIMITED BY SIZE
                       INTO WS-CURRENT-FULL-NAME
                   END-STRING
                   IF FUNCTION UPPER-CASE(
                      FUNCTION TRIM(WS-CURRENT-FULL-NAME)) =
                      FUNCTION UPPER-CASE(
                      FUNCTION TRIM(WS-SEARCH-FULL-NAME))
                       SET USER-SEARCH-FOUND TO TRUE
                       PERFORM PARSE-PROFILE-LINE
                   END-IF
           END-READ.

       DISPLAY-FOUND-USER-PROFILE.
           MOVE "--- Found User Profile ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PARSED-FNAME) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PARSED-LNAME) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "University: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PARSED-UNIVERSITY) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Major: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PARSED-MAJOR) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Graduation Year: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PARSED-GRAD-YEAR) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           IF WS-PARSED-ABOUT NOT = SPACES
               MOVE SPACES TO WS-OUTLINE
               STRING "About Me: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PARSED-ABOUT) DELIMITED BY SIZE
                      INTO WS-OUTLINE
               END-STRING
               PERFORM PRINT-LINE
           END-IF
           MOVE SPACES TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Experience:" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               IF WS-PARSED-WORK-TITLE(WS-I) NOT = SPACES
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Title: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PARSED-WORK-TITLE(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Company: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PARSED-WORK-EMPLOYER(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Dates: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PARSED-WORK-DATES(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   IF WS-PARSED-WORK-DESC(WS-I) NOT = SPACES
                       MOVE SPACES TO WS-OUTLINE
                       STRING " Description: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-PARSED-WORK-DESC(WS-I))
                              DELIMITED BY SIZE INTO WS-OUTLINE
                       END-STRING
                       PERFORM PRINT-LINE
                   END-IF
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
           IF WS-PARSED-WORK-TITLE(1) = SPACES
               MOVE " None" TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF
           MOVE SPACES TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Education:" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               IF WS-PARSED-EDU-DEGREE(WS-I) NOT = SPACES
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Degree: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PARSED-EDU-DEGREE(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " University: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PARSED-EDU-SCHOOL(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Years: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PARSED-EDU-YEAR(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
           IF WS-PARSED-EDU-DEGREE(1) = SPACES
               MOVE " None" TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF
           MOVE SPACES TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "-------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE
           PERFORM PROFILE-ACTION-MENU.

       PROFILE-ACTION-MENU.
           MOVE "1. Send Connection Request" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "2. Back to Main Menu" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
           EVALUATE WS-TRIMMED
               WHEN "1"
                   PERFORM SEND-CONNECTION-REQUEST
               WHEN "2"
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice." TO WS-OUTLINE
                   PERFORM PRINT-LINE
           END-EVALUATE.

       LOAD-PROFILE.
           SET PROFILE-NOT-FOUND TO TRUE
           SET PROFILE-EOF-NO TO TRUE
           OPEN INPUT PROFILES-FILE
           IF PROFILES-STATUS = "00"
               PERFORM READ-PROFILE-FILE
                   UNTIL PROFILE-EOF-YES OR PROFILE-EXISTS
               IF PROFILE-EXISTS
                   MOVE WS-PARSED-FNAME TO WS-P-FNAME
                   MOVE WS-PARSED-LNAME TO WS-P-LNAME
                   MOVE WS-PARSED-MAJOR TO WS-P-MAJOR
                   MOVE WS-PARSED-UNIVERSITY TO WS-P-UNIVERSITY
                   MOVE WS-PARSED-GRAD-YEAR TO WS-P-GRAD-YEAR
                   MOVE WS-PARSED-ABOUT TO WS-P-ABOUT
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
               CONTINUE
           END-IF
           SET PROFILE-EOF-NO TO TRUE.
       
       READ-PROFILE-FILE.
           READ PROFILES-FILE INTO WS-PROFILE-LINE
               AT END
                   SET PROFILE-EOF-YES TO TRUE
               NOT AT END
                   PERFORM PARSE-PROFILE-LINE
                   IF WS-PARSED-USERNAME = WS-CURRENT-USERNAME
                       SET PROFILE-EXISTS TO TRUE
                   END-IF
           END-READ.
       
       PARSE-PROFILE-LINE.
           MOVE SPACES TO WS-PARSED-PROFILE
           UNSTRING WS-PROFILE-LINE
               DELIMITED BY "|"
               INTO
                   WS-PARSED-USERNAME  WS-PARSED-FNAME
                   WS-PARSED-LNAME     WS-PARSED-UNIVERSITY
                   WS-PARSED-MAJOR     WS-PARSED-GRAD-YEAR
                   WS-PARSED-ABOUT
                   WS-PARSED-WORK-TITLE(1)  WS-PARSED-WORK-EMPLOYER(1)
                   WS-PARSED-WORK-DATES(1)  WS-PARSED-WORK-DESC(1)
                   WS-PARSED-WORK-TITLE(2)  WS-PARSED-WORK-EMPLOYER(2)
                   WS-PARSED-WORK-DATES(2)  WS-PARSED-WORK-DESC(2)
                   WS-PARSED-WORK-TITLE(3)  WS-PARSED-WORK-EMPLOYER(3)
                   WS-PARSED-WORK-DATES(3)  WS-PARSED-WORK-DESC(3)
                   WS-PARSED-EDU-DEGREE(1)  WS-PARSED-EDU-SCHOOL(1)
                   WS-PARSED-EDU-YEAR(1)
                   WS-PARSED-EDU-DEGREE(2)  WS-PARSED-EDU-SCHOOL(2)
                   WS-PARSED-EDU-YEAR(2)
                   WS-PARSED-EDU-DEGREE(3)  WS-PARSED-EDU-SCHOOL(3)
                   WS-PARSED-EDU-YEAR(3)
           END-UNSTRING.
       
       SAVE-PROFILE.
           SET PROFILE-NOT-FOUND TO TRUE
           SET PROFILE-EOF-NO TO TRUE
           CLOSE PROFILES-FILE
           CLOSE TEMP-PROFILES-FILE
           OPEN INPUT PROFILES-FILE
           IF PROFILES-STATUS = "00"
               CLOSE PROFILES-FILE
               PERFORM REWRITE-PROFILE-FILE
           ELSE
               OPEN OUTPUT PROFILES-FILE
               IF PROFILES-STATUS = "00"
                   PERFORM WRITE-CURRENT-PROFILE-NEW
                   CLOSE PROFILES-FILE
               ELSE
                   DISPLAY "ERROR: Cannot create profiles.dat. Status="
                       PROFILES-STATUS
               END-IF
           END-IF
           SET PROFILE-EOF-NO TO TRUE.
       
       REWRITE-PROFILE-FILE.
           OPEN INPUT PROFILES-FILE
           IF PROFILES-STATUS NOT = "00"
               DISPLAY "ERROR: Cannot open profiles.dat. Status="
                   PROFILES-STATUS
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT TEMP-PROFILES-FILE
           IF TEMP-PROFILES-STATUS NOT = "00"
               DISPLAY "ERROR: Cannot create temp profile file. Status="
                   TEMP-PROFILES-STATUS
               CLOSE PROFILES-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL PROFILE-EOF-YES
               READ PROFILES-FILE INTO WS-PROFILE-LINE
                   AT END
                       SET PROFILE-EOF-YES TO TRUE
                   NOT AT END
                       PERFORM PARSE-PROFILE-LINE
                       IF WS-PARSED-USERNAME = WS-CURRENT-USERNAME
                           SET PROFILE-EXISTS TO TRUE
                           PERFORM WRITE-CURRENT-PROFILE
                       ELSE
                           WRITE TEMP-PROFILE-REC FROM WS-PROFILE-LINE
                       END-IF
               END-READ
           END-PERFORM
           IF PROFILE-NOT-FOUND
               PERFORM WRITE-CURRENT-PROFILE
           END-IF
           CLOSE TEMP-PROFILES-FILE
           SET PROFILE-EOF-NO TO TRUE
           PERFORM REPLACE-PROFILE-FILE.
       
       WRITE-CURRENT-PROFILE-NEW.
           MOVE SPACES TO WS-PROFILE-LINE
           STRING
               FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-FNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-LNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-UNIVERSITY) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-MAJOR) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-GRAD-YEAR) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-ABOUT) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(3)) DELIMITED BY SIZE
               INTO WS-PROFILE-LINE
           END-STRING
           WRITE PROFILE-REC FROM WS-PROFILE-LINE.
       
       WRITE-CURRENT-PROFILE.
           MOVE SPACES TO WS-PROFILE-LINE
           STRING
               FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-FNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-LNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-UNIVERSITY) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-MAJOR) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-GRAD-YEAR) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-P-ABOUT) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-TITLE(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-EMPLOYER(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DATES(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-WORK-DESC(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(1)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(2)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-DEGREE(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-SCHOOL(3)) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-EDU-YEAR(3)) DELIMITED BY SIZE
               INTO WS-PROFILE-LINE
           END-STRING
           WRITE TEMP-PROFILE-REC FROM WS-PROFILE-LINE.
       
       REPLACE-PROFILE-FILE.
           CALL "SYSTEM" USING "rm -f profiles.dat"
           END-CALL
           CALL "SYSTEM" USING "mv temp-profiles.dat profiles.dat"
           END-CALL
           IF RETURN-CODE NOT = 0
               DISPLAY "ERROR: Failed to replace profiles.dat. Data may be lost."
           END-IF.
       
       CLEAR-PROFILE-DATA.
           MOVE SPACES TO WS-PROFILE.
       
       VIEW-PROFILE.
           MOVE "--- Your Profile ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-P-FNAME) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-P-LNAME) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "University: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-P-UNIVERSITY) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Major: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-P-MAJOR) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Graduation Year: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-P-GRAD-YEAR) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           IF WS-P-ABOUT NOT = SPACES
               MOVE SPACES TO WS-OUTLINE
               STRING "About Me: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-P-ABOUT) DELIMITED BY SIZE
                      INTO WS-OUTLINE
               END-STRING
               PERFORM PRINT-LINE
           END-IF
           MOVE SPACES TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Experience:" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               IF WS-WORK-TITLE(WS-I) NOT = SPACES
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Title: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-WORK-TITLE(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Company: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-WORK-EMPLOYER(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Dates: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-WORK-DATES(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   IF WS-WORK-DESC(WS-I) NOT = SPACES
                       MOVE SPACES TO WS-OUTLINE
                       STRING " Description: " DELIMITED BY SIZE
                              FUNCTION TRIM(WS-WORK-DESC(WS-I))
                              DELIMITED BY SIZE INTO WS-OUTLINE
                       END-STRING
                       PERFORM PRINT-LINE
                   END-IF
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
           IF WS-WORK-TITLE(1) = SPACES
               MOVE " None" TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF
           MOVE "Education:" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               IF WS-EDU-DEGREE(WS-I) NOT = SPACES
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Degree: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-EDU-DEGREE(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " University: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-EDU-SCHOOL(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   STRING " Years: " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-EDU-YEAR(WS-I))
                          DELIMITED BY SIZE INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
                   MOVE SPACES TO WS-OUTLINE
                   PERFORM PRINT-LINE
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
           IF WS-EDU-DEGREE(1) = SPACES
               MOVE " None" TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF
           MOVE "--------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.

      *>*********************************************
      *> CONNECTION REQUEST ROUTINES (COPYBOOK)     *
      *>*********************************************
       COPY SENDREQ_SRC.
       COPY VIEWREQ_SRC.
       COPY VIEWNET_SRC.

      *>*********************************************
      *> JOB SEARCH / INTERNSHIP ROUTINES           *
      *>*********************************************

       JOB-SEARCH-MENU.
           SET JOB-MENU-EXIT-NO TO TRUE

           PERFORM UNTIL JOB-MENU-EXIT-YES OR EOF-YES

               MOVE "--- Job Search/Internship Menu ---" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "1. Post a Job/Internship" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "2. Browse Jobs/Internships" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "3. View My Applications" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "4. Back to Main Menu" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUTLINE
               PERFORM PRINT-INLINE

               PERFORM REQUIRE-INPUT
               IF EOF-YES
                   SET EXIT-YES TO TRUE
                   EXIT PERFORM
               END-IF
               IF EXIT-YES
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED

               EVALUATE WS-TRIMMED
                   WHEN "1"
                       PERFORM POST-JOB-INTERN
                   WHEN "2"
                       PERFORM BROWSE-JOBS
                   WHEN "3"
                       PERFORM VIEW-MY-APPLICATIONS
                   WHEN "4"
                       SET JOB-MENU-EXIT-YES TO TRUE
                   WHEN OTHER
                       MOVE "Invalid choice." TO WS-OUTLINE
                       PERFORM PRINT-LINE
               END-EVALUATE

           END-PERFORM.

       POST-JOB-INTERN.
           MOVE "--- Post a New Job/Internship ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter Job Title:" TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-JOB-TITLE
           MOVE "Enter Description (max 200 chars):" TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:200) TO WS-JOB-DESC
           MOVE "Enter Employer Name:" TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-JOB-EMPLOYER
           MOVE "Enter Location:" TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-JOB-LOCATION
           MOVE "Enter Salary (optional, enter 'NONE' to skip):"
               TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
           IF FUNCTION UPPER-CASE(WS-TRIMMED) = "NONE"
               MOVE SPACES TO WS-JOB-SALARY
           ELSE
               MOVE FUNCTION TRIM(WS-INLINE)(1:40) TO WS-JOB-SALARY
           END-IF
           PERFORM SAVE-JOB-POSTING
           MOVE "Job posted successfully!" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "----------------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.

       SAVE-JOB-POSTING.
           MOVE SPACES TO WS-JOB-LINE
           STRING
               FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-TITLE) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-DESC) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-EMPLOYER) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-LOCATION) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-SALARY) DELIMITED BY SIZE
               INTO WS-JOB-LINE
           END-STRING
           OPEN EXTEND JOBS-FILE
           IF WS-JOBS-STAT = "41"
               CLOSE JOBS-FILE
               OPEN EXTEND JOBS-FILE
           END-IF
           IF WS-JOBS-STAT = "00" OR WS-JOBS-STAT = "05"
               WRITE JOBS-REC FROM WS-JOB-LINE
               CLOSE JOBS-FILE
           ELSE
               DISPLAY "ERROR: Cannot write to jobs.dat. Status="
                   WS-JOBS-STAT
           END-IF.

       COUNT-JOBS.
           MOVE 0 TO WS-JOB-CNT
           SET JOBS-EOF-NO TO TRUE
           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STAT NOT = "00"
               CLOSE JOBS-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL JOBS-EOF-YES
               READ JOBS-FILE INTO WS-JOB-LINE
                   AT END
                       SET JOBS-EOF-YES TO TRUE
                   NOT AT END
                       IF FUNCTION TRIM(WS-JOB-LINE) NOT = SPACES
                           ADD 1 TO WS-JOB-CNT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JOBS-FILE
           SET JOBS-EOF-NO TO TRUE.

       DISPLAY-JOB-LIST.
           MOVE 0 TO WS-JOB-ID
           SET JOBS-EOF-NO TO TRUE
           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STAT NOT = "00"
               CLOSE JOBS-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL JOBS-EOF-YES
               READ JOBS-FILE INTO WS-JOB-LINE
                   AT END
                       SET JOBS-EOF-YES TO TRUE
                   NOT AT END
                       IF FUNCTION TRIM(WS-JOB-LINE) NOT = SPACES
                           ADD 1 TO WS-JOB-ID
                           PERFORM PARSE-JOB-LINE-FOR-SUMMARY
                           PERFORM PRINT-JOB-SUMMARY
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JOBS-FILE
           SET JOBS-EOF-NO TO TRUE.

       PARSE-JOB-LINE-FOR-SUMMARY.
           MOVE SPACES TO WS-JOB-PARSE-POSTER
           MOVE SPACES TO WS-JOB-PARSE-TITLE
           MOVE SPACES TO WS-JOB-PARSE-DESC
           MOVE SPACES TO WS-JOB-PARSE-EMP
           MOVE SPACES TO WS-JOB-PARSE-LOC
           MOVE SPACES TO WS-JOB-PARSE-SAL
           UNSTRING WS-JOB-LINE
               DELIMITED BY "|"
               INTO WS-JOB-PARSE-POSTER WS-JOB-PARSE-TITLE
                    WS-JOB-PARSE-DESC   WS-JOB-PARSE-EMP
                    WS-JOB-PARSE-LOC    WS-JOB-PARSE-SAL
           END-UNSTRING.

       PRINT-JOB-SUMMARY.
           MOVE SPACES TO WS-OUTLINE
           STRING
               FUNCTION TRIM(WS-JOB-PARSE-TITLE) DELIMITED BY SIZE
               " at " DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-PARSE-EMP) DELIMITED BY SIZE
               " (" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-PARSE-LOC) DELIMITED BY SIZE
               ")" DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE.

       LOAD-JOB-BY-NUM.
           MOVE 0 TO WS-JOB-ID
           SET JOBS-EOF-NO TO TRUE
           OPEN INPUT JOBS-FILE
           IF WS-JOBS-STAT NOT = "00"
               CLOSE JOBS-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL JOBS-EOF-YES
               READ JOBS-FILE INTO WS-JOB-LINE
                   AT END
                       SET JOBS-EOF-YES TO TRUE
                   NOT AT END
                       IF FUNCTION TRIM(WS-JOB-LINE) NOT = SPACES
                           ADD 1 TO WS-JOB-ID
                           IF WS-JOB-ID = WS-JOB-SELECT
                               PERFORM PARSE-JOB-LINE-FULL
                               SET JOBS-EOF-YES TO TRUE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JOBS-FILE
           SET JOBS-EOF-NO TO TRUE.

       PARSE-JOB-LINE-FULL.
           MOVE SPACES TO WS-JOB-PARSE-POSTER
           MOVE SPACES TO WS-JOB-PARSE-TITLE
           MOVE SPACES TO WS-JOB-PARSE-DESC
           MOVE SPACES TO WS-JOB-PARSE-EMP
           MOVE SPACES TO WS-JOB-PARSE-LOC
           MOVE SPACES TO WS-JOB-PARSE-SAL
           UNSTRING WS-JOB-LINE
               DELIMITED BY "|"
               INTO WS-JOB-PARSE-POSTER WS-JOB-PARSE-TITLE
                    WS-JOB-PARSE-DESC   WS-JOB-PARSE-EMP
                    WS-JOB-PARSE-LOC    WS-JOB-PARSE-SAL
           END-UNSTRING
           MOVE FUNCTION TRIM(WS-JOB-PARSE-TITLE) TO WS-JOB-TITLE
           MOVE FUNCTION TRIM(WS-JOB-PARSE-DESC)  TO WS-JOB-DESC
           MOVE FUNCTION TRIM(WS-JOB-PARSE-EMP)   TO WS-JOB-EMPLOYER
           MOVE FUNCTION TRIM(WS-JOB-PARSE-LOC)   TO WS-JOB-LOCATION
           MOVE FUNCTION TRIM(WS-JOB-PARSE-SAL)   TO WS-JOB-SALARY.

       VIEW-JOB-DETAILS.
           MOVE "--- Job details ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Title: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Description: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-DESC) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Employer: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Location: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-LOCATION) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           IF FUNCTION TRIM(WS-JOB-SALARY) NOT = SPACES
              AND FUNCTION UPPER-CASE(
                  FUNCTION TRIM(WS-JOB-SALARY)) NOT = "NONE"
               MOVE SPACES TO WS-OUTLINE
               STRING "Salary: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-JOB-SALARY) DELIMITED BY SIZE
                      INTO WS-OUTLINE
               END-STRING
               PERFORM PRINT-LINE
           END-IF
           MOVE "-------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Apply for this job" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Back to job list" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "Enter your choice:" TO WS-OUTLINE
           PERFORM PRINT-INLINE
           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
           EVALUATE TRUE
               WHEN WS-TRIMMED = "1"
                 OR WS-TRIMMED = "Apply for this job"
                   PERFORM NOTE-JOB-APPLICATION
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "Your application for " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-TITLE) DELIMITED BY SIZE
                       " at " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-EMPLOYER) DELIMITED BY SIZE
                       " has been submitted." DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       BROWSE-JOBS.
           PERFORM COUNT-JOBS
           MOVE "--- Available Job Listings ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
           IF WS-JOB-CNT = 0
               MOVE "----------------------------" TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF
           PERFORM DISPLAY-JOB-LIST
           MOVE "----------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE 10000 TO WS-JOB-SELECT
           PERFORM UNTIL WS-JOB-SELECT = 0 OR EXIT-YES OR EOF-YES
               MOVE "Enter job number to view details, or 0 to go back:"
                   TO WS-OUTLINE
               PERFORM PRINT-INLINE
               PERFORM REQUIRE-INPUT
               IF EXIT-YES OR EOF-YES
                   EXIT PARAGRAPH
               END-IF
               MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED
               IF FUNCTION TEST-NUMVAL(WS-TRIMMED) = 1
                   MOVE FUNCTION NUMVAL(WS-TRIMMED) TO WS-JOB-SELECT
               ELSE
                   MOVE 99999 TO WS-JOB-SELECT
               END-IF
               IF WS-JOB-SELECT = 0 AND WS-TRIMMED NOT = "0"
                   MOVE "Invalid choice." TO WS-OUTLINE
                   PERFORM PRINT-LINE
                   MOVE 99999 TO WS-JOB-SELECT
               ELSE
                   IF WS-JOB-SELECT = 0
                       EXIT PERFORM
                   END-IF
                   IF WS-JOB-SELECT < 1 OR WS-JOB-SELECT > WS-JOB-CNT
                       MOVE "Invalid choice." TO WS-OUTLINE
                       PERFORM PRINT-LINE
                       MOVE 99999 TO WS-JOB-SELECT
                   ELSE
                       PERFORM LOAD-JOB-BY-NUM
                       PERFORM VIEW-JOB-DETAILS
                       MOVE "--- Available job listings ---" TO WS-OUTLINE
                       PERFORM PRINT-LINE
                       PERFORM DISPLAY-JOB-LIST
                       MOVE "----------------------------" TO WS-OUTLINE
                       PERFORM PRINT-LINE
                   END-IF
               END-IF
           END-PERFORM
           MOVE 0 TO WS-JOB-SELECT.

       NOTE-JOB-APPLICATION.
           MOVE SPACES TO WS-APPS-LINE
           STRING
               FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-TITLE) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-EMPLOYER) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-LOCATION) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               WS-JOB-SELECT DELIMITED BY SIZE
               INTO WS-APPS-LINE
           END-STRING
           OPEN EXTEND APPS-FILE
           IF WS-APPS-STAT = "41"
               CLOSE APPS-FILE
               OPEN EXTEND APPS-FILE
           END-IF
           IF WS-APPS-STAT = "00" OR WS-APPS-STAT = "05"
               WRITE APPS-REC FROM WS-APPS-LINE
               CLOSE APPS-FILE
           ELSE
               DISPLAY "ERROR: Cannot write to applications.dat. Status="
                   WS-APPS-STAT
           END-IF.

      *>*********************************************
      *> MESSAGES MENU (WEEK 8)                     *
      *> Uses WS-MSG-MENU-EXIT (local flag) so that *
      *> "Back to Main Menu" does NOT set EXIT-YES  *
      *> and accidentally log the user out.         *
      *>*********************************************
       MESSAGES-MENU.
           SET MSG-MENU-EXIT-NO TO TRUE

           PERFORM UNTIL MSG-MENU-EXIT-YES OR EOF-YES

               MOVE "--- Messages Menu ---" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "1. Send a New Message" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "2. View My Messages" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "3. Back to Main Menu" TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "Enter your choice:" TO WS-OUTLINE
               PERFORM PRINT-INLINE

               PERFORM REQUIRE-INPUT
               IF EOF-YES
                   SET EXIT-YES TO TRUE
                   EXIT PERFORM
               END-IF
               IF EXIT-YES
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED

               EVALUATE WS-TRIMMED
                   WHEN "1"
                       PERFORM SEND-MESSAGE
                   WHEN "2"
                       PERFORM VIEW-MY-MESSAGES
                   WHEN "3"
                       SET MSG-MENU-EXIT-YES TO TRUE
                   WHEN OTHER
                       MOVE "Invalid choice." TO WS-OUTLINE
                       PERFORM PRINT-LINE
               END-EVALUATE

           END-PERFORM.

       VIEW-MY-APPLICATIONS.
           MOVE 0 TO WS-APPS-COUNT-NUM
           SET APPS-EOF-NO TO TRUE
           MOVE "--- Your Job Applications ---" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           STRING "Application Summary for " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE "------------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE
           OPEN INPUT APPS-FILE
           IF WS-APPS-STAT NOT = "00"
               MOVE "No applications found." TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "------------------------------" TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL APPS-EOF-YES
               READ APPS-FILE INTO WS-APPS-LINE
                   AT END
                       SET APPS-EOF-YES TO TRUE
                   NOT AT END
                       MOVE SPACES TO WS-APP-PARSE-USER
                       MOVE SPACES TO WS-APP-PARSE-TITLE
                       MOVE SPACES TO WS-APP-PARSE-EMP
                       MOVE SPACES TO WS-APP-PARSE-LOC
                       UNSTRING WS-APPS-LINE
                           DELIMITED BY "|"
                           INTO WS-APP-PARSE-USER  WS-APP-PARSE-TITLE
                                WS-APP-PARSE-EMP   WS-APP-PARSE-LOC
                                WS-APP-PARSE-JOBNUM
                       END-UNSTRING
                       IF FUNCTION TRIM(WS-APP-PARSE-USER) =
                          FUNCTION TRIM(WS-CURRENT-USERNAME)
                           ADD 1 TO WS-APPS-COUNT-NUM
                           MOVE SPACES TO WS-OUTLINE
                           STRING "Job Title: " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-APP-PARSE-TITLE)
                                  DELIMITED BY SIZE INTO WS-OUTLINE
                           END-STRING
                           PERFORM PRINT-LINE
                           MOVE SPACES TO WS-OUTLINE
                           STRING "Employer: " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-APP-PARSE-EMP)
                                  DELIMITED BY SIZE INTO WS-OUTLINE
                           END-STRING
                           PERFORM PRINT-LINE
                           MOVE SPACES TO WS-OUTLINE
                           STRING "Location: " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-APP-PARSE-LOC)
                                  DELIMITED BY SIZE INTO WS-OUTLINE
                           END-STRING
                           PERFORM PRINT-LINE
                           MOVE "---" TO WS-OUTLINE
                           PERFORM PRINT-LINE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE APPS-FILE
           SET APPS-EOF-NO TO TRUE
           IF WS-APPS-COUNT-NUM = 0
               MOVE "You have not applied to any jobs yet." TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF
           MOVE "------------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           MOVE WS-APPS-COUNT-NUM TO WS-APPS-COUNT
           STRING "Total Applications: " DELIMITED BY SIZE
                  WS-APPS-COUNT DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE "------------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.

      *>*********************************************
      *> MESSAGING COPYBOOK (WEEK 8)                *
      *>*********************************************
       COPY SENDMESSAGE_SRC.
       COPY VIEWMESSAGES_SRC.


       