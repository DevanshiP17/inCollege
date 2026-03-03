>>SOURCE FORMAT FREE
      *>*********************************************
      *> VIEWNET_SRC.cob - Network & Connection Mgmt
      *> Purpose: Manage pending connection requests
      *>          and display established connections
      *> Called: From AFTER-LOGIN-MENU options 4 & 5
      *>*********************************************

      *>---------------------------------------------
      *> MANAGE-PENDING-REQUESTS
      *> Loads all connection records into memory,
      *> then prompts accept/reject for each PENDING
      *> request addressed to the current user.
      *>---------------------------------------------
       MANAGE-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUTLINE
           PERFORM PRINT-LINE

           *> Load all connection records into the in-memory array
           MOVE 0 TO WS-CONN-TOTAL
           SET CONN-EOF-NO TO TRUE

           OPEN INPUT CONN-FILE

           IF WS-CONN-STAT NOT = "00"
               MOVE
                   "You have no pending connection requests at this time."
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "-----------------------------------" TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL CONN-EOF-YES
               READ CONN-FILE INTO WS-CONN-LINE
                   AT END
                       SET CONN-EOF-YES TO TRUE
                   NOT AT END
                       PERFORM PARSE-CONNECTION-LINE
                       IF WS-CONN-TOTAL < 25
                           ADD 1 TO WS-CONN-TOTAL
                           MOVE WS-CONN-SENDER-PARSE
                               TO WS-CA-SENDER(WS-CONN-TOTAL)
                           MOVE WS-CONN-RECIP-PARSE
                               TO WS-CA-RECIP(WS-CONN-TOTAL)
                           MOVE WS-CONN-STATUS-PARSE
                               TO WS-CA-STATUS(WS-CONN-TOTAL)
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CONN-FILE
           SET CONN-EOF-NO TO TRUE

           *> Count pending requests for current user
           MOVE 0 TO WS-PROFILE-COUNT
           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-TOTAL
               IF FUNCTION TRIM(WS-CA-RECIP(WS-CONN-IDX)) =
                  FUNCTION TRIM(WS-CURRENT-USERNAME)
               AND FUNCTION TRIM(WS-CA-STATUS(WS-CONN-IDX)) = "PENDING"
                   ADD 1 TO WS-PROFILE-COUNT
               END-IF
           END-PERFORM

           IF WS-PROFILE-COUNT = 0
               MOVE
                   "You have no pending connection requests at this time."
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "-----------------------------------" TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Process each pending request interactively
           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-TOTAL
                   OR EXIT-YES OR EOF-YES
               IF FUNCTION TRIM(WS-CA-RECIP(WS-CONN-IDX)) =
                  FUNCTION TRIM(WS-CURRENT-USERNAME)
               AND FUNCTION TRIM(WS-CA-STATUS(WS-CONN-IDX)) = "PENDING"
                   PERFORM PROCESS-ONE-REQUEST
               END-IF
           END-PERFORM

           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF

           *> Write updated statuses back to connections.dat
           PERFORM REWRITE-CONN-FILE

           MOVE "-----------------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.

      *>---------------------------------------------
      *> PROCESS-ONE-REQUEST
      *> Purpose: Display one pending request and
      *>          prompt the user to accept or reject.
      *> Uses:    WS-CONN-IDX (set by caller loop)
      *>---------------------------------------------
       PROCESS-ONE-REQUEST.
           MOVE SPACES TO WS-OUTLINE
           STRING
               "Request from: "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX))
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE

           MOVE "1. Accept" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE "2. Reject" TO WS-OUTLINE
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUTLINE
           STRING
               "Enter your choice for "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX))
               DELIMITED BY SIZE
               ":"
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-INLINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION TRIM(WS-INLINE) TO WS-TRIMMED

           EVALUATE WS-TRIMMED
               WHEN "1"
               WHEN "Accept"
               WHEN "accept"
                   MOVE "ACCEPTED" TO WS-CA-STATUS(WS-CONN-IDX)
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "Connection request from "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX))
                       DELIMITED BY SIZE
                       " accepted!"
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE

               WHEN "2"
               WHEN "Reject"
               WHEN "reject"
                   MOVE "REJECTED" TO WS-CA-STATUS(WS-CONN-IDX)
                   MOVE SPACES TO WS-OUTLINE
                   STRING
                       "Connection request from "
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX))
                       DELIMITED BY SIZE
                       " rejected."
                       DELIMITED BY SIZE
                       INTO WS-OUTLINE
                   END-STRING
                   PERFORM PRINT-LINE

               WHEN OTHER
                   MOVE "Invalid choice. Skipping request." TO WS-OUTLINE
                   PERFORM PRINT-LINE
           END-EVALUATE.

      *>---------------------------------------------
      *> REWRITE-CONN-FILE
      *> Purpose: Overwrite connections.dat from the
      *>          in-memory array.  REJECTED records
      *>          are dropped; PENDING and ACCEPTED
      *>          records are preserved.
      *>---------------------------------------------
       REWRITE-CONN-FILE.
           OPEN OUTPUT CONN-FILE
           IF WS-CONN-STAT NOT = "00"
               MOVE "ERROR: Cannot rewrite connections.dat."
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-TOTAL
               IF FUNCTION TRIM(WS-CA-STATUS(WS-CONN-IDX))
                      NOT = "REJECTED"
                   MOVE SPACES TO WS-CONN-LINE
                   STRING
                       FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX))
                       DELIMITED BY SIZE
                       "|"
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-CA-RECIP(WS-CONN-IDX))
                       DELIMITED BY SIZE
                       "|"
                       DELIMITED BY SIZE
                       FUNCTION TRIM(WS-CA-STATUS(WS-CONN-IDX))
                       DELIMITED BY SIZE
                       INTO WS-CONN-LINE
                   END-STRING
                   WRITE CONN-REC FROM WS-CONN-LINE
               END-IF
           END-PERFORM

           CLOSE CONN-FILE.

      *>---------------------------------------------
      *> VIEW-MY-NETWORK
      *> Purpose: Display all ACCEPTED connections
      *>          for the current user (as sender or
      *>          recipient). Cross-references profiles
      *>          to show name, university, and major.
      *>---------------------------------------------
       VIEW-MY-NETWORK.
           MOVE "--- Your Network ---" TO WS-OUTLINE
           PERFORM PRINT-LINE

           *> Load all connection records into the in-memory array
           MOVE 0 TO WS-CONN-TOTAL
           SET CONN-EOF-NO TO TRUE

           OPEN INPUT CONN-FILE

           IF WS-CONN-STAT NOT = "00"
               MOVE "You have no connections yet." TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "--------------------" TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL CONN-EOF-YES
               READ CONN-FILE INTO WS-CONN-LINE
                   AT END
                       SET CONN-EOF-YES TO TRUE
                   NOT AT END
                       PERFORM PARSE-CONNECTION-LINE
                       IF WS-CONN-TOTAL < 25
                           ADD 1 TO WS-CONN-TOTAL
                           MOVE WS-CONN-SENDER-PARSE
                               TO WS-CA-SENDER(WS-CONN-TOTAL)
                           MOVE WS-CONN-RECIP-PARSE
                               TO WS-CA-RECIP(WS-CONN-TOTAL)
                           MOVE WS-CONN-STATUS-PARSE
                               TO WS-CA-STATUS(WS-CONN-TOTAL)
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CONN-FILE
           SET CONN-EOF-NO TO TRUE

           *> Count accepted connections for current user
           MOVE 0 TO WS-PROFILE-COUNT
           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-TOTAL
               IF FUNCTION TRIM(WS-CA-STATUS(WS-CONN-IDX)) = "ACCEPTED"
                   IF FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                   OR FUNCTION TRIM(WS-CA-RECIP(WS-CONN-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                       ADD 1 TO WS-PROFILE-COUNT
                   END-IF
               END-IF
           END-PERFORM

           IF WS-PROFILE-COUNT = 0
               MOVE "You have no connections yet." TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "--------------------" TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Display each accepted connection with profile details
           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-TOTAL
               IF FUNCTION TRIM(WS-CA-STATUS(WS-CONN-IDX)) = "ACCEPTED"
                   *> Check if current user is sender - show recipient
                   IF FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                       MOVE WS-CA-RECIP(WS-CONN-IDX)
                           TO WS-NETWORK-DISPLAY-USER
                       PERFORM DISPLAY-CONNECTION-DETAILS
                   *> Check if current user is recipient - show sender
                   ELSE IF FUNCTION TRIM(WS-CA-RECIP(WS-CONN-IDX)) =
                      FUNCTION TRIM(WS-CURRENT-USERNAME)
                       MOVE WS-CA-SENDER(WS-CONN-IDX)
                           TO WS-NETWORK-DISPLAY-USER
                       PERFORM DISPLAY-CONNECTION-DETAILS
                   END-IF
                   END-IF
               END-IF
           END-PERFORM

           MOVE "--------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.

      *>---------------------------------------------
      *> DISPLAY-CONNECTION-DETAILS
      *> Purpose: Look up profile for the connected
      *>          user and display their details.
      *> Uses: WS-NETWORK-DISPLAY-USER (set by caller)
      *>---------------------------------------------
       DISPLAY-CONNECTION-DETAILS.
           *> Initialize display fields
           MOVE SPACES TO WS-NET-FNAME
           MOVE SPACES TO WS-NET-LNAME
           MOVE SPACES TO WS-NET-UNIVERSITY
           MOVE SPACES TO WS-NET-MAJOR
           SET NETWORK-PROFILE-NOT-FOUND TO TRUE

           *> Search profiles.dat for this user
           SET PROFILE-EOF-NO TO TRUE
           OPEN INPUT PROFILES-FILE

           IF PROFILES-STATUS NOT = "00"
               *> Cannot open profiles file - show username only
               MOVE SPACES TO WS-OUTLINE
               STRING
                   "Connected with: "
                   DELIMITED BY SIZE
                   FUNCTION TRIM(WS-NETWORK-DISPLAY-USER)
                   DELIMITED BY SIZE
                   INTO WS-OUTLINE
               END-STRING
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL PROFILE-EOF-YES OR NETWORK-PROFILE-FOUND
               READ PROFILES-FILE INTO WS-PROFILE-LINE
                   AT END
                       SET PROFILE-EOF-YES TO TRUE
                   NOT AT END
                       PERFORM PARSE-NETWORK-PROFILE-LINE
                       IF FUNCTION TRIM(WS-NET-PARSED-USERNAME) =
                          FUNCTION TRIM(WS-NETWORK-DISPLAY-USER)
                           SET NETWORK-PROFILE-FOUND TO TRUE
                           MOVE WS-NET-PARSED-FNAME TO WS-NET-FNAME
                           MOVE WS-NET-PARSED-LNAME TO WS-NET-LNAME
                           MOVE WS-NET-PARSED-UNIVERSITY
                               TO WS-NET-UNIVERSITY
                           MOVE WS-NET-PARSED-MAJOR TO WS-NET-MAJOR
                       END-IF
               END-READ
           END-PERFORM

           CLOSE PROFILES-FILE
           SET PROFILE-EOF-NO TO TRUE

           *> Build and display the connection line
           IF NETWORK-PROFILE-FOUND
               MOVE SPACES TO WS-OUTLINE
               STRING
                   "Connected with: "
                   DELIMITED BY SIZE
                   FUNCTION TRIM(WS-NETWORK-DISPLAY-USER)
                   DELIMITED BY SIZE
                   " (University: "
                   DELIMITED BY SIZE
                   FUNCTION TRIM(WS-NET-UNIVERSITY)
                   DELIMITED BY SIZE
                   ", Major: "
                   DELIMITED BY SIZE
                   FUNCTION TRIM(WS-NET-MAJOR)
                   DELIMITED BY SIZE
                   ")"
                   DELIMITED BY SIZE
                   INTO WS-OUTLINE
               END-STRING
               PERFORM PRINT-LINE
           ELSE
               *> Profile not found - show username with empty details
               MOVE SPACES TO WS-OUTLINE
               STRING
                   "Connected with: "
                   DELIMITED BY SIZE
                   FUNCTION TRIM(WS-NETWORK-DISPLAY-USER)
                   DELIMITED BY SIZE
                   " (University: , Major: )"
                   DELIMITED BY SIZE
                   INTO WS-OUTLINE
               END-STRING
               PERFORM PRINT-LINE
           END-IF.

      *>---------------------------------------------
      *> PARSE-NETWORK-PROFILE-LINE
      *> Purpose: Parse pipe-delimited profile data
      *>          for network display purposes.
      *> Format: username|fname|lname|university|major|...
      *>---------------------------------------------
       PARSE-NETWORK-PROFILE-LINE.
           MOVE SPACES TO WS-NET-PARSED-USERNAME
           MOVE SPACES TO WS-NET-PARSED-FNAME
           MOVE SPACES TO WS-NET-PARSED-LNAME
           MOVE SPACES TO WS-NET-PARSED-MAJOR
           MOVE SPACES TO WS-NET-PARSED-UNIVERSITY

           UNSTRING WS-PROFILE-LINE
               DELIMITED BY "|"
               INTO
                   WS-NET-PARSED-USERNAME
                   WS-NET-PARSED-FNAME
                   WS-NET-PARSED-LNAME
                   WS-NET-PARSED-UNIVERSITY
                   WS-NET-PARSED-MAJOR
           END-UNSTRING.
