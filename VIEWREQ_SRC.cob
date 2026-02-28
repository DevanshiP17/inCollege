 *>---------------------------------------------
      *> VIEW-PENDING-REQUESTS
      *> Purpose: Display all PENDING connection
      *>          requests where the current user is
      *>          the recipient.
      *> Called: From AFTER-LOGIN-MENU option 4
      *>---------------------------------------------
       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO WS-OUTLINE
           PERFORM PRINT-LINE

           *> Reset counter and EOF flag
           MOVE 0 TO WS-PROFILE-COUNT
           SET CONN-EOF-NO TO TRUE

           *> Open connections file for reading
           OPEN INPUT CONN-FILE

           IF WS-CONN-STAT NOT = "00"
               *> File missing - no requests exist yet
               MOVE "You have no pending connection requests at this time."
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
                       *> Only display records where:
                       *>   recipient = current user AND status = PENDING
                       IF FUNCTION TRIM(WS-CONN-RECIP-PARSE) =
                          FUNCTION TRIM(WS-CURRENT-USERNAME)
                       AND FUNCTION TRIM(WS-CONN-STATUS-PARSE) = "PENDING"
                           ADD 1 TO WS-PROFILE-COUNT
                           MOVE SPACES TO WS-OUTLINE
                           STRING
                               FUNCTION TRIM(WS-CONN-SENDER-PARSE)
                               DELIMITED BY SIZE
                               " has sent you a connection request."
                               DELIMITED BY SIZE
                               INTO WS-OUTLINE
                           END-STRING
                           PERFORM PRINT-LINE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CONN-FILE
           SET CONN-EOF-NO TO TRUE

           IF WS-PROFILE-COUNT = 0
               MOVE "You have no pending connection requests at this time."
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF

           MOVE "-----------------------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.