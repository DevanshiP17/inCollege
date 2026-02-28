>>SOURCE FORMAT FREE
      *>*********************************************
      *> SENDREQ_SRC.cob - Connection Request Sending
      *> Purpose: Handle sending connection requests
      *>---------------------------------------------
       SEND-CONNECTION-REQUEST.
           *> Build the recipient username from the found profile
           MOVE WS-PARSED-USERNAME TO WS-CONN-RECIPIENT
           
           *> Validate the connection request
           PERFORM CHECK-CONNECTION-EXISTS
           
           IF WS-CONN-INVALID = "Y"
               *> Error message already displayed in CHECK-CONNECTION-EXISTS
               EXIT PARAGRAPH
           END-IF
           
           *> Connection is valid - save it
           PERFORM SAVE-CONNECTION-TO-FILE
           
           *> Display confirmation
           MOVE SPACES TO WS-OUTLINE
           STRING
               "Connection request sent to "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-PARSED-FNAME)
               DELIMITED BY SIZE
               " "
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-PARSED-LNAME)
               DELIMITED BY SIZE
               "."
               DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE.

      *>---------------------------------------------
      *> CHECK-CONNECTION-EXISTS                     
      *>---------------------------------------------
       CHECK-CONNECTION-EXISTS.
           *> Reset validation flag
           MOVE "N" TO WS-CONN-INVALID
           SET CONN-EOF-NO TO TRUE
           
           *> Open connections file for reading
           OPEN INPUT CONN-FILE
           
           IF WS-CONN-STAT = "00"
               *> File exists - check for existing connections
               PERFORM READ-AND-CHECK-CONNECTION
                   UNTIL CONN-EOF-YES OR WS-CONN-INVALID = "Y"
               
               CLOSE CONN-FILE
           ELSE
               *> File doesn't exist yet (status 05 or 35)
               *> This is the first connection - no validation needed
               *> No need to close since open failed
               CONTINUE
           END-IF
           
           *> Reset EOF flag
           SET CONN-EOF-NO TO TRUE.

      *>---------------------------------------------
      *> READ-AND-CHECK-CONNECTION                           
      *>---------------------------------------------
       READ-AND-CHECK-CONNECTION.
           READ CONN-FILE INTO WS-CONN-LINE
               AT END
                   SET CONN-EOF-YES TO TRUE
               NOT AT END
                   *> Parse the connection line
                   PERFORM PARSE-CONNECTION-LINE
                   
                   *> Check if users are already connected (ACCEPTED status)
                   IF (WS-CONN-SENDER-PARSE = WS-CURRENT-USERNAME AND
                       WS-CONN-RECIP-PARSE = WS-CONN-RECIPIENT AND
                       WS-CONN-STATUS-PARSE = "ACCEPTED")
                   OR (WS-CONN-SENDER-PARSE = WS-CONN-RECIPIENT AND
                       WS-CONN-RECIP-PARSE = WS-CURRENT-USERNAME AND
                       WS-CONN-STATUS-PARSE = "ACCEPTED")
                       MOVE "Y" TO WS-CONN-INVALID
                       MOVE "You are already connected with this user."
                           TO WS-OUTLINE
                       PERFORM PRINT-LINE
                   END-IF
                   
                   *> Check if recipient already sent a request to sender
                   IF WS-CONN-SENDER-PARSE = WS-CONN-RECIPIENT AND
                      WS-CONN-RECIP-PARSE = WS-CURRENT-USERNAME AND
                      WS-CONN-STATUS-PARSE = "PENDING"
                       MOVE "Y" TO WS-CONN-INVALID
                       MOVE "This user has already sent you a connection request."
                           TO WS-OUTLINE
                       PERFORM PRINT-LINE
                   END-IF
                   
                   *> Check if sender already sent a request to recipient
                   IF WS-CONN-SENDER-PARSE = WS-CURRENT-USERNAME AND
                      WS-CONN-RECIP-PARSE = WS-CONN-RECIPIENT AND
                      WS-CONN-STATUS-PARSE = "PENDING"
                       MOVE "Y" TO WS-CONN-INVALID
                       MOVE "You have already sent a request to this user."
                           TO WS-OUTLINE
                       PERFORM PRINT-LINE
                   END-IF
           END-READ.

      *>---------------------------------------------
      *> PARSE-CONNECTION-LINE                                  
      *>---------------------------------------------
       PARSE-CONNECTION-LINE.
           MOVE SPACES TO WS-CONN-SENDER-PARSE
           MOVE SPACES TO WS-CONN-RECIP-PARSE
           MOVE SPACES TO WS-CONN-STATUS-PARSE
           
           UNSTRING WS-CONN-LINE
               DELIMITED BY "|"
               INTO
                   WS-CONN-SENDER-PARSE
                   WS-CONN-RECIP-PARSE
                   WS-CONN-STATUS-PARSE
           END-UNSTRING.

      *>---------------------------------------------
      *> SAVE-CONNECTION-TO-FILE                     
      *> Purpose: Append new connection request      
      *> Called: After validation passes             
      *>---------------------------------------------
       SAVE-CONNECTION-TO-FILE.
           *> Build connection record first
           MOVE SPACES TO WS-CONN-LINE
           STRING
               FUNCTION TRIM(WS-CURRENT-USERNAME)
               DELIMITED BY SIZE
               "|"
               DELIMITED BY SIZE
               FUNCTION TRIM(WS-CONN-RECIPIENT)
               DELIMITED BY SIZE
               "|PENDING"
               DELIMITED BY SIZE
               INTO WS-CONN-LINE
           END-STRING
           
           *> Open file in EXTEND mode (creates if doesn't exist)
           *> Status 41 means already open - try to close and reopen
           OPEN EXTEND CONN-FILE
           
           IF WS-CONN-STAT = "41"
               *> File is already open, close it first
               CLOSE CONN-FILE
               OPEN EXTEND CONN-FILE
           END-IF
           
           IF WS-CONN-STAT = "00" OR WS-CONN-STAT = "05"
               *> Write the connection request
               WRITE CONN-REC FROM WS-CONN-LINE
               CLOSE CONN-FILE
           ELSE
               DISPLAY "ERROR: Cannot write to connections.dat. Status="
                   WS-CONN-STAT
           END-IF.

      *>---------------------------------------------
      *> LOAD-CONNECTIONS-FROM-FILE                  
      *>---------------------------------------------
       LOAD-CONNECTIONS-FROM-FILE.
           *> Use the same pattern as accounts.dat for Windows compatibility
           OPEN INPUT CONN-FILE
           
           IF WS-CONN-STAT = "00"
               *> File exists - close it
               CLOSE CONN-FILE
           ELSE
               *> File doesn't exist - create it
               IF WS-CONN-STAT = "05" OR WS-CONN-STAT = "35"
                   *> Don't close since open failed
                   OPEN OUTPUT CONN-FILE
                   IF WS-CONN-STAT = "00" OR WS-CONN-STAT = "05"
                       *> File created successfully
                       CLOSE CONN-FILE
                   END-IF
               END-IF
           END-IF.
           
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
