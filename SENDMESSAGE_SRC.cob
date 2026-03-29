*>*********************************************
      *> SENDMESSAGE_SRC.cob                        *
      *> Copybook: Send Message & View Messages     *
      *> Week 8 - Basic Messaging System Part 1     *
      *>*********************************************

      *>---------------------------------------------
      *> SEND-MESSAGE
      *> Purpose: Send a private message to a
      *>          connected user.
      *> Called:  From MESSAGES-MENU option 1
      *>---------------------------------------------
       SEND-MESSAGE.
           MOVE "Enter recipient's username (must be a connection):"
               TO WS-OUTLINE
           PERFORM PRINT-INLINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(WS-INLINE) TO WS-MSG-RECIPIENT

      *>   Validate: recipient must exist as an account
           PERFORM CHECK-MSG-RECIPIENT-EXISTS
           IF MSG-RECIP-NOT-FOUND
               MOVE "User not found." TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

      *>   Validate: recipient must be an accepted connection
           PERFORM CHECK-MSG-RECIPIENT-CONNECTED
           IF MSG-NOT-CONNECTED
               MOVE "You can only message users you are connected with."
                   TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

      *>   Prompt for message content
           MOVE "Enter your message (max 200 chars):" TO WS-OUTLINE
           PERFORM PRINT-INLINE

           PERFORM REQUIRE-INPUT
           IF EXIT-YES OR EOF-YES
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(WS-INLINE)(1:200) TO WS-MSG-CONTENT

      *>   Generate Timestamp
           MOVE FUNCTION CURRENT-DATE TO WS-MSG-TIMESTAMP-RAW
           MOVE SPACES TO WS-MSG-TIMESTAMP
           STRING
               WS-MSG-TS-YEAR   DELIMITED BY SIZE
               "-"              DELIMITED BY SIZE
               WS-MSG-TS-MONTH  DELIMITED BY SIZE
               "-"              DELIMITED BY SIZE
               WS-MSG-TS-DAY    DELIMITED BY SIZE
               " "              DELIMITED BY SIZE
               WS-MSG-TS-HOUR   DELIMITED BY SIZE
               ":"              DELIMITED BY SIZE
               WS-MSG-TS-MIN    DELIMITED BY SIZE
               ":"              DELIMITED BY SIZE
               WS-MSG-TS-SEC    DELIMITED BY SIZE
               INTO WS-MSG-TIMESTAMP
           END-STRING

      *>   Persist the message
           PERFORM SAVE-MESSAGE

      *>   Confirmation
           MOVE SPACES TO WS-OUTLINE
           STRING
               "Message sent to "        DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-RECIPIENT) DELIMITED BY SIZE
               " successfully!"          DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE.

      *>---------------------------------------------
      *> CHECK-MSG-RECIPIENT-EXISTS
      *> Purpose: Verify recipient username exists
      *>          in accounts array (case-insensitive)
      *>---------------------------------------------
       CHECK-MSG-RECIPIENT-EXISTS.
           SET MSG-RECIP-NOT-FOUND TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-ACCOUNT-COUNT
                   OR MSG-RECIP-FOUND
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-USERNAME(WS-I))) =
                  FUNCTION UPPER-CASE(FUNCTION TRIM(WS-MSG-RECIPIENT))
                   SET MSG-RECIP-FOUND TO TRUE
               END-IF
           END-PERFORM.

      *>---------------------------------------------
      *> CHECK-MSG-RECIPIENT-CONNECTED
      *> Purpose: Verify recipient is an accepted
      *>          connection (case-insensitive match)
      *>---------------------------------------------
       CHECK-MSG-RECIPIENT-CONNECTED.
           SET MSG-NOT-CONNECTED TO TRUE
           PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
               UNTIL WS-CONN-IDX > WS-CONN-TOTAL
                   OR MSG-IS-CONNECTED

               IF FUNCTION UPPER-CASE(
                  FUNCTION TRIM(WS-CA-STATUS(WS-CONN-IDX)))
                  = "ACCEPTED"

                   IF (FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX))) =
                       FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-CURRENT-USERNAME))
                       AND
                       FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-CA-RECIP(WS-CONN-IDX))) =
                       FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-MSG-RECIPIENT)))
                   OR (FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-CA-SENDER(WS-CONN-IDX))) =
                       FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-MSG-RECIPIENT))
                       AND
                       FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-CA-RECIP(WS-CONN-IDX))) =
                       FUNCTION UPPER-CASE(
                       FUNCTION TRIM(WS-CURRENT-USERNAME)))
                       SET MSG-IS-CONNECTED TO TRUE
                   END-IF
               END-IF
           END-PERFORM.

      *>---------------------------------------------
      *> SAVE-MESSAGE
      *> Purpose: Append message record to messages.dat
      *> Format:  sender|recipient|timestamp|content
      *>---------------------------------------------
       SAVE-MESSAGE.
           MOVE SPACES TO WS-MSG-LINE
           STRING
               FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-RECIPIENT)    DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-TIMESTAMP)    DELIMITED BY SIZE
               "|" DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-CONTENT)      DELIMITED BY SIZE
               INTO WS-MSG-LINE
           END-STRING

           OPEN EXTEND MSGS-FILE
           IF WS-MSGS-STAT = "41"
               CLOSE MSGS-FILE
               OPEN EXTEND MSGS-FILE
           END-IF

           IF WS-MSGS-STAT = "00" OR WS-MSGS-STAT = "05"
               WRITE MSGS-REC FROM WS-MSG-LINE
               CLOSE MSGS-FILE
           ELSE
               DISPLAY "ERROR: Cannot write to messages.dat. Status="
                   WS-MSGS-STAT
           END-IF.

      *>---------------------------------------------
      *> VIEW-MY-MESSAGES
      *> Purpose: Display all messages sent to or
      *>          from the logged-in user.
      *> Called:  From MESSAGES-MENU option 2
      *>---------------------------------------------
       VIEW-MY-MESSAGES.
           MOVE 0 TO WS-MSG-COUNT-NUM
           SET MSG-VIEW-EOF-NO TO TRUE

           MOVE "--- Your Messages ---" TO WS-OUTLINE
           PERFORM PRINT-LINE

           OPEN INPUT MSGS-FILE
           IF WS-MSGS-STAT NOT = "00"
               MOVE "No messages found." TO WS-OUTLINE
               PERFORM PRINT-LINE
               MOVE "---------------------" TO WS-OUTLINE
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL MSG-VIEW-EOF-YES
               READ MSGS-FILE INTO WS-MSG-LINE
                   AT END
                       SET MSG-VIEW-EOF-YES TO TRUE
                   NOT AT END
                       PERFORM PARSE-MSG-LINE
                       IF FUNCTION UPPER-CASE(
                          FUNCTION TRIM(WS-MSG-PARSE-SENDER)) =
                          FUNCTION UPPER-CASE(
                          FUNCTION TRIM(WS-CURRENT-USERNAME))
                       OR FUNCTION UPPER-CASE(
                          FUNCTION TRIM(WS-MSG-PARSE-RECIP)) =
                          FUNCTION UPPER-CASE(
                          FUNCTION TRIM(WS-CURRENT-USERNAME))
                           ADD 1 TO WS-MSG-COUNT-NUM
                           PERFORM DISPLAY-ONE-MESSAGE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE MSGS-FILE
           SET MSG-VIEW-EOF-NO TO TRUE

           IF WS-MSG-COUNT-NUM = 0
               MOVE "You have no messages." TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF

           MOVE "---------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE
           MOVE SPACES TO WS-OUTLINE
           MOVE WS-MSG-COUNT-NUM TO WS-MSG-COUNT-DISP
           STRING "Total Messages: " DELIMITED BY SIZE
                  WS-MSG-COUNT-DISP DELIMITED BY SIZE
                  INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE
           MOVE "---------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.

      *>---------------------------------------------
      *> PARSE-MSG-LINE
      *> Purpose: Split a messages.dat record into
      *>          its four fields
      *>---------------------------------------------
       PARSE-MSG-LINE.
           MOVE SPACES TO WS-MSG-PARSE-SENDER
           MOVE SPACES TO WS-MSG-PARSE-RECIP
           MOVE SPACES TO WS-MSG-PARSE-TS
           MOVE SPACES TO WS-MSG-PARSE-BODY

           UNSTRING WS-MSG-LINE
               DELIMITED BY "|"
               INTO WS-MSG-PARSE-SENDER
                    WS-MSG-PARSE-RECIP
                    WS-MSG-PARSE-TS
                    WS-MSG-PARSE-BODY
           END-UNSTRING.

      *>---------------------------------------------
      *> DISPLAY-ONE-MESSAGE
      *> Purpose: Print a single message record
      *>---------------------------------------------
       DISPLAY-ONE-MESSAGE.
           MOVE SPACES TO WS-OUTLINE
           STRING
               "From: " DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-PARSE-SENDER) DELIMITED BY SIZE
               "  To: " DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-PARSE-RECIP) DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUTLINE
           STRING
               "Sent: " DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-PARSE-TS) DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE

           MOVE SPACES TO WS-OUTLINE
           STRING
               "  " DELIMITED BY SIZE
               FUNCTION TRIM(WS-MSG-PARSE-BODY) DELIMITED BY SIZE
               INTO WS-OUTLINE
           END-STRING
           PERFORM PRINT-LINE

           MOVE "---" TO WS-OUTLINE
           PERFORM PRINT-LINE.



           