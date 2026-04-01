*>*********************************************
      *> SENDMESSAGE_SRC.cob                        *
      *> Copybook: Send Message                     *
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

      *>   Load connections into memory for validation
           PERFORM LOAD-CONN-FOR-MSG

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
           PERFORM PRINT-LINE
           MOVE "---------------------" TO WS-OUTLINE
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
               IF WS-MSGS-STAT NOT = "00" AND
                  WS-MSGS-STAT NOT = "05"
                   DISPLAY "ERROR: Cannot re-open messages.dat."
                       " Status=" WS-MSGS-STAT
                   EXIT PARAGRAPH
               END-IF
           END-IF

           *> File does not exist yet - close virtual open then create it
           IF WS-MSGS-STAT = "05" OR WS-MSGS-STAT = "35"
               CLOSE MSGS-FILE
               OPEN OUTPUT MSGS-FILE
           END-IF

           IF WS-MSGS-STAT = "00"
               WRITE MSGS-REC FROM WS-MSG-LINE
               IF WS-MSGS-STAT NOT = "00"
                   DISPLAY "ERROR: Write to messages.dat failed. Status="
                       WS-MSGS-STAT
                   CLOSE MSGS-FILE
                   EXIT PARAGRAPH
               END-IF
               CLOSE MSGS-FILE
           ELSE
               DISPLAY "ERROR: Cannot open messages.dat. Status="
                   WS-MSGS-STAT
               EXIT PARAGRAPH
           END-IF.

      *>---------------------------------------------
      *> LOAD-CONN-FOR-MSG
      *> Purpose: Read connections.dat into WS-CA-*
      *>          array so CHECK-MSG-RECIPIENT-CONNECTED
      *>          can validate the sender/recipient pair.
      *>---------------------------------------------
       LOAD-CONN-FOR-MSG.
           MOVE 0 TO WS-CONN-TOTAL
           SET CONN-EOF-NO TO TRUE

           OPEN INPUT CONN-FILE
           IF WS-CONN-STAT NOT = "00"
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
           SET CONN-EOF-NO TO TRUE.


           