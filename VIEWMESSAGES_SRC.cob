*>*********************************************
*> VIEWMESSAGES_SRC.cob                        *
*> Copybook: View Messages                     *
*> Week 9 - View Received Messages             *
*>*********************************************

      *>---------------------------------------------
      *> VIEW-MY-MESSAGES
      *> Purpose: Display all messages where the
      *>          logged-in user is the recipient.
      *> Called:  From MESSAGES-MENU option 2
      *> Format:  messages.dat -> sender|recip|ts|body
      *>---------------------------------------------
       VIEW-MY-MESSAGES.
           MOVE 0 TO WS-MSG-COUNT-NUM
           SET MSG-VIEW-EOF-NO TO TRUE

           MOVE "--- Your Messages ---" TO WS-OUTLINE
           PERFORM PRINT-LINE

           OPEN INPUT MSGS-FILE
           IF WS-MSGS-STAT NOT = "00"
               MOVE "You have no messages at this time." TO WS-OUTLINE
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
                       END-UNSTRING
                       IF FUNCTION UPPER-CASE(
                          FUNCTION TRIM(WS-MSG-PARSE-RECIP)) =
                          FUNCTION UPPER-CASE(
                          FUNCTION TRIM(WS-CURRENT-USERNAME))
                           ADD 1 TO WS-MSG-COUNT-NUM
                           MOVE SPACES TO WS-OUTLINE
                           STRING "From: " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-MSG-PARSE-SENDER)
                                  DELIMITED BY SIZE
                                  INTO WS-OUTLINE
                           END-STRING
                           PERFORM PRINT-LINE
                           MOVE SPACES TO WS-OUTLINE
                           STRING "Message: " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-MSG-PARSE-BODY)
                                  DELIMITED BY SIZE
                                  INTO WS-OUTLINE
                           END-STRING
                           PERFORM PRINT-LINE
                           MOVE SPACES TO WS-OUTLINE
                           STRING "Sent: " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-MSG-PARSE-TS)
                                  DELIMITED BY SIZE
                                  INTO WS-OUTLINE
                           END-STRING
                           PERFORM PRINT-LINE
                           MOVE "---" TO WS-OUTLINE
                           PERFORM PRINT-LINE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE MSGS-FILE
           SET MSG-VIEW-EOF-NO TO TRUE

           IF WS-MSG-COUNT-NUM = 0
               MOVE "You have no messages at this time." TO WS-OUTLINE
               PERFORM PRINT-LINE
           END-IF
           MOVE "---------------------" TO WS-OUTLINE
           PERFORM PRINT-LINE.
