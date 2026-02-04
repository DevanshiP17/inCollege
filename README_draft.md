SAMPLE READ ME FOR CODE CLARIFICATIONS

VARIABLES:

FILES:
IN-FILE - input file, organized as line sequential
OUT-FILE - output file, organized as line sequential
ACCT-FILE - accounts file, organized as line sequential (optional because file may be missing at runtime, which it will then be created as a result)


FILE DESCRIPTORS:
IN-FILE
IN-REC - input record of 256 width for reading file lines 

OUT-FILE
OUT-REC - input record of 256 width for reading file lines 


ACCT-FILE
ACCT-REC - input record of 256 width for reading file lines 

INPUT HANDLING:
WS-INLINE(256) - used for reading from input file record
WS-OUTLINE(256) - used to write to output file record
WS-PREV-PROMPT(256) - used for inline function mentioned below

Each of these flags are PIC X VALUE “N”
WS-EOF-FLAG - its constituent conditional items EOF-YES and EOF-NO are set to true based on whether or not we are at the end of file

WS-ACCT-FLAG  - same as WS-EOF-FLAG, but just for ACCT-FILE as shared usage between all three files of WS-EOF-FLAG caused errors

WS-EXIT-FLAG - functions the same as WS-EOF-FLAG, but marks when user wants to exit the menu or whole program

WS-TRIMMED(64) - trimmed input for menu input

ACCOUNT HANDLING:
WS-ACCOUNTS - group item continuing 5 WS-ACCOUNTs with each having a respective WS-USERNAME(20) and WS-PASSWORD (12)

WS-FOUND - flags yes if USERNAME-FOUND is true or no if USERNAME-NOT-FOUND

WS-PASS-FLAG (PIC X VALUE “N”) - has PASSWORD-VALID and PASSWORD-INVALID which either can be set to true if password has been found or not

These all verify if password requirements are met, each of type X with default value “N”
WS-HAS-UPPER
WS-HAS-DIGIT
WS-HAS-SPECIAL


WS-PASS-LEN (PIC 99 VALUE 0) - used for tracking password length
WS-CHAR-IDX (PIC 99 VALUE 0) - used for character looping in a string
WS-CHAR (PIC X) - used for holding a singular character for string looping

WS-I (PIC 9 VALUE 0) - used for general looping throughout program

PROFILE VARIABLES - self explanatory
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




FUNCTIONS:

MAIN:
-Just like a traditional main() in c based languages 
-executes functions for loading/creating files, loading account information, printing top level menu until user requests to exit or we reach end of file, printing end of program execution message and general clean up functions
-program execution, if all goes well, ends here

INIT-FILES
-opens input file as INPUT, tests if file was opened successfully. If not, we display error message stored in WS-IN-STAT and top program
-opening output file is the same, except we open as OUTPUT, which creates the file if it does not exist

-accounts file is a bit more complex:
	-first we open file in I-O mode for read/write
	-if open is successful we skip logic below and paragraph terminates
	-else, we create the file if it is not found or missing
	-after creating the ACCT-FILE with OUTPUT, we test to see if file was created successfully or if it was created successfully, but it’s “missing” (weird bug, but ACCT-FILE is still created)
	-if not, we stop program and print error
	-if created, we reopen the ACCT-FILE in I-O mode
	-if file still not opening successfully, we terminate program execution and print the file error
	-the last else stops program execution and prints the file flag error code if the code is neither “05” (file created, but “missing” optional file) or “35” (file not found)


CLOSE-FILES:
Just closes files

GET-NEXT-INPUT:
-reads input from infile and writes it to WS-INLINE for printing/processing reasons
-if we are at the end, we set the EOF-YES flag to true to terminate paragraphs that will test for it external to this one

PRINT-LINE:
-prints contents of WS-OUTLINE 
-writes contents to outputfile via OUT-REC
-will stop program execution if outputfile flag is not successful “00”

PRINT-INLINE:
Similar to PRINT-LINE, but used for printing lines that expect input

-it moves WS-OUTLINE to WS-PREV-PROMPT 
-then it displays the trimmed WS-OUTLINE (contents from input file) with a space without advancing so that input is on the same line, hence inline name

REQUIRE-INPUT:
Intended to be used right after PRINT-INLINE

-performs GET-NEXT-INPUT routine to get input into WS-INLINE
-tests to see if we are at the end of the file
-if so, we exit paragraph which causes cascading effect 
-if not, we print the trimmed input line from file, followed by new line
-then we call ECHO-INPUT 

ECHO-INPUT:
Echos outline contents to output file by stringing together WS-PREV-PROMPT (which would hold the prompt output) and WS-INLINE (which would hold input)

-clears WS-OUTLINE
-writes combined strings of WS-PREV-PROMPT and WS-INLINE into WS-OUTLINE
-and then writes to output file
-does file testing again and terminates program if failure is detected from outputfile flag

TOP-LEVEL-MENU:
Typical top level menu printing

Key here is that every time REQUIRE-INPUT is called, we test exit and EOF flags and exit paragraph if either flag is “Y”



…..WILL CONTINUE README LATER….
