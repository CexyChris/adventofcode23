      ******************************************************************
      *  Dec. 1st
      *  1st Puzzle
      *
      *  Trebuchet Calibration
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. D01TRBCT.
        AUTHOR. ChristophBuck.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Calibration ASSIGN TO CALIBR
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD Calibration RECORDING MODE F.
       01  Calibration-Record     PIC X(64).

       WORKING-STORAGE SECTION.
       01  Working-Fields.
           05  MY-PGM             PIC X(8) VALUE 'D01TRBCT'.
           05  File-Status        PIC 9(1) BINARY.
               88 EOF             VALUE 1
                             WHEN FALSE 0.
           05  Inspect-String     PIC X(64).
      * A string is an immutable array of chars... 
           05  Inspect-Array      REDEFINES Inspect-String.
               10 Inspect-Char    PIC X OCCURS 64 TIMES.
           05  Rec-Length         PIC 9(04) BINARY.
           05  i                  PIC 9(04) BINARY.

       01  Output-Msg             PIC X(80).
       01  Result-Fields.
           05  Result-Calibration          PIC 9(02) DISPLAY.
           05  Result-Total                PIC 9(08) DISPLAY.

      /
       PROCEDURE DIVISION.

       000-Main SECTION.
      * init
           INITIALIZE Result-Fields 
           INITIALIZE Output-Msg 
           SET EOF TO FALSE

      * Read ahead
           OPEN INPUT Calibration 
           READ Calibration NEXT RECORD
                AT END SET EOF TO TRUE
           END-READ

           PERFORM UNTIL EOF
             DISPLAY Calibration-Record 
             PERFORM 100-Sum-Calibration
             DISPLAY Result-Calibration 
             READ Calibration  NEXT RECORD
                  AT END SET EOF TO TRUE
             END-READ
           END-PERFORM

           CLOSE Calibration 

           STRING "The total calibration is "
                  Result-Total 
                  "."
             DELIMITED BY SIZE
             INTO Output-Msg
           END-STRING
           DISPLAY Output-Msg

           GOBACK
           .

       100-Sum-Calibration SECTION.
           MOVE 1 TO Rec-Length 
           MOVE SPACE TO Inspect-String
           COMPUTE Result-Calibration = 0

           STRING Calibration-Record 
             DELIMITED BY SPACE 
             INTO Inspect-String 
             POINTER Rec-Length
           END-STRING
      * For part 2 of the puzzle 
      * we just sneak the digits into the string, carefully considering
      * overlaps e.g. "...eightwo..."
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL 
             "one" BY "o1e"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "two" BY "t2o"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "three" BY "th3ee"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "four" BY "f4ur"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "five" BY "f5ve"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "six" BY "s6x"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "seven" BY "se7en"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "eight" BY "ei8ht"
           INSPECT Inspect-String(1 : Rec-Length) REPLACING ALL
             "nine" BY "n9ne"
      *-------------------------------------------------------------    

           PERFORM 105-Find-Digit 

           DISPLAY Inspect-String(1 : Rec-Length)
      * Move first digit one to the left.
           COMPUTE Result-Calibration = Result-Calibration * 10

           MOVE FUNCTION REVERSE(Inspect-String(1 : Rec-Length - 1)) 
                TO Inspect-String

           DISPLAY Inspect-String

           PERFORM 105-Find-Digit 

           COMPUTE Result-Total = Result-Total + Result-Calibration 
           .

       105-Find-Digit SECTION.

           PERFORM VARYING i FROM 1 BY 1
             UNTIL i >= Rec-Length 
              IF Inspect-Char (i) IS NUMERIC 
                COMPUTE Result-Calibration = Result-Calibration +
                        FUNCTION NUMVAL(Inspect-Char(i))
                COMPUTE i = Rec-Length 
              END-IF
           END-PERFORM
           .

      /
       END PROGRAM D01TRBCT.
      /
