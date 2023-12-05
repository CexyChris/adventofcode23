      ******************************************************************
      *  Dec. 3rd
      *  1st Puzzle
      *
      *  Gear Ratio
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. D03ENGIN.
        AUTHOR. ChristophBuck.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Engine-Schematic ASSIGN TO ENGNSCHM
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD Engine-Schematic RECORDING MODE F.
       01  Schema-Record            PIC X(140).

       WORKING-STORAGE SECTION.
       01  Working-Fields.
           05  MY-PGM             PIC X(8) VALUE 'D03ENGIN'.
           05  File-Status        PIC 9(1) BINARY.
               88 EOF             VALUE 1
                             WHEN FALSE 0.
               
           05  part-number        PIC 9(04).
           05  WS-Schema-Records.
               10  Previous-Record PIC X(140).
               10  Previous-Array   REDEFINES Previous-Record .
                   15  Previous-Char  PIC X OCCURS 140 TIMES.  
               10  Current-Record  PIC X(140).
               10  Current-Array   REDEFINES Current-Record.
                   15  Current-Char  PIC X OCCURS 140 TIMES.
               10  Next-Record     PIC X(140).
               10  Next-Array   REDEFINES Next-Record.
                   15  Next-Char  PIC X OCCURS 140 TIMES.
           05  i               PIC 9(04) BINARY.
           05  Add-switch      PIC 9 BINARY.
               88  ADD-PART-NUMBER  VALUE 1
                              WHEN FALSE  0.

       01  Output-Msg             PIC X(80).
       01  Result-Fields.
           05  Result-Total                PIC 9(08) DISPLAY.

      /
       PROCEDURE DIVISION.

       000-Main SECTION.
      * init
           INITIALIZE Result-Fields 
           INITIALIZE Output-Msg 
           SET EOF TO FALSE

      * Read ahead
           OPEN INPUT Engine-Schematic  
           READ Engine-Schematic NEXT RECORD
                AT END SET EOF TO TRUE
           END-READ

           MOVE Schema-Record TO Current-Record 
           MOVE ALL "." TO Previous-Record 

           PERFORM UNTIL EOF            
             READ Engine-Schematic  NEXT RECORD
                  AT END SET EOF TO TRUE
             END-READ
             MOVE Schema-Record TO Next-Record 
             PERFORM 100-Scan

             MOVE Current-Record TO Previous-Record 
             MOVE Next-Record    TO Current-Record 
           END-PERFORM

           CLOSE Engine-Schematic 

           MOVE ALL "." TO Next-Record 
           PERFORM 100-Scan   

           STRING "The total is "
                  Result-Total 
                  "."
             DELIMITED BY SIZE
             INTO Output-Msg
           END-STRING
           DISPLAY Output-Msg

           GOBACK
           .

       100-Scan SECTION.
           MOVE ZERO TO part-number 
      * I am not using UNSTRING because last time I did, I nearly
      * lost my mind and so did operations at zXplore. I am gonna do it 
      * byte-wise.    
           PERFORM VARYING i FROM 1 BY 1 
              UNTIL i > LENGTH OF Current-Record 
      * If I find a digit
      * 1) I add that to my part-number
             IF Current-Char(i) IS NUMERIC 
               COMPUTE part-number = part-number * 10 
                          + FUNCTION NUMVAL(Current-Char(i))
      * 2) I check for adjacent signs
               IF i > 1
                 EVALUATE FALSE ALSO FALSE 
                   WHEN Previous-Char(i - 1) IS NUMERIC 
                     ALSO Previous-Char(i - 1) EQUAL "."
                   WHEN Current-Char(i - 1) IS NUMERIC 
                     ALSO Current-Char(i - 1) EQUAL "."
                   WHEN Next-Char(i - 1) IS NUMERIC 
                     ALSO Next-Char(i - 1) EQUAL "."
                       SET ADD-PART-NUMBER TO TRUE 
                   WHEN OTHER 
                     CONTINUE 
                 END-EVALUATE
               END-IF
               EVALUATE FALSE ALSO FALSE 
                 WHEN Previous-Char(i) IS NUMERIC 
                   ALSO Previous-Char(i) EQUAL "."
                 WHEN Next-Char(i) IS NUMERIC 
                   ALSO Next-Char(i) EQUAL "."
                     SET ADD-PART-NUMBER TO TRUE 
                 WHEN OTHER 
                   CONTINUE 
               END-EVALUATE 
               IF i < LENGTH OF Current-Record 
                 EVALUATE FALSE ALSO FALSE 
                   WHEN Previous-Char(i + 1) IS NUMERIC 
                     ALSO Previous-Char(i + 1) EQUAL "."
                   WHEN Current-Char(i + 1) IS NUMERIC 
                     ALSO Current-Char(i + 1) EQUAL "."
                   WHEN Next-Char(i + 1) IS NUMERIC 
                     ALSO Next-Char(i + 1) EQUAL "."
                       SET ADD-PART-NUMBER TO TRUE 
                   WHEN OTHER 
                     CONTINUE 
                 END-EVALUATE
               END-IF
             ELSE
      * Not a digit   
               IF ADD-PART-NUMBER
                 ADD part-number TO Result-Total 
                 SET ADD-PART-NUMBER TO FALSE 
               END-IF 
               MOVE ZERO TO part-number  
             END-IF 

           END-PERFORM
      * Number could have been at the end of the line.     
           IF ADD-PART-NUMBER 
             ADD part-number TO Result-Total 
             SET ADD-PART-NUMBER TO FALSE 
           END-IF 
           .

      /
       END PROGRAM D03ENGIN.
      /
