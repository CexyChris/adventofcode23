      ******************************************************************
      *  Dec. 2nd
      *  1st Puzzle
      *
      *  Cube Conundrum
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. D02CUBES.
        AUTHOR. ChristophBuck.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Cube-Games ASSIGN TO GAMES
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD Cube-Games RECORDING MODE F.
       01  Games-Record.
           05                       PIC X(5).
           05 Unstring-Part         PIC X(251).

       WORKING-STORAGE SECTION.
       01  Working-Fields.
           05  MY-PGM             PIC X(8) VALUE 'D02CUBES'.
           05  File-Status        PIC 9(1) BINARY.
               88 EOF             VALUE 1
                             WHEN FALSE 0.
           05  Game-ID                  PIC 9(03) DISPLAY.
           05  Cubes                    PIC X(248).
               
           05  Cube-Fields.   
               10  Cube-Number          PIC 9(2) DISPLAY.
                   88 RED-POSSIBLE      VALUES 1 THROUGH 12.
                   88 BLUE-POSSIBLE     VALUES 1 THROUGH 14.
                   88 GREEN-POSSIBLE    VALUEs 1 THROUGH 13.
               10  Cube-Color           PIC X(5).
                   88 RED               VALUE "red  ".
                   88 BLUE              VALUE "blue ".
                   88 GREEN             VALUE "green".
               10  Delim                PIC X(2).
                   88 END-OF-LINE       VALUE SPACES 
                                  WHEN  FALSE IS "XX".
               10  Unstring-Pointer     PIC 9(4) BINARY. 
               10  Possible             PIC 9(1) BINARY.
                   88 STILL-POSSIBLE    VALUE 1
                                  WHEN  FALSE 0.

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
           OPEN INPUT Cube-Games 
           READ Cube-Games NEXT RECORD
                AT END SET EOF TO TRUE
           END-READ

           PERFORM UNTIL EOF
             PERFORM 100-Play
             READ Cube-Games  NEXT RECORD
                  AT END SET EOF TO TRUE
             END-READ
           END-PERFORM

           CLOSE Cube-Games  

           STRING "The total is "
                  Result-Total 
                  "."
             DELIMITED BY SIZE
             INTO Output-Msg
           END-STRING
           DISPLAY Output-Msg

           GOBACK
           .

       100-Play SECTION.
           SET STILL-POSSIBLE TO TRUE
           SET END-OF-LINE TO FALSE
           MOVE 1 TO Unstring-Pointer 
           UNSTRING Unstring-Part  
             DELIMITED BY ": " 
             INTO Game-ID  
             WITH POINTER Unstring-Pointer 
           END-UNSTRING

           PERFORM 105-Check UNTIL END-OF-LINE 

           IF STILL-POSSIBLE
             ADD Game-ID TO Result-Total 
           END-IF
           .

       105-Check SECTION.

           UNSTRING Unstring-Part  
             DELIMITED BY SPACE
             INTO Cube-Number
             WITH POINTER Unstring-Pointer
           END-UNSTRING 

           UNSTRING Unstring-Part  
             DELIMITED BY ", " OR "; "
             INTO Cube-Color 
             DELIMITER IN Delim
             WITH POINTER Unstring-Pointer 
           END-UNSTRING

           EVALUATE TRUE ALSO FALSE 
             WHEN RED ALSO RED-POSSIBLE 
               SET STILL-POSSIBLE TO FALSE 
               SET END-OF-LINE TO TRUE 
             WHEN GREEN ALSO GREEN-POSSIBLE
               SET END-OF-LINE TO TRUE 
               SET STILL-POSSIBLE TO FALSE  
             WHEN BLUE ALSO BLUE-POSSIBLE
               SET END-OF-LINE TO TRUE 
               SET STILL-POSSIBLE TO FALSE 
             WHEN OTHER 
               CONTINUE  
           END-EVALUATE 
           .

      /
       END PROGRAM D02CUBES.
      /
