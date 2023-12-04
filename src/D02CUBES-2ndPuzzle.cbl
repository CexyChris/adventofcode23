      ******************************************************************
      *  Dec. 2nd
      *  2nd Puzzle
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
               10 Cube-Number  PIC 9(2).
               10 Min-Red      PIC 9(2) BINARY.
               10 Min-Blue     PIC 9(2) BINARY.
               10 Min-Green    PIC 9(2) BINARY.
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
           05  Result-Power                PIC 9(08) BINARY.
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
      *     SET STILL-POSSIBLE TO TRUE
           SET END-OF-LINE TO FALSE
           MOVE ZERO TO Min-Red 
           MOVE ZERO TO Min-Green 
           MOVE ZERO TO Min-Blue 
           MOVE 1 TO Unstring-Pointer 

           UNSTRING Unstring-Part  
             DELIMITED BY ": " 
             INTO Game-ID  
             WITH POINTER Unstring-Pointer 
           END-UNSTRING

           PERFORM 105-Check UNTIL END-OF-LINE 

           COMPUTE Result-Power = Min-Red * Min-Green * Min-Blue 
           ADD Result-Power TO Result-Total 
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

           EVALUATE TRUE ALSO TRUE 
             WHEN RED ALSO Cube-Number > Min-Red 
               COMPUTE Min-Red = Cube-Number 
             WHEN GREEN ALSO Cube-Number > Min-Green 
               COMPUTE Min-Green = Cube-Number 
             WHEN BLUE ALSO Cube-Number > Min-Blue 
               COMPUTE Min-Blue = Cube-Number 
             WHEN OTHER 
               CONTINUE  
           END-EVALUATE 
           .

      /
       END PROGRAM D02CUBES.
      /
