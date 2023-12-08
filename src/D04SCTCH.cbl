      ******************************************************************
      * Originally written in 2022. In 2023 used as boilerplate.
      *
      *  Dec. 4th
      *  1st Puzzle
      *
      *  Scratchcards
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. D04SCTCH.
        AUTHOR. ChristophBuck.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Scratch-Cards ASSIGN TO CARDS
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD Scratch-Cards RECORDING MODE F.
      * TFW input is fixed Length  :)
       01  Game-Record.
           05 FILLER                  PIC X(5). *> "Card "
           05 Game-Nr                 PIC X(3).
           05 Double-Colon            PIC X.
           05 Winning-Numbers         PIC X(30).
           05 FILLER                  PIC X(2).
           05 My-Numbers              PIC X(75).

       WORKING-STORAGE SECTION.
       01  Working-Fields.
           05  MY-PGM             PIC X(8) VALUE 'D04SCTCH'.
           05  File-Status        PIC 9(1) BINARY.
               88 EOF             VALUE 1
                             WHEN FALSE 0.
           05  Winning-Rec.
               10  Winning-Table      PIC X(03) OCCURS 10 TIMES
                                           ASCENDING KEY Winning-Table
                                           INDEXED BY windex.
           05  My-Rec.
               10  My-Table           PIC X(03) OCCURS 25 TIMES.
           05  i                  PIC 9(02) BINARY.


       01  Output-Msg             PIC X(80).
       01  Result-Fields.
           05  Points            PIC 9(8) BINARY.
           05  Points-Total      PIC 9(8) DISPLAY.

      /
       PROCEDURE DIVISION.

       000-Main SECTION.
      * init
           INITIALIZE Result-Fields 
           INITIALIZE Output-Msg 
           SET EOF TO FALSE

      * Read ahead
           OPEN INPUT Scratch-Cards 
           READ Scratch-Cards NEXT RECORD
                AT END SET EOF TO TRUE
           END-READ

           PERFORM UNTIL EOF
             PERFORM 100-count-points
             READ Scratch-Cards NEXT RECORD
                  AT END SET EOF TO TRUE
             END-READ
           END-PERFORM

           CLOSE Scratch-Cards 

           STRING "The total points are "
                  Points-Total 
                  "."
             DELIMITED BY SIZE
             INTO Output-Msg
           END-STRING
           DISPLAY Output-Msg

           GOBACK
           .

       100-count-points SECTION.
           MOVE Winning-Numbers TO Winning-Rec 
           MOVE My-Numbers TO My-Rec
           MOVE ZERO TO Points 

           SORT Winning-Table
           SET windex TO 1.

           PERFORM VARYING i FROM 1 BY 1
             UNTIL i > 25
               SEARCH ALL Winning-Table 
                 WHEN Winning-Table(windex) = My-Table(i)
                   IF POINTS = ZERO 
                     COMPUTE Points = 1
                   ELSE 
                     COMPUTE Points = Points * 2
                   END-IF 
               END-SEARCH
           END-PERFORM

           ADD Points TO Points-Total 
           .

      /
       END PROGRAM D04SCTCH.
      /
