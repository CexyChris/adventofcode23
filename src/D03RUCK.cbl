      ******************************************************************
      * Originally written in 2022. In 2023 used as boilerplate.
      *
      *  Dec. 3rd
      *  1st Puzzle
      *
      *  Rucksack Reorg
      *
      *  input3.txt resides in USS with fmat=lf
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. D03RUCK.
        AUTHOR. ChristophBuck.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER.
           PROGRAM COLLATING SEQUENCE IS ABC.
       SPECIAL-NAMES.
           ALPHABET ABC IS "abcdefghijklmnopqrstuvwxyz",
                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Rucksack-List ASSIGN TO RUCKLST
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD Rucksack-List.
       01  Rucksack-Record              PIC X(64).

       WORKING-STORAGE SECTION.
       01  Working-Fields.
           05  MY-PGM             PIC X(8) VALUE 'D03RUCK '.
           05  File-Status        PIC 9(1) BINARY.
               88 EOF             VALUE 1
                             WHEN FALSE 0.
           05  Inspect-String     PIC X(64).
           05  Inspect-Array      REDEFINES Inspect-String.
               10 Inspect-Char    PIC X OCCURS 32 TIMES.
               10 FILLER          PIC X(32).
           05  Rec-Length         PIC 9(04) BINARY.
           05  half               PIC 9(04) BINARY.
           05  i                  PIC 9(04) BINARY.
           05  occurence          PIC 9(04) BINARY.

       01  Output-Msg             PIC X(80).
       01  Prio-Fields.
           05  TotalPrio          PIC 9(8) BINARY.
           05  TotalPrio-Disp     PIC 9(8) DISPLAY.

      /
       PROCEDURE DIVISION.

       000-Main SECTION.
      * init
           INITIALIZE Prio-Fields
           SET EOF TO FALSE

      * Read ahead
           OPEN INPUT Rucksack-List
           READ Rucksack-List NEXT RECORD
                AT END SET EOF TO TRUE
           END-READ

           PERFORM UNTIL EOF
             PERFORM 100-Sum-Prio
             READ Rucksack-List NEXT RECORD
                  AT END SET EOF TO TRUE
             END-READ
           END-PERFORM

           CLOSE Rucksack-List

           MOVE TotalPrio TO TotalPrio-Disp
           STRING "The summed up priority is "
                  TotalPrio-Disp
                  "."
             DELIMITED BY SIZE
             INTO Output-Msg
           END-STRING
           DISPLAY Output-Msg

           GOBACK
           .

       100-Sum-Prio SECTION.
           MOVE 1 TO Rec-Length
           MOVE SPACE TO Inspect-String
           MOVE ZERO to occurence

           STRING Rucksack-Record
             DELIMITED BY SPACE
             INTO INSPECT-STRING
             POINTER Rec-Length
           END-STRING
           Compute half = ( Rec-Length  / 2 ) + 1
           PERFORM VARYING i FROM 1 BY 1
             UNTIL i > half

               INSPECT Inspect-String(half : )
                 TALLYING occurence
                 FOR ALL Inspect-Char(i)

               IF occurence > 0
                 COMPUTE TotalPrio = TotalPrio +
                                     FUNCTION ORD (Inspect-Char(i))
                 EXIT PERFORM
               END-IF
           END-PERFORM
           .

      /
       END PROGRAM D03RUCK.
      /
