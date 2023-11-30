      ******************************************************************
      * Originally written in 2022. In 2023 used as boilerplate.
      *
      *  Dec. 6th
      *  1st Puzzle
      *
      *  Tuning Trouble
      *
      *  input3.txt resides in USS with fmat=lf
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. D06TUNE.
        AUTHOR. ChristophBuck.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT datastream ASSIGN TO DATABUF
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD datastream.
       01  data-buffer                  PIC X(4096).

       WORKING-STORAGE SECTION.
       01  Working-Fields.
           05  MY-PGM             PIC X(8) VALUE 'D06TUNE '.
           05  File-Status        PIC 9(1) BINARY.
               88 EOF             VALUE 1
                             WHEN FALSE 0.
           05  msg                PIC X(2048).

       01  Output-Msg             PIC X(80).
       01  Counter-Fields.
           05  Counter            PIC 9(8) BINARY.
           05  Counter-Disp       PIC 9(8) DISPLAY.

       LINKAGE SECTION.
       01  four-bytes.
           05  Byte-one           PIC X.
           05  Byte-two           PIC X.
           05  Byte-three         PIC X.
           05  Byte-four          PIC X.
      /
       PROCEDURE DIVISION.

       000-Main SECTION.
      * init
           INITIALIZE Counter-Fields
           SET EOF TO FALSE

      * Read ahead
           OPEN INPUT datastream
           READ datastream NEXT RECORD
                AT END SET EOF TO TRUE
           END-READ


      * Should only be one iteration in this case!
           PERFORM UNTIL EOF
             PERFORM 100-check-marker
             READ datastream NEXT RECORD
                  AT END SET EOF TO TRUE
             END-READ
           END-PERFORM

           CLOSE datastream

           STRING "The first marker ends at "
                  Counter-Disp
                  "."
             DELIMITED BY SIZE
             INTO Output-Msg
           END-STRING
           DISPLAY Output-Msg

           GOBACK
           .

       100-check-marker SECTION.
           STRING data-buffer
             DELIMITED BY SPACE
             INTO msg
           END-STRING

           SET ADDRESS OF four-bytes TO ADDRESS OF msg

           PERFORM UNTIL EOF

             EVALUATE TRUE
               WHEN byte-one EQUAL Byte-two
               WHEN byte-one EQUAL byte-three
               WHEN byte-one EQUAL byte-four
                 SET ADDRESS OF Four-Bytes TO ADDRESS OF byte-two
                 ADD 1 To Counter
               WHEN byte-two EQUAL byte-three
               WHEN byte-two EQUAL byte-four
                 SET ADDRESS OF four-bytes to ADDRESS of byte-three
                 ADD 2 To Counter
               WHEN byte-three EQUAL byte-four
                 SET ADDRESS of four-bytes TO ADDRESS of byte-four
                 ADD 3 To Counter
               WHEN OTHER
                 ADD 4 to Counter
                 MOVE counter TO counter-disp
                 SET EOF TO TRUE
             END-EVALUATE
           END-PERFORM
           .

      /
       END PROGRAM D06TUNE.
      /
