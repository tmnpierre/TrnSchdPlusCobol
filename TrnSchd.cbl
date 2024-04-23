      ******************************************************************
      * DATE-WRITTEN. 2024-04-23                                       *
      *                                                                *
      * PURPOSE:                                                       *
      *   This program processes train data from 'train.dat',          *
      *   calculates arrival times and train types, and outputs        *
      *   results to 'train2.dat'. It handles data reading,            *
      *   processing of each record, and final output generation.      *
      ******************************************************************       
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TrnSchd.
       AUTHOR. Pierre.

      *      Define the source and destination of data
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Define the output train file and its properties
           SELECT TRAIN-FILE ASSIGN TO 'train.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
      *      Define the input train file and its properties               
           SELECT OUTPUT-FILE ASSIGN TO 'train2.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *    File description for input file
       FD TRAIN-FILE
           RECORD IS VARYING IN SIZE FROM 27 TO 37 CHARACTERS
               DEPENDING ON TRAIN-RECORD-LENGTH.
       COPY 'train-record.cpy'.
       
      *    File description for output file
       FD OUTPUT-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01 OUTPUT-RECORD PIC X(142).

       WORKING-STORAGE SECTION.
      *    Define the variables for program operatio
       01  TRAIN-RECORD-LENGTH         PIC 9(2) COMP.
       01  WS-NUMBER-OF-STOPS-LENGTH   PIC 9(2).
       01  WS-END-OF-FILE              PIC X VALUE 'N'.
           88 EOF               VALUE 'Y'.
           88 NOT-EOF           VALUE 'N'.
       01  WS-HALT-COUNTER             PIC 9(2) VALUE ZERO.
       01  WS-HALT-INDEX               PIC 9(2).
       01  WS-RECORD-TYPE              PIC X(16).
       01  WS-ARRIVAL-HOUR             PIC 99.
       01  WS-ARRIVAL-MINUTES          PIC 99.
       01  WS-TOTAL-TRAINS             PIC 9(5) VALUE ZERO.

       PROCEDURE DIVISION.

      *    Start of main processing loop
           OPEN INPUT TRAIN-FILE
           OPEN OUTPUT OUTPUT-FILE

      *    Perform reading and processing until end of file
           PERFORM UNTIL EOF
               READ TRAIN-FILE INTO TRAIN-PLANNING

      *    Check for end of file condition
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE

      *    Process each record if not end of file
                   NOT AT END
                       ADD 1 TO WS-TOTAL-TRAINS
                       MOVE 0 TO WS-HALT-COUNTER

      *    Loop to count number of stops
                       PERFORM VARYING WS-HALT-INDEX FROM 1 BY 1 UNTIL 
                                       WS-HALT-INDEX > 10
                           IF TRAIN-HALT-FLAG (WS-HALT-INDEX) = 'H'
                               ADD 1 TO WS-HALT-COUNTER
                           END-IF
                       END-PERFORM

      *    Calculate the arrival hour considering total hours
                       COMPUTE WS-ARRIVAL-HOUR = TRAIN-TIME-HH + 
                               TRAIN-NBRE-HEURES

      *    Adjust arrival hour if over 24 hours
                       PERFORM UNTIL WS-ARRIVAL-HOUR <= 23
                           SUBTRACT 24 FROM WS-ARRIVAL-HOUR
                       END-PERFORM

                       MOVE TRAIN-TIME-MM TO WS-ARRIVAL-MINUTES

      *    Determine the type of train
                       EVALUATE TRUE
                           WHEN TGV
                               MOVE "High-speed Train" TO WS-RECORD-TYPE
                           WHEN CORAIL
                               MOVE "Intercity Train" TO WS-RECORD-TYPE
                           WHEN TER
                               MOVE "Regional Train" TO WS-RECORD-TYPE
                       END-EVALUATE

      *    Build the output record string
                       STRING "Record Type: " WS-RECORD-TYPE 
                              DELIMITED BY SIZE
                              " Departure Station: " STATION-DEPART 
                              DELIMITED BY SIZE
                              " Train Time: " TRAIN-TIME-HH 
                              DELIMITED BY SIZE
                              ":" TRAIN-TIME-MM "h"
                              DELIMITED BY SIZE
                              " Total Duration: " TRAIN-NBRE-HEURES 
                              DELIMITED BY SIZE
                              " Stops Flags: " WS-HALT-COUNTER
                              DELIMITED BY SIZE
                              " Arrival Time: " WS-ARRIVAL-HOUR ":" 
                              WS-ARRIVAL-MINUTES "h"
                              DELIMITED BY SIZE
                              INTO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
                END-READ
           END-PERFORM

      *    Write total number of trains processed
           MOVE "Total Number of trains processed" TO OUTPUT-RECORD
           STRING WS-TOTAL-TRAINS DELIMITED BY SIZE INTO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           
      *    Close files after processing is complete    
           CLOSE TRAIN-FILE
           CLOSE OUTPUT-FILE.

      *    End the program  
           STOP RUN.
