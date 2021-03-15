       IDENTIFICATION DIVISION.
       PROGRAM-ID.       BOSSIO-CODING-ASST.
       AUTHOR.           LUCIUS-BOSSIO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F01-INVENTORY-FILE ASSIGN TO 'codingasst.dat'
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F02-INVENTORY-REPORT ASSIGN TO 'asstreport.dat'
                                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  F01-INVENTORY-FILE
           RECORD CONTAINS 35 CHARACTERS
           DATA RECORD IS F01-INVENTORY-RECORD.
       01  F01-INVENTORY-RECORD.
           05  F01-INV-PART        PIC X(20).
           05  F01-INV-QUANTITY    PIC 9(3).
           05  F01-INV-RECEIVED    PIC 9(3).
           05  F01-INV-SHIPPED     PIC 9(3).
           05  F01-INV-PRICE       PIC 9(4)V99.

       FD  F02-INVENTORY-REPORT
           RECORD CONTAINS 83 CHARACTERS
           DATA RECORD IS F02-INVENTORY-REP-LINE.
       01  F02-INVENTORY-REP-LINE  PIC X(83).

       WORKING-STORAGE SECTION.
       01  W01-EOF-SWITCH      PIC X(2)    VALUE SPACES.

       01  W02-NAME.
           05                  PIC X(34)   VALUE SPACES.
           05                  PIC X(21)   VALUE 'LUCIUS ASSIGNMENT'. 
           05                  PIC X(28)   VALUE SPACES.
       
       01  W03-HEADING.
           05                  PIC X(23)   VALUE 'PART NAME'.
           05                  PIC X(13)   VALUE 'BEGINNING'.
           05                  PIC X(8)    VALUE 'RECD'.
           05                  PIC X(10)   VALUE 'SHIPPED'.
           05                  PIC X(10)   VALUE 'ENDING'.
           05                  PIC X(12)   VALUE 'PRICE'.
           05                  PIC X(7)    VALUE 'TOTAL'.

       01  W04-DETAIL.
           05  W04-PART        PIC X(26).
           05  W04-QUANT       PIC ZZ9.
           05  W04-RECD        PIC Z(10)9.
           05  W04-SHIP        PIC Z(8)9.
           05  W04-END         PIC Z(9)9.
           05  W04-PRICE       PIC Z(6)9.99.
           05  W04-TOTAL       PIC Z(8)9.99.
           05  W04-FLAGS       PIC XX      VALUE SPACES.
       
       01  W05-DASH.
           05                  PIC X(71)   VALUE SPACES.
           05                  PIC X(10)   VALUE '----------'.
           05                  PIC X(2)    VALUE SPACES.
       
       01  W06-GRAND-TOTAL.
           05                  PIC X(71)
                               VALUE 'TOTAL VALUE OF ALL INVENTORY'.
           05  W06-GRAND       PIC Z(3),ZZ9.99.

       01  W07-CALCULATIONS.
           05  W07-CALC-END    PIC 9(3).
           05  W07-CALC-TOTAL  PIC 9(4)V99.
           05  W07-CALC-GRAND  PIC 9(6)V99.

       PROCEDURE DIVISION.
           PERFORM 100-OPEN-FILES
           PERFORM 200-WRITE-HEADING-LINES
           READ F01-INVENTORY-FILE
               AT END MOVE 'NO' TO W01-EOF-SWITCH
           END-READ
           PERFORM 300-PROCESS-RECORDS
               UNTIL W01-EOF-SWITCH = 'NO'
           PERFORM 400-PRINT-TOTALS
           PERFORM 500-CLOSE-FILES
       STOP RUN
       .

       100-OPEN-FILES.
           OPEN    INPUT F01-INVENTORY-FILE
                   OUTPUT F02-INVENTORY-REPORT
       .

       200-WRITE-HEADING-LINES.
           MOVE W02-NAME TO F02-INVENTORY-REP-LINE
           WRITE F02-INVENTORY-REP-LINE
           MOVE SPACES TO F02-INVENTORY-REP-LINE
           WRITE F02-INVENTORY-REP-LINE
           MOVE W03-HEADING TO F02-INVENTORY-REP-LINE
           WRITE F02-INVENTORY-REP-LINE
       .

       300-PROCESS-RECORDS.
           PERFORM 310-DO-CALCULATIONS
           MOVE F01-INV-PART TO W04-PART
           MOVE F01-INV-QUANTITY TO W04-QUANT
           MOVE F01-INV-RECEIVED TO W04-RECD
           MOVE F01-INV-SHIPPED TO W04-SHIP
           MOVE W07-CALC-END TO W04-END
           MOVE F01-INV-PRICE TO W04-PRICE
           MOVE W07-CALC-TOTAL TO W04-TOTAL
           
           IF W07-CALC-TOTAL > 50000
               MOVE '**' TO W04-FLAGS
           ELSE
               IF W07-CALC-TOTAL <=50000 AND >=40000
                   MOVE '* ' TO W04-FLAGS
               END-IF
           END-IF

           MOVE W04-DETAIL TO F02-INVENTORY-REP-LINE
           WRITE F02-INVENTORY-REP-LINE

           READ F01-INVENTORY-FILE
               AT END MOVE 'NO' TO W01-EOF-SWITCH
           END-READ
       .

       310-DO-CALCULATIONS.
           COMPUTE W07-CALC-END ROUNDED =
                   F01-INV-QUANTITY + F01-INV-RECEIVED - F01-INV-SHIPPED
           COMPUTE W07-CALC-TOTAL ROUNDED =
                   W07-CALC-END * F01-INV-PRICE
           ADD W07-CALC-TOTAL TO W07-CALC-GRAND ROUNDED
       .

       400-PRINT-TOTALS.
           MOVE W05-DASH TO F02-INVENTORY-REP-LINE
           WRITE F02-INVENTORY-REP-LINE
           MOVE W07-CALC-GRAND TO W06-GRAND
           MOVE W06-GRAND-TOTAL TO F02-INVENTORY-REP-LINE
           WRITE F02-INVENTORY-REP-LINE
       .

       500-CLOSE-FILES.
           CLOSE   F01-INVENTORY-FILE
                   F02-INVENTORY-REPORT
       .