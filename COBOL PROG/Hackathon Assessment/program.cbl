 IDENTIFICATION DIVISION.
       PROGRAM-ID. PROBLE1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTFILE ASSIGN TO ACCTKSDS
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ACCT-NO
           FILE STATUS IS ACCT-FS.

           SELECT TRAN-FILE ASSIGN TO TRANFILE
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD ACCTFILE.
       01 ACCT-REC.
          05 ACCT-NO PIC X(10).
          05 CUST-NAME PIC X(30).
          05 ACCT-TYPE PIC X(1).
          05 BALANCE PIC 9(7)V99.
          05 WS-STATUS PIC X(1).
       FD TRAN-FILE.
       01 TRAN-REC.
          05 TR-ACCT-NO PIC X(10).
          05 TR-AMOUNT PIC 9(7)V99.
       WORKING-STORAGE SECTION.
       01 ACCT-FS PIC XX.
       01 TRAN-FS PIC XX.
       01 WS-TOTAL-TRANS PIC 9(5) VALUE 0.
       01 WS-SUCCESS PIC 9(5) VALUE 0.
       01 WS-FAILED PIC 9(5) VALUE 0.
       01 WS-TOTAL-AMT PIC 9(9)V99 VALUE 0.
       01 WS-MAX-WITHDRAW PIC 9(7)V99 VALUE 20000.
       01 WS-MIN-BAL PIC 9(7)V99 VALUE 1000.
       01 WS-NEW-BAL PIC 9(7)99.

       PROCEDURE DIVISION.
           OPEN I-O ACCTFILE
                INPUT TRAN-FILE.
           IF ACCT-FS NOT = "00"
               DISPLAY "ERROR IN VSAM FILE OPEN"
               STOP RUN
           END-IF.
           PERFORM UNTIL TRAN-FS = "10"
               READ TRAN-FILE
                   AT END
                       MOVE "10" TO TRAN-FS
                   NOT AT END
                       PERFORM PROCESS-TRANSACTION
               END-READ
           END-PERFORM.
           PERFORM DISPLAY-SUMMARY.
           CLOSE ACCTFILE TRAN-FILE.
           STOP RUN.
       PROCESS-TRANSACTION.
           ADD 1 TO WS-TOTAL-TRANS
           MOVE TR-ACCT-NO TO ACCT-NO
           READ ACCTFILE KEY IS ACCT-NO
               INVALID KEY
                   DISPLAY "ACCOUNT NOT FOUND:" TR-ACCT-NO
                   ADD 1 TO WS-FAILED
                   EXIT PARAGRAPH
           END-READ
           IF WS-STATUS = "I"
               DISPLAY "INACTIVE ACCOUNT :" TR-ACCT-NO
               ADD 1 TO WS-FAILED
               EXIT PARAGRAPH
           END-IF
           IF TR-AMOUNT > WS-MAX-WITHDRAW
               DISPLAY "LIMIT EXCEED:" TR-ACCT-NO
               ADD 1 TO WS-FAILED
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-NEW-BAL = BALANCE - TR-AMOUNT
           IF WS-NEW-BAL < WS-MIN-BAL
               DISPLAY "MIN BALANCE VIOLATED:" TR-ACCT-NO
               ADD 1 TO WS-FAILED
               EXIT PARAGRAPH
           END-IF
           MOVE WS-NEW-BAL TO BALANCE
           REWRITE ACCT-REC
               INVALID KEY
                   DISPLAY "REWRITE FAILED:" TR-ACCT-NO
                   ADD 1 TO WS-FAILED
                   EXIT PARAGRAPH
           END-REWRITE
           ADD 1 TO WS-SUCCESS
           ADD TR-AMOUNT TO WS-TOTAL-AMT
           DISPLAY "WITHDRAW SUCCESS:" TR-ACCT-NO.
       DISPLAY-SUMMARY.
           DISPLAY "TOTAL TRANSACTION:" WS-TOTAL-TRANS
           DISPLAY "SUCCESSFUL WITHDRAW:" WS-SUCCESS
           DISPLAY "FAILED WITHDRAW :" WS-FAILED
           DISPLAY "TOTAL AMOUNT TAKEN:" WS-TOTAL-AMT.
