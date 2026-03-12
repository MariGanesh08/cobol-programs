       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTDB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE ACCTDCL END-EXEC.
       01 WS-ACCT-NO PIC X(10) VALUE "9000000002".
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT ACCOUNT_NO,CUST_NAME,ACCOUNT_TYPE,BALANCE,
                      STATUS
               INTO :DCL-ACCOUNT-NO , :DCL-CUST-NAME,
                    :DCL-ACCOUNT-TYPE, :DCL-BALANCE, :DCL-STATUS
               FROM ACCOUNT WHERE     ACCOUNT_NO = :WS-ACCT-NO
           END-EXEC.
           IF SQLCODE = 100
               DISPLAY "ACCOUNT NOT FOUND" SQLCODE
               DISPLAY WS-ACCT-NO
               STOP RUN
           END-IF.
           IF DCL-STATUS = 'I'
               DISPLAY "ACCOUNT INACTIVE" SQLCODE
               STOP RUN
           END-IF.
           DISPLAY "*****ATM BALANCE ENQUIRY******"
           DISPLAY "ACCOUNT NUMBER :" WS-ACCT-NO.
           DISPLAY "CUSTOMER NAME  :" DCL-CUST-NAME.
           DISPLAY "BALANCE        :" DCL-BALANCE.
           STOP RUN.
