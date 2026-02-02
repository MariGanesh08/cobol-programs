       IDENTIFICATION DIVISION.
         PROGRAM-ID. GRADE-ALLOCATION.
         AUTHOR MARI.
  
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-3278.
           OBJECT-COMPUTER. IBM-3278.
    
       DATA DIVISION.
         WORKING-STORAGE SECTION.
         01 MARK PIC 9(3).
             88 A VALUE 90 THRU 100.
             88 B VALUE 80 THRU 90.
             88 C VALUE 70 THRU 80.
             88 D VALUE 60 THRU 70.
             88 E VALUE 50 THRU 60.
             88 F VALUE 0 THRU 50.
  
       PROCEDURE DIVISION.
           DISPLAY "GRADE-ALLOCATION-PROGRAM RUNNING...".
           ACCEPT MARK.
    
           IF A
             DISPLAY 'A'
           ELSE IF B
             DISPLAY 'B'
           ELSE IF C
             DISPLAY 'C'
           ELSE IF D
             DISPLAY 'D'
           ELSE IF E
             DISPLAY 'E'
           ELSE IF F
             DISPLAY 'F'
           ELSE
             DISPLAY 'INVALID MARK'
           END-IF.
  
           STOP-RUN.
