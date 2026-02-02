       IDENTIFICATION DIVISION.
         PROGRAM-ID. PALINDROME-CHECKER.
         AUTHOR MARI.
  
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-3278.
           OBJECT-COMPUTER. IBM-3278.
    
       DATA DIVISION.
         WORKING-STORAGE SECTION.
         01 NUM       PIC 9(6).
         01 TEMP      PIC 9(6).
         01 DIGIT     PIC 9(1).
         01 RESULT    PIC 9(6).

       PROCEDURE DIVISION.
           DISPLAY "PALINDROME CHECKER PROGRAM RUNNING...".
    
           ACCEPT NUM.
           MOVE NUM TO TEMP.
    
           PERFORM UNTIL TEMP = 0
             COMPUTE DIGIT = FUNCTION MOD(TEMP,10)
             COMPUTE RESULT = (RESULT*10)+DIGIT
             COMPUTE TEMP = TEMP / 10
      
           END-PERFORM.

           IF NUM = RESULT
             DISPLAY 'ITS A PALINDROME'
           ELSE
             DISPLAY 'ITS NOT A PALINDROME'
           END-IF.
    
           STOP-RUN.
