       IDENTIFICATION DIVISION.
         PROGRAM-ID. BUBBLE-SORT.
         AUTHOR MARI.
  
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-3278.
           OBJECT-COMPUTER. IBM-3278.
    
       DATA DIVISION.
         WORKING-STORAGE SECTION.
         01 ARRAY.
           05 STR1 PIC 9(2) OCCURS 5 TIMES.
         01 ITE  PIC 9(2).
         01 ITE2 PIC 9(2) VALUE 0.
         01 TEMP PIC 9(2).
       PROCEDURE DIVISION.
           DISPLAY "BUBBLE-SORT-PROGRAM RUNNING...".
    
           PERFORM VARYING ITE FROM 1 BY 1 UNTIL ITE > 5
             ACCEPT STR1(ITE)
      
           END-PERFORM.
           PERFORM 100-SORT THRU 100-SORT-EXIT.
           DISPLAY ARRAY.
    
           STOP-RUN.

       100-SORT.
           SET ITE TO 1.
           PERFORM VARYING ITE FROM 1 BY 1 UNTIL ITE > 5
            PERFORM VARYING ITE2 FROM 1 BY 1 UNTIL ITE2 > 5
             IF STR1(ITE) < STR1(ITE2)
               MOVE STR1(ITE) TO TEMP
               MOVE STR1(ITE2) TO STR1(ITE)
               MOVE TEMP TO STR1(ITE2)
             ELSE
               CONTINUE
             END-IF
           END-PERFORM
           END-PERFORM.
  
       100-SORT-EXIT.