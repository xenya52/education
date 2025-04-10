       IDENTIFICATION DIVISION.
            PROGRAM-ID. COB001.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  GP-1.
           02 A1 PIC 9(2) VALUE 50.
           02 B1 PIC 9(2) VALUE 20.
           02 C1 PIC 9(2) VALUE 0.
           02 D1 PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PARA.
            DISPLAY "Value A1 = " A1.
            DISPLAY "Value B1 = " B1.
            DISPLAY "Value C1 = " C1.
            DISPLAY "Value D1 = " D1.

            ADD A1 TO B1 GIVING D1.
            DISPLAY "Value D1 after ADD A1 TO B1 = " D1.
            ADD D1 TO 0.50 GIVING D1 ROUNDED.
            DISPLAY "Value D1 after ROUNDED = " D1.

       STOP RUN.
       END PROGRAM COB001.
