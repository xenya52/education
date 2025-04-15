       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USER-CHOICE PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-MENU
           PERFORM GET-USER-CHOICE
           EVALUATE WS-USER-CHOICE
               WHEN 1
                   PERFORM TRANSFER-FUNDS
               WHEN 2
                   PERFORM LOAD-ACCOUNTS
               WHEN 3
                   PERFORM SAVE-ACCOUNTS
               WHEN 4
                   PERFORM EXIT-PROGRAM
               WHEN OTHER
                   DISPLAY "Invalid choice, please try again."
                   PERFORM MAIN-PROCEDURE
           END-EVALUATE
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "Main Menu"
           DISPLAY "1. Transfer Funds"
           DISPLAY "2. Load Accounts"
           DISPLAY "3. Save Accounts"
           DISPLAY "4. Exit"
           .

       GET-USER-CHOICE.
           ACCEPT WS-USER-CHOICE
           .

       TRANSFER-FUNDS.
           CALL 'TRANSFER' USING ...
           .

       LOAD-ACCOUNTS.
           CALL 'FILEHANDLER' USING 'LOAD'
           .

       SAVE-ACCOUNTS.
           CALL 'FILEHANDLER' USING 'SAVE'
           .

       EXIT-PROGRAM.
           DISPLAY "Exiting program..."
           STOP RUN
           .
