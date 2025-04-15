       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "data/accounts.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE.
       01  ACCOUNTS-RECORD.
           05  ACCOUNT-NUMBER    PIC X(10).
           05  ACCOUNT-NAME      PIC X(30).
           05  ACCOUNT-BALANCE   PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01  WS-USER-CHOICE         PIC 9.
       01  WS-TRANSFER-AMOUNT     PIC 9(7)V99.
       01  WS-ACCOUNT-NUMBER      PIC X(10).
       01  WS-ACCOUNT-NAME        PIC X(30).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Main Menu".
           DISPLAY "1. Transfer Funds".
           DISPLAY "2. Load Accounts".
           DISPLAY "3. Save Accounts".
           DISPLAY "4. Exit".
           ACCEPT WS-USER-CHOICE.
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
           END-EVALUATE.

       TRANSFER-FUNDS.
            DISPLAY "Transfer Funds procedure not implemented yet."
            PERFORM MAIN-PROCEDURE.

       LOAD-ACCOUNTS.
           DISPLAY "Load Accounts procedure not implemented yet."
           PERFORM MAIN-PROCEDURE.

       SAVE-ACCOUNTS.
           DISPLAY "Enter account number to save: ".
           ACCEPT WS-ACCOUNT-NUMBER.
           DISPLAY "Enter account name to save: ".
           ACCEPT WS-ACCOUNT-NAME.
           SAVE-ACCOUNT-RECORD.
               MOVE WS-ACCOUNT-NUMBER TO ACCOUNT-NUMBER.
               MOVE WS-ACCOUNT-NAME TO ACCOUNT-NAME.
               MOVE WS-TRANSFER-AMOUNT TO ACCOUNT-BALANCE.
               OPEN OUTPUT ACCOUNTS-FILE.
               WRITE ACCOUNTS-RECORD.
               CLOSE ACCOUNTS-FILE.
               DISPLAY "Account saved successfully.".
           PERFORM MAIN-PROCEDURE.

       EXIT-PROGRAM.
           DISPLAY "Exiting the program."
           STOP RUN.
