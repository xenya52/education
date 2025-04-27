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
       01  EOF-LOOP-SWITCH        PIC X VALUE 'N'.

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

       TRANSFER-FUNDS SECTION.
           COPY "src/transfer_funds.cbl".

       LOAD-ACCOUNTS SECTION.
           COPY "src/load_accounts.cbl".

       SAVE-ACCOUNTS SECTION.
           COPY "src/save_accounts.cbl".

       EXIT-PROGRAM SECTION.
           COPY "src/exit_pr.cbl".
