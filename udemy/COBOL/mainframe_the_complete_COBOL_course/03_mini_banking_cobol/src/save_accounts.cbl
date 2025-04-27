           DISPLAY "Enter account number to save: ".
           ACCEPT WS-ACCOUNT-NUMBER.
           DISPLAY "Enter account name to save: ".
           ACCEPT WS-ACCOUNT-NAME.

           MOVE WS-ACCOUNT-NUMBER TO ACCOUNT-NUMBER.
           MOVE WS-ACCOUNT-NAME TO ACCOUNT-NAME.
           MOVE 0 TO ACCOUNT-BALANCE.

           OPEN OUTPUT ACCOUNTS-FILE.
           WRITE ACCOUNTS-RECORD.
           CLOSE ACCOUNTS-FILE.

           DISPLAY "Account saved successfully.".
