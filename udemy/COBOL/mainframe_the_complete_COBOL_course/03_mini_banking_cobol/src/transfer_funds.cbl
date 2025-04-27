           DISPLAY "Enter account number where the money should go to".
           ACCEPT WS-ACCOUNT-NUMBER.
           DISPLAY "Enter the exact amount of money you want to send".
           ACCEPT WS-TRANSFER-AMOUNT.

           OPEN I-O ACCOUNTS-FILE.
           SET EOF-LOOP-SWITCH TO 'N'.

           PERFORM UNTIL EOF-LOOP-SWITCH = 'Y'
               READ ACCOUNTS-FILE INTO ACCOUNTS-RECORD
                 AT END
                   SET EOF-LOOP-SWITCH TO 'Y'
                 NOT AT END
                   IF WS-ACCOUNT-NUMBER = ACCOUNT-NUMBER
                     ADD WS-TRANSFER-AMOUNT TO ACCOUNT-BALANCE
                     REWRITE ACCOUNTS-RECORD
                   END-IF
               END-READ
           END-PERFORM.

           CLOSE ACCOUNTS-FILE.
