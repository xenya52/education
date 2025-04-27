           OPEN INPUT ACCOUNTS-FILE.
           SET EOF-LOOP-SWITCH TO 'N'.

           PERFORM UNTIL EOF-LOOP-SWITCH = 'Y'
               READ ACCOUNTS-FILE INTO ACCOUNTS-RECORD
                 AT END
                   SET EOF-LOOP-SWITCH TO 'Y'
                 NOT AT END
                   DISPLAY ACCOUNT-NUMBER ACCOUNT-NAME ACCOUNT-BALANCE
               END-READ
           END-PERFORM.

           CLOSE ACCOUNTS-FILE.
