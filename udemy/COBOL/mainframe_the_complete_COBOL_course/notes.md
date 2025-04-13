# Notes

COBOL (Common Bisness Oriented Language) is a high-level programming language that was developed in the late 1950s and early 1960s for business applications. It was created by a committee of experts from various organizations, including the US Department of Defense, and was designed to be easy to read and write, with a syntax that resembles English. COBOL is known for its ability to handle large amounts of data and is still used today in many legacy systems, particularly in the banking and finance industries.

## Versions of COBOL

VS Cobol 2
COBOL / 370
COBOL for MVS and VM
COBOL for OS/390 and VM
Enterprise COBOL (**Current Version**)

## Structre of a Cobol program

### COBOL
Cobol programs are divided into **four main divisions**, each of which serves a specific purpose

### DIVISION

**IDENTIFICATION DIVISION**
Is the first division in a COBOL program and is used to provide **information about the program**, such as its name, author, and date of creation. It is also used to specify the program's environment and any special features that are being used.

```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. Test.
        AUTHOR. LUCY.
        INSTALLATION. My Company.
        DATE-WRITTEN. 2025-04-08.
        DATE-COMPILED. 2025-04-08.
  ```

**ENVIRONMENT DIVISION**
IDENTIFICATION DIVISION must follow. Is used to **specify the environment in which the program will run**, including the hardware and software configurations. It is also used to define any external files that the program will use.

configurations section:
1. Enviroment (hardware and software) related details are listed here like source computer, target computer and special names paragraph.
2. *All these are optional.*

input-output section:
1. Defines the input and output files that the program will use. and links to external defices where the program will read and write data.
2. it has a "special" syntax
``` cobol
000000 CONFIGURATION SECTION.
000000 SPECIAL-NAMES.
000000     DECIMAL-POINT IS COMMA.
000000     CURRENCY SIGN IS EURO SIGN.
```

**DATA DIVISION**
Is used to **define the data structures that the program will use**, including variables, records, and files. It is divided into several sections, including the:
1. FILE SECTION - For defining files
2. WORKING-STORAGE SECTION - For defining variables and data structures that will be used during the program's execution
3. LINKAGE SECTION - For defining data that will be passed between programs or modules
4. REPORT SECTION - For preparing reports
5. COMMUNICATION SECTION - For communicating between 2 programs running simultaneously

**PROCEDURE DIVISION**
Is used to **define the program's logic and control flow**. It contains the actual code that will be executed, including procedures, functions, and statements. aka. "main division"

### SECTION
Are the building blocks of a COBOL program and are used to group related code together. Each section has a specific purpose and is defined by a SECTION header.

### PARAGRAPH
Are smaller units of code within a section and are used to group related statements together. Each paragraph has a specific purpose and is defined by a PARAGRAPH header.

### SENTENCE
Are the smallest units of code in COBOL and are used to group related statements together. Each sentence is defined by a period (.) at the end of the last statement in the sentence and must be code in area B.

### STATEMENT
Are the individual instructions that make up a COBOL program. Each statement is defined by a specific syntax and is used to perform a specific action, such as assigning a value to a variable or performing a calculation. Its in one line.

### CLAUSE/VERB
Are the individual instructions that make up a COBOL program. Each statement is defined by a specific syntax and is used to perform a specific action, such as assigning a value to a variable or performing a calculation. Its in one line.

### CHARACTER/WORD

## Variables, Literals, Figurative constants...

### Variable basics
- Identifier to hold a value. It identifies a memory location.
- It can be of maxiumum length of 30 characters.
- Must only contain digits(0.9) and letters(A-Z) and hyphen(-).
- Cannot be a reserved word of cobol (REPLACE, INSPECT, TALLYING, etc.)
- Must not start with a digit.

### Literals
- Are constants. These are directly "hard-coded" in the program.
- Can be numeric, alphanumeric, or figurative.
Numeric literal:
  - Maximum of 18 characters
  - Valid characters:
    - 0-9
    - +, -
    - decimal point, must be used not at the end (.)
  ``` cobol
  - Example - 123
  ```
Non-numeric literal:
  - Maximum of 18 characters
  - Valid characters:
    - A-Z
    - 0-9
    - special characters like @, #, $, %, &, *, (, ), _, +, -, =, {, }, [, ], |, \, :, ;, ", ', <, >, ,, ., ?, /, ~
  ``` cobol
  - Example - "Hello World"
  ```

### Figurative constants
Are special literals that are used to represent specific values or concepts in COBOL. They are not defined by the user but are built into the language.
1. ZERO or ZEROS or ZEROES
2. SPACE or SPACES
3. HIGH-VALUE or HIGH-VALUES - Highest value of the data type
4. LOW-VALUE or LOW-VALUES - Lowest value of the data type
5. QUOTE or QUOTES - Represents a single quote character
6. ALL
7. NULL OR NULLS
**Important: Don't use HIGH-VALUE or LOW-VALUE for numeric data types**


## Data types (Picture clause), Levels...

Data Types - Denoted by PICTURE (PIC) clause. Is used to define the format and characteristics of a data item. The PICTURE clause specifies the type of data, its length, and any special formatting requirements.

**FILLER**
- Used to define a data item that is not used in the program. It is often used to reserve space in a record or to align data items within a record.

### Numeric (denoted 9)
- max length is 18
- from 0 to 9

**Sign data type (denoted S)**
- Links a sign to a number. If its present, the number is a signed number. If its not the number is a unsigned one.

**Plus Sign (denoted +)**
- Used to print "+" as sign for positive numbers.

**Minus Sign (denoted -)**
- Used to print "-" as sign for negative numbers.

**Implied Decimal point (denoted V)**
- Does not hold any memory space. This is not used for displa but rather for computation.

**Decimal point (denoted .)**
- Used for display not for calculation any arithmetic operations.

***Leading Zeros (denoted 0)**
- Used to display leading zeros with blanks. It is used for display purposes only and does not affect the actual value of the data item.

**Comma (denoted ,)**
- To insert a comma into the data item at a particular position. It is used for display purposes only and does not affect the actual value of the data item.

**Dollar symbol (denoted $)**
- To insert a dollar sign at the first position. This is normally used for currency purposes.

### Alphabet (denoted A)
- max length is 255
- from A to Z

### Alphanumeric (denoted X)
- Combination of numeric and alphabet

## Level numbers

### 01 - level - Topmost level
- This can be used for Individual data item or group item. There cannot be another 01 level within a 01 level.
- This can also used for Group data item. Group data item does not have a picture clause. It has elementary items below it.
- *Level numbers with the same numeric value are considered to be at the samel level*
- As the move down the level number must increase
- Level 01 - 49 is used for general purpose

### Special purpose level: 66, 77, 88

- 66 - Used for RENAMES clause. It should not have a picture clause
- 77 - Normally we should avoid using 77 level. It is used for Individual data item or elementry data item. It cannot be further subdivided.
- 88 - This is used for conditional processing. This must be coded under a group item. It works on the principle of true or false

## Arithmetic operations
- ADD - Adds two or more numbers together
``` COBOL
      * A
       ADD A TO B

      * B
       ADD A B GIVING C D
       C = A + B
       AND D = A + B

      * C
       ADD A TO B ROUNDED
```
- SUBTRACT - Subtracts one number from another
``` COBOL
      * A
       SUBTRACT A FROM B

      * B
       SUBTRACT A B GIVING C D
       C = A - B
       AND D = A - B
```
- MULTIPLY - Multiplies two or more numbers together
``` COBOL
      * A
       MULTIPLY A BY B
      * ...
```
- DIVIDE - Divides one number by another
``` COBOL
      * A
       DIVIDE A BY B
      * ...
```
- COMPUTE - Performs a calculation and assigns the result to a variable
``` COBOL
      * A
       COMPUTE A = B + C
      * ...
```

## Conditional processing

- IF - Used to test a condition and execute a block of code if the condition is true
``` COBOL
       PROCEDURE DIVISION.
       IF A > B THEN
          DISPLAY "A is greater than B"
       END-IF
```
- IF ELSE - Used to test a condition and execute one block of code if the condition is true and another block of code if the condition is false
``` COBOL
       PROCEDURE DIVISION.
       IF A > B THEN
          DISPLAY "A is greater than B"
       ELSE
          DISPLAY "A is less than or equal to B"
       END-IF
```
- EVALUATE - Used to test multiple conditions and execute a block of code based on the first true condition
``` COBOL
      PROCEDURE DIVISION.

      EVALUATE TRUE
         WHEN A > B
            DISPLAY "A is greater than B"
         WHEN A < B
            DISPLAY "A is less than B"
         WHEN OTHER
            DISPLAY "A is equal to B"
       END-EVALUATE
```
## Iteration and Looping - Using PERFORM

- PERFORM - Used to execute a block of code multiple times
``` COBOL
      PROCEDURE DIVISION.

      PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
         DISPLAY I
      END-PERFORM
```

## Internal Table in COBOL - Arrays

- TABLE - Used to define an array or table in COBOL
``` COBOL
      WORKING-STORAGE SECTION.

      DATA DIVISION.

      01 EMPLOYEE-TABLE.
         05 EMPLOYEE-RECORD OCCURS 10 TIMES.
            10 EMPLOYEE-NAME PIC X(20).
            10 EMPLOYEE-ID PIC 9(5).
```

## Difference  between INDEX and SUBSCRIPT

| Feature       | Index                                                                 | Subscript                                                                 |
|---------------|----------------------------------------------------------------------|--------------------------------------------------------------------------|
| Definition    | Offset or displacement from the beginning                           | Number of occurrences of the array element                               |
| Clause        | INDEXED BY clause is used here                                      | INDEXED BY clause is not used here                                       |
| Performance   | INDEX is faster as it refers to the memory location, so has better performance | SUBSCRIPT is slower as it refers to the data item, so has comparatively less performance |
| Definition Location | INDEX is not defined in WORKING-STORAGE SECTION                | SUBSCRIPT is defined in WORKING-STORAGE SECTION                          |
| Initialization | Initialized by SET operation                                       | Initialized by MOVE operation                                            |

## SEARCH and SEARCH ALL

- SEARCH - Used to lineal search for a specific value in an array or table
``` COBOL
      PROCEDURE DIVISION.

      SEARCH EMPLOYEE-TABLE
         WHEN EMPLOYEE-ID = 12345
            DISPLAY "Employee found"
         WHEN OTHER
            DISPLAY "Employee not found"
      END-SEARCH
```

- SEARCH ALL - Used to perform a binary search on a sorted array or table
``` COBOL
      PROCEDURE DIVISION.

      SEARCH ALL EMPLOYEE-TABLE
         WHEN EMPLOYEE-ID = 12345
            DISPLAY "Employee found"
         WHEN OTHER
            DISPLAY "Employee not found"
      END-SEARCH
```

## STRING

- STRING - Used to concatenate two or more strings together
``` COBOL
      PROCEDURE DIVISION.

      STRING FIRST-NAME DELIMITED BY SPACE
             LAST-NAME DELIMITED BY SPACE
             INTO FULL-NAME
      END-STRING
```

## UNSTRING

- UNSTRING - Used to split a string into multiple substrings based on a delimiter
``` COBOL
      PROCEDURE DIVISION.

      UNSTRING FULL-NAME DELIMITED BY SPACE
         INTO FIRST-NAME LAST-NAME
      END-UNSTRING
```

## INSPECT

- INSPECT - Used to count the number of occurrences of a specific character in a string
``` COBOL
      PROCEDURE DIVISION.

      INSPECT FULL-NAME TALLYING COUNT OF "A"
      END-INSPECT
```

- REPLACE - Used to replace a specific character in a string with another character
``` COBOL
      PROCEDURE DIVISION.

      INSPECT FULL-NAME REPLACING "A" BY "B"
      END-INSPECT
```

## GOTO, GOBACK, EXIT and STOP RUN

- **GOTO**: Primarily used to jump to a specific paragraph or section in the program. However, it is generally discouraged as it can make the code difficult to read and maintain.
- **EXIT PROGRAM**: Commonly used in external subroutines (CALLED programs). It returns control back to the calling program.
- **GOBACK**: When used in a CALLED program, it functions similarly to EXIT PROGRAM. When used in the main program, it acts like STOP RUN. In the CALLING program, it is preferable to use STOP RUN instead of GOBACK.
- **STOP RUN**: Terminates the execution of the program. Unlike EXIT PROGRAM, it does not return control to the calling program but instead returns control to the operating system.

## CALL

Used to call a subprogram or module from the main program. The called program can be either a COBOL program or an external program written in another language.
1. Without parsing any value it is a "Simple CALL"
2. ```CALL X USING BY REFERENCE X, X, ...``` - TO refer the same memory location for paramaeters in calling and called program. If we change the value of any parameter in the sub program, the same value will reflect in main program too.
3. ```CALL X USING BY CONTANT X, X, ...``` - To send the copy of the content of the parameters but not the momory location. Any changes to te parameters in sub program will not reflect in the main program
4. ```CALL X USING BY VALUE X, X, ...``` - To send the copy of the content of the parameters but not the momory location. Any changes to te parameters in sub program will not reflect in the main program

There are two types of CALL:
1. STATIC CALL
2. DYNAMIC CALL

## File Handling in COBOL

The File will be defined in the ENVIRONMENT DIVISION and the file structure will be defined in the DATA DIVISION. The file will be opened in the PROCEDURE DIVISION.

Access modes:
1. Sequential - The records are processed one after the other.
2. Random - The records are accessed in any order.
3. Dynamic - The records are accessed in any order and the file can be opened for both reading and writing. A combination of sequential and random access.

File types:
1. Sequential (also called "Flat file") file - A file that is read or written in a sequential manner, meaning that the records are processed one after the other.
  - The records are stored in a sequential manner one afterthe other
  - To access the Nth record, we have to read  first (N-1) records also.
  - New records are always added at the end of the file
  - It can have a fixed or variable length
  - *Recommended* to use this type of file is simple file read and write is requiered and there are less frequent search of a record is required.
```COBOL
  SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEE.DAT"
       ORGANIZATION IS SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL
       FILE STATUS IS WS-FILE-STATUS.
```
  - The file status is used to check the status of the file after each read or write operation. It is a two-character alphanumeric variable that indicates the success or failure of the operation.

2. Line Squential file - A file that is organized in a way that allows for fast access to specific records based on a key value.
  - Line squential is a special type of sequential file where each record is separated by carriage return(X"0D") or line feed(X"0A") at the end of last non-space character. Line sequential files always contain variable-length records.
  - Line sequential files are also called as text files. For Report file, we *should define the file as Line Squequential file*.

3. Indexed file - Index file which can be accessed faster. Here the acces is done using key value/s.
  - Indexed file uses alphanumeric key as "KEY".
  - We can access any record in any order using "KEY".
  - ACCESS MODE can be sequential as well as Random.
  - "KEYS" is "INDEXED" file must be unique.
  - ORGANIZATION IS INDEXED.

4. Relative Organization File - They also called as Realtive Record Data Set (RRDS) file
  - The records are stored in a relative manner.
  - The records are accessed using the relative record number (RRN).
  - The RRN is the position of the record in the file, starting from 1.
  - It can have Random *or* Sequential access.
  - We can access the record in any order by declaring a "RECORD KEY". Here the key should be a numeric key only.

## FD CLAUSE

- FD (File Description) clause is used to define the structure of a file in COBOL. It specifies the file name, organization, access mode, and other attributes of the file.

``` COBOL
       DATA DIVISION.
       FILE SECTION.
       FD FILE-NAME
       [RECORD CONTAINS N characters]
       [BLOCK CONTAINS I RECORDS]
       [DATA RECORD IS RECORD-DETAILS]
       [RECORDING MODE IS {F/V/U}]
```

[BLOCK CONTAINS I RECORDS] - Is used to define the block size of the file. It decides the size of physical record.

``` COBOL
        BLOCK CONTAINS 80 CHARACTERS
       * We can define like this for fixed length files

        BLOCK CONTAINS SIZE IN N-1 TO N-2 CHARACTERS DEPENDING ON DATA-ITEM-NAME
       * This is for dynamic files

        BLOCK CONTAINS N-1 TO N2 CHARACTERS
       * This is for variable files

        BLOCK CONTAINS 0 CHARACTERS FOR sequential(QSAM) files.
       * If we code this, the block size is determined at run time.
```

## File Operations - Legoci of File Operations

A file must be opened (OPEN) first. After the successful open of the program it, can perform READ /WRITE / REWRITE / DELETE functionality.

At end, the file should be closed (CLOSE). The file should be closed after the successful completion of the program.

### OPEN

All the files must be opened in PROCEDURE DIVISION before any other operations are performed in it.

``` COBOL
       OPEN INPUT FILE-NAME
      * File opend for reading

       OPEN OUTPUT FILE-NAME
      * File opened for writing. If the file already exists, it will be replaced with a new file.

       OPEN I-O FILE-NAME
      * File opened for both reading and writing. If the file already exists, it will be replaced with a new file.

       OPEN EXTEND FILE-NAME
      * File opened for appending. If the file already exists, new records will be added at the end of the file.

```

### READ

All the input files must be READ in PROCEDURE DDIVISON befor the records in the file are used further. At one time, only one record can be read from the file. To read the next record we have to keep on reading the file till the end of the file is reached.

```COBOL
        READ FILE-NAME [NEXT/PREVIOUS] RECORD [INTO identifier1]
        [AT END/NOT AT END {Imperative statement}]
        [END-READ]
```

### START
- If we want to position the pointer at a specific position in INDEX or RELTIVE ORGANIZTION.
- This can only be used, if file is opened in I-O mode or input mode.
- Access mode ca only be sequential or Dynamic

```COBOL
       START FILE-NAME
       [KEY IS {=/EQUAL TO/ GREATER THAN/ >/ LESS THAN/ </ NOT LESS THAN...} key-1}]
       [INVALID KEY {Imperative statement}]
       [NOT INVALID KEY {Imperative statement}]
       [END-START]
```
START will not get the value of any record but is just sets the pointer to the specific record for reading.

### WRITE

``` COBOL
       WRITE FILE-NAME [FROM identifier1/literal1]
       [Before/ AFTER ADVANCING {Identifier2/number2/PAGE} [Line/Lines]]
       [KEY IS key-1]
       [INVALID KEY {Imperative statement}]
       [NOT INVALID KEY {Imperative statement}]
       [AT END-OF-PAGE/EOP {Imperative statement}]
       [NOT AT END-OF-PAGE/EOP {Imperative statement}]
       [END-WRITE]
```

## USAGE

Usage clause is used to denote the internal representation of Data. There are two types

1. DISPLAY
   - This is the default usage. It is used for alphanumeric data.
   - It is used to display the data in a readable format.
   - It is used for all types of data except binary data.
2. Computational
    - This is used for numeric data.
    - It is used to perform arithmetic operations on the data.
    - It is used for all types of data except alphanumeric data.

**COMP** - Store as Binary format
``` Cobol
       01 A PIC 9(Z) USAGE IS COMP.
```
If 'Z' = 1 to 4, it occupies 2 bytes
If 'Z' = 5 to 9, it occupies 4 bytes
If 'Z' = 10 to 18, it occupies 8 bytes

**COMP-1** - Store as HEXADECIMAL FORM. It is single precision Floating point and it must not have a "PIC" clause.
``` Cobol
       01 N USAGE IS COMP-1.
```
- It is internal floating point and always occupies 4 byte in memory.

**COMP-2** - Stored as HEXADECIMAL FORM. It is double precision Floating point and it must not have a "PIC" clause.
``` Cobol
       01 N USAGE IS COMP-2.
```
- It is internal floating point and always occupies 8 byte in memory.

**COMP-3** - Store as packed decimal format. It is double precision floating point Each digit occupies 1/2 byte. 1 Byte is equal to 2 Nibble so 1/2 byte is 1 nibble. If sign is present, it is stored in the right most Nibble
``` Cobol
       01 A PIC 9(Z) USAGE IS COMP-3.
```
If Z is even number then, nzmber of bytes allocated = (Z/2) + 1
If Z is odd number then, nzmber of bytes allocated = (Z/2) + (1/2)

## STATEMENT examples with explanation

```MOVE "HELLO WORLD" TO WS-HELLO``` - *MOVES* the r-value string "HELLO WORLD" *to* the value of WS-HELLO
```MOVE 123 TO WS-NUMBER``` - *MOVES* the r-value number 123 *to* the value of WS-NUMBER
```MOVE WS-NUMBER TO WS-HELLO``` - *MOVES* the value of WS-NUMBER *to* the value of WS-HELLO
```MOVE CORRESPONDING GROUP-X TO GROUP-Y``` - *MOVES* the *corresponding* values from GROUP-X *to* GROUP-Y
```MOVE CORRESPONDING GROUP-X TO GROUP-Y USING KEY IS KEY-NAME``` - *MOVES* the *corresponding* values from GROUP-X *to* GROUP-Y *using* KEY-NAME
```MOVE 12 TO A OF GP-1``` - *MOVES* the value 12 *to* the value A *of* GP-1
```MOVE WS-HELLO(7, 5) TO WS-REF-MOD``` - If WS-HELLO has the value "HELLO WORLD" and the WS-REF-MOD exists too. So you will *move* the value of WS-HELLO starting from *7th* (index 0) position and *5* (index 1) characters long to the value of WS-REF-MOD. So the value of WS-REF-MOD will be "WORLD"
```MOVE WS-HELLO(7:5) TO WS-REF-MOD``` - If WS-HELLO has the value "HELLO WORLD" and the WS-REF-MOD exists too. So you will *move* the value of WS-HELLO starting from *7th* (index 0) position and *5* (index 1) characters long to the value of WS-REF-MOD. So the value of WS-REF-MOD will be "WORLD"
```MOVE WS-HELLO(7:5) TO WS-REF-MOD(2:3)``` - If WS-HELLO has the value "HELLO WORLD" and the WS-REF-MOD exists too. So you will *move* the value of WS-HELLO starting from *7th* (index 0) position and *5* (index 1) characters long to the value of WS-REF-MOD starting from *2nd* (index 0) position and *3* (index 1) characters long. So the value of WS-REF-MOD will be "WOR"
```DISPLAY "HELLO WORLD"``` - *Displays* the string "HELLO WORLD" as output
```DISPLAY WS-HELLO WITH NO ADVANCING``` - *Displays* the value of WS-HELLO as output *without moving to the next line*
```DISPLAY WS-HELLO WITH NO ADVANCING AT END OF LINE``` - *Displays* the value of WS-HELLO as output *without moving to the next line* and *at the end of the line*
```DISPLAY WS-HELLO WITH NO ADVANCING AT END OF PAGE``` - *Displays* the value of WS-HELLO as output *without moving to the next line* and *at the end of the page*
```DISPLAY WS-HELLO WITH NO ADVANCING AT END OF PAGE AND AT END OF LINE``` - *Displays* the value of WS-HELLO as output *without moving to the next line* and *at the end of the page* and *at the end of the line*
```ACCEPT EMPLOYEE-DET``` - *Accepts* the value of EMPLOYEE-DET as input
```ACCEPT EMPLOYEE-DET FROM DATE``` - *Accepts* the value of EMPLOYEE-DET as input *from* the date
```ACCEPT EMPLOYEE-DET FROM TIME``` - *Accepts* the value of EMPLOYEE-DET as input *from* the time
```//SYSIN DD *``` - *System in in-Stream data*
```//SYSOUT DD SYSOUT=*``` - *System out in-Stream data*
```//SYSOUT DD SYSOUT=H``` - *System out in-Stream data* with header
```//SYSOUT DD SYSOUT=E``` - *System out in-Stream data* with error
```//SYSOUT DD SYSOUT=F``` - *System out in-Stream data* with footer
```//SYSOUT DD SYSOUT=N``` - *System out in-Stream data* with no header and no footer
```//SYSOUT DD SYSOUT=R``` - *System out in-Stream data* with report
```//SYSOUT DD SYSOUT=T``` - *System out in-Stream data* with trailer
