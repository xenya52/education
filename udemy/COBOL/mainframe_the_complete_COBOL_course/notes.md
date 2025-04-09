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
000300 CONFIGURATION SECTION.
000400 SPECIAL-NAMES.
000500     DECIMAL-POINT IS COMMA.
000600     CURRENCY SIGN IS EURO SIGN.
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
