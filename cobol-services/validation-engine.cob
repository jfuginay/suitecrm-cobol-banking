       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATION-ENGINE.
       AUTHOR. SUITECRM-COBOL-TEAM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'validation-input.json'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'validation-output.json'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD PIC X(1000).
       
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD PIC X(1000).
       
       WORKING-STORAGE SECTION.
       01  WS-VALIDATION-TYPE      PIC X(20).
       01  WS-VALUE-TO-VALIDATE    PIC X(100).
       01  WS-VALIDATION-RESULT    PIC X VALUE 'Y'.
       01  WS-ERROR-MESSAGE        PIC X(200).
       01  WS-JSON-OUTPUT          PIC X(1000).
       
       01  WS-ACCOUNT-NUMBER       PIC X(20).
       01  WS-ROUTING-NUMBER       PIC 9(9).
       01  WS-CHECK-DIGIT          PIC 9.
       01  WS-CALCULATED-DIGIT     PIC 9.
       01  WS-SSN                  PIC 9(9).
       01  WS-TAX-ID               PIC X(15).
       01  WS-IBAN                 PIC X(34).
       01  WS-SWIFT-CODE           PIC X(11).
       01  WS-CREDIT-LIMIT         PIC 9(12)V99.
       01  WS-CURRENT-BALANCE      PIC 9(12)V99.
       01  WS-INCOME               PIC 9(12)V99.
       
       01  WS-TEMP-NUM             PIC 9(15).
       01  WS-REMAINDER            PIC 9(3).
       01  WS-SUM                  PIC 9(5).
       01  WS-MULTIPLIER           PIC 9.
       01  WS-I                    PIC 9(2).
       
       01  WS-ROUTING-WEIGHTS.
           05  WS-WEIGHT           PIC 9 OCCURS 9 TIMES.
       
       01  WS-IBAN-COUNTRY-CODES.
           05  FILLER              PIC X(6) VALUE 'US28  '.
           05  FILLER              PIC X(6) VALUE 'GB22  '.
           05  FILLER              PIC X(6) VALUE 'DE22  '.
           05  FILLER              PIC X(6) VALUE 'FR27  '.
           05  FILLER              PIC X(6) VALUE 'IT27  '.
           05  FILLER              PIC X(6) VALUE 'ES24  '.
           05  FILLER              PIC X(6) VALUE 'CH21  '.
           05  FILLER              PIC X(6) VALUE 'NL18  '.
       
       01  WS-IBAN-TABLE REDEFINES WS-IBAN-COUNTRY-CODES.
           05  WS-IBAN-ENTRY OCCURS 8 TIMES.
               10  WS-COUNTRY-CODE PIC XX.
               10  WS-IBAN-LENGTH  PIC 99.
               10  FILLER          PIC XX.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           PERFORM READ-INPUT
           PERFORM VALIDATE-VALUE
           PERFORM WRITE-OUTPUT
           STOP RUN.
       
       READ-INPUT.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO INPUT-RECORD
           CLOSE INPUT-FILE
           
           PERFORM PARSE-INPUT.
       
       PARSE-INPUT.
           MOVE 'ACCOUNT-VALIDATOR' TO WS-VALIDATION-TYPE
           MOVE '123456789' TO WS-VALUE-TO-VALIDATE.
       
       VALIDATE-VALUE.
           MOVE 'Y' TO WS-VALIDATION-RESULT
           MOVE SPACES TO WS-ERROR-MESSAGE
           
           EVALUATE WS-VALIDATION-TYPE
               WHEN 'ACCOUNT-VALIDATOR'
                   PERFORM VALIDATE-ACCOUNT-NUMBER
               WHEN 'ROUTING-VALIDATOR'
                   PERFORM VALIDATE-ROUTING-NUMBER
               WHEN 'SSN-VALIDATOR'
                   PERFORM VALIDATE-SSN
               WHEN 'TAX-ID-VALIDATOR'
                   PERFORM VALIDATE-TAX-ID
               WHEN 'IBAN-VALIDATOR'
                   PERFORM VALIDATE-IBAN
               WHEN 'SWIFT-VALIDATOR'
                   PERFORM VALIDATE-SWIFT
               WHEN 'CREDIT-LIMIT'
                   PERFORM VALIDATE-CREDIT-LIMIT
               WHEN 'COMPLIANCE-CHECK'
                   PERFORM COMPLIANCE-CHECK
               WHEN OTHER
                   MOVE 'N' TO WS-VALIDATION-RESULT
                   MOVE 'Unknown validation type' TO WS-ERROR-MESSAGE
           END-EVALUATE.
       
       VALIDATE-ACCOUNT-NUMBER.
           MOVE WS-VALUE-TO-VALIDATE TO WS-ACCOUNT-NUMBER
           
           IF WS-ACCOUNT-NUMBER = SPACES
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Account number cannot be blank' TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 20
               IF WS-ACCOUNT-NUMBER(WS-I:1) NOT = SPACE
                   IF WS-ACCOUNT-NUMBER(WS-I:1) NOT NUMERIC
                      AND WS-ACCOUNT-NUMBER(WS-I:1) NOT ALPHABETIC
                       MOVE 'N' TO WS-VALIDATION-RESULT
                       MOVE 'Invalid character in account number' 
                           TO WS-ERROR-MESSAGE
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-PERFORM
           
           IF WS-ACCOUNT-NUMBER(1:2) = '00'
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Account number cannot start with 00' 
                   TO WS-ERROR-MESSAGE
           END-IF.
       
       VALIDATE-ROUTING-NUMBER.
           IF WS-VALUE-TO-VALIDATE NOT NUMERIC
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Routing number must be numeric' TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           MOVE WS-VALUE-TO-VALIDATE TO WS-ROUTING-NUMBER
           
           MOVE 3 TO WS-WEIGHT(1)
           MOVE 7 TO WS-WEIGHT(2)
           MOVE 1 TO WS-WEIGHT(3)
           MOVE 3 TO WS-WEIGHT(4)
           MOVE 7 TO WS-WEIGHT(5)
           MOVE 1 TO WS-WEIGHT(6)
           MOVE 3 TO WS-WEIGHT(7)
           MOVE 7 TO WS-WEIGHT(8)
           MOVE 1 TO WS-WEIGHT(9)
           
           MOVE 0 TO WS-SUM
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               COMPUTE WS-TEMP-NUM = 
                   FUNCTION NUMVAL(WS-ROUTING-NUMBER(WS-I:1))
               COMPUTE WS-SUM = WS-SUM + 
                   (WS-TEMP-NUM * WS-WEIGHT(WS-I))
           END-PERFORM
           
           COMPUTE WS-REMAINDER = FUNCTION MOD(WS-SUM, 10)
           
           IF WS-REMAINDER NOT = 0
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Invalid routing number checksum' 
                   TO WS-ERROR-MESSAGE
           END-IF.
       
       VALIDATE-SSN.
           IF WS-VALUE-TO-VALIDATE NOT NUMERIC
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'SSN must be numeric' TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           MOVE WS-VALUE-TO-VALIDATE TO WS-SSN
           
           IF WS-SSN(1:3) = '000' OR WS-SSN(1:3) = '666'
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Invalid SSN area number' TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-SSN(4:2) = '00'
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Invalid SSN group number' TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-SSN(6:4) = '0000'
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Invalid SSN serial number' TO WS-ERROR-MESSAGE
           END-IF.
       
       VALIDATE-TAX-ID.
           MOVE WS-VALUE-TO-VALIDATE TO WS-TAX-ID
           
           IF WS-TAX-ID(1:2) NOT NUMERIC
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Tax ID must start with 2 digits' 
                   TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-TAX-ID(3:1) NOT = '-'
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Tax ID format invalid (XX-XXXXXXX)' 
                   TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-TAX-ID(4:7) NOT NUMERIC
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Tax ID must end with 7 digits' 
                   TO WS-ERROR-MESSAGE
           END-IF.
       
       VALIDATE-IBAN.
           MOVE WS-VALUE-TO-VALIDATE TO WS-IBAN
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               IF WS-IBAN(1:2) = WS-COUNTRY-CODE(WS-I)
                   IF FUNCTION LENGTH(FUNCTION TRIM(WS-IBAN)) 
                       NOT = WS-IBAN-LENGTH(WS-I)
                       MOVE 'N' TO WS-VALIDATION-RESULT
                       STRING 'Invalid IBAN length for country ' 
                           WS-COUNTRY-CODE(WS-I)
                           DELIMITED BY SIZE INTO WS-ERROR-MESSAGE
                   END-IF
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
           IF WS-IBAN(3:2) NOT NUMERIC
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'IBAN check digits must be numeric' 
                   TO WS-ERROR-MESSAGE
           END-IF.
       
       VALIDATE-SWIFT.
           MOVE WS-VALUE-TO-VALIDATE TO WS-SWIFT-CODE
           
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-SWIFT-CODE)) NOT = 8
              AND FUNCTION LENGTH(FUNCTION TRIM(WS-SWIFT-CODE)) NOT = 11
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'SWIFT code must be 8 or 11 characters' 
                   TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-SWIFT-CODE(1:4) NOT ALPHABETIC
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'SWIFT bank code must be alphabetic' 
                   TO WS-ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-SWIFT-CODE(5:2) NOT ALPHABETIC
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'SWIFT country code must be alphabetic' 
                   TO WS-ERROR-MESSAGE
           END-IF.
       
       VALIDATE-CREDIT-LIMIT.
           MOVE FUNCTION NUMVAL(WS-VALUE-TO-VALIDATE) TO WS-CREDIT-LIMIT
           
           IF WS-INCOME = 0
               MOVE 50000 TO WS-INCOME
           END-IF
           
           IF WS-CREDIT-LIMIT > WS-INCOME * 3
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Credit limit exceeds 3x annual income' 
                   TO WS-ERROR-MESSAGE
           END-IF
           
           IF WS-CREDIT-LIMIT < 500
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Minimum credit limit is $500' 
                   TO WS-ERROR-MESSAGE
           END-IF.
       
       COMPLIANCE-CHECK.
           IF WS-VALUE-TO-VALIDATE = 'HIGH_RISK_COUNTRY'
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Compliance check failed - restricted country' 
                   TO WS-ERROR-MESSAGE
           END-IF
           
           IF WS-VALUE-TO-VALIDATE = 'SANCTIONED_ENTITY'
               MOVE 'N' TO WS-VALIDATION-RESULT
               MOVE 'Compliance check failed - sanctioned entity' 
                   TO WS-ERROR-MESSAGE
           END-IF.
       
       WRITE-OUTPUT.
           IF WS-VALIDATION-RESULT = 'Y'
               STRING '{"valid":true,"message":"Validation passed"}'
                   DELIMITED BY SIZE INTO WS-JSON-OUTPUT
           ELSE
               STRING '{"valid":false,"message":"' 
                   FUNCTION TRIM(WS-ERROR-MESSAGE) '"}'
                   DELIMITED BY SIZE INTO WS-JSON-OUTPUT
           END-IF
           
           OPEN OUTPUT OUTPUT-FILE
           WRITE OUTPUT-RECORD FROM WS-JSON-OUTPUT
           CLOSE OUTPUT-FILE.