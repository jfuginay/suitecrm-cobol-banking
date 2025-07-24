       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEGACY-CONVERTER.
       AUTHOR. SUITECRM-COBOL-TEAM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEGACY-FILE ASSIGN TO 'legacy-input.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT JSON-FILE ASSIGN TO 'json-output.json'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COPYBOOK-FILE ASSIGN TO 'copybook.cpy'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LEGACY-FILE.
       01  LEGACY-RECORD PIC X(500).
       
       FD  JSON-FILE.
       01  JSON-RECORD PIC X(1000).
       
       FD  COPYBOOK-FILE.
       01  COPYBOOK-RECORD PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-MASTER.
           05  CM-CUSTOMER-ID      PIC X(10).
           05  CM-CUSTOMER-NAME    PIC X(30).
           05  CM-ADDRESS-1        PIC X(30).
           05  CM-ADDRESS-2        PIC X(30).
           05  CM-CITY             PIC X(20).
           05  CM-STATE            PIC X(2).
           05  CM-ZIP              PIC X(10).
           05  CM-PHONE            PIC X(15).
           05  CM-EMAIL            PIC X(50).
           05  CM-BALANCE          PIC S9(9)V99 COMP-3.
           05  CM-CREDIT-LIMIT     PIC S9(9)V99 COMP-3.
           05  CM-LAST-PAYMENT     PIC 9(8).
           05  CM-STATUS           PIC X(1).
       
       01  WS-ACCOUNT-LEDGER.
           05  AL-ACCOUNT-NUMBER   PIC X(15).
           05  AL-ACCOUNT-TYPE     PIC X(2).
           05  AL-OPENING-DATE     PIC 9(8).
           05  AL-CURRENT-BALANCE  PIC S9(11)V99 COMP-3.
           05  AL-AVAILABLE-BAL    PIC S9(11)V99 COMP-3.
           05  AL-INTEREST-RATE    PIC 9(3)V9(4) COMP-3.
           05  AL-LAST-ACTIVITY    PIC 9(8).
           05  AL-BRANCH-CODE      PIC X(5).
           05  AL-CUSTOMER-ID      PIC X(10).
       
       01  WS-TRANSACTION-LOG.
           05  TL-TRANSACTION-ID   PIC X(20).
           05  TL-ACCOUNT-NUMBER   PIC X(15).
           05  TL-TRANSACTION-DATE PIC 9(8).
           05  TL-TRANSACTION-TIME PIC 9(6).
           05  TL-TRANSACTION-TYPE PIC X(3).
           05  TL-AMOUNT           PIC S9(9)V99 COMP-3.
           05  TL-BALANCE-AFTER    PIC S9(11)V99 COMP-3.
           05  TL-DESCRIPTION      PIC X(50).
           05  TL-REFERENCE        PIC X(20).
       
       01  WS-CONVERSION-TYPE      PIC X(20).
       01  WS-RECORD-COUNT         PIC 9(9) VALUE 0.
       01  WS-ERROR-COUNT          PIC 9(9) VALUE 0.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-JSON-STRING          PIC X(1000).
       01  WS-TEMP-STRING          PIC X(100).
       01  WS-FORMATTED-DATE       PIC X(10).
       01  WS-FORMATTED-AMOUNT     PIC Z,ZZZ,ZZ9.99-.
       
       01  WS-EBCDIC-TABLE.
           05  EBCDIC-CHARS        PIC X(256) VALUE 
               X'00010203372D2E2F1605250B0C0D0E0F' &
               X'101112133C3D322618193F271C1D1E1F' &
               X'405A7F7B5B6C507D4D5D5C4E6B604B61' &
               X'F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F' &
               X'7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6' &
               X'D7D8D9E2E3E4E5E6E7E8E9ADE0BD5F6D' &
               X'79818283848586878889919293949596' &
               X'979899A2A3A4A5A6A7A8A9C04FD0A107'.
       
       01  WS-ASCII-TABLE.
           05  ASCII-CHARS         PIC X(256) VALUE SPACES.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           PERFORM INITIALIZE-CONVERSION
           PERFORM READ-CONVERSION-TYPE
           PERFORM PROCESS-LEGACY-FILE
           PERFORM FINALIZE-CONVERSION
           STOP RUN.
       
       INITIALIZE-CONVERSION.
           PERFORM BUILD-ASCII-TABLE
           OPEN OUTPUT JSON-FILE
           WRITE JSON-RECORD FROM '{"records":['
           MOVE 0 TO WS-RECORD-COUNT.
       
       BUILD-ASCII-TABLE.
           MOVE ALL SPACES TO ASCII-CHARS
           MOVE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' TO ASCII-CHARS(194:26)
           MOVE 'abcdefghijklmnopqrstuvwxyz' TO ASCII-CHARS(130:26)
           MOVE '0123456789' TO ASCII-CHARS(241:10)
           MOVE ' ' TO ASCII-CHARS(65:1).
       
       READ-CONVERSION-TYPE.
           MOVE 'CUSTOMER-MASTER' TO WS-CONVERSION-TYPE.
       
       PROCESS-LEGACY-FILE.
           OPEN INPUT LEGACY-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ LEGACY-FILE INTO LEGACY-RECORD
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM CONVERT-RECORD
               END-READ
           END-PERFORM
           CLOSE LEGACY-FILE.
       
       CONVERT-RECORD.
           IF WS-RECORD-COUNT > 0
               WRITE JSON-RECORD FROM ','
           END-IF
           
           ADD 1 TO WS-RECORD-COUNT
           
           EVALUATE WS-CONVERSION-TYPE
               WHEN 'CUSTOMER-MASTER'
                   PERFORM CONVERT-CUSTOMER-MASTER
               WHEN 'ACCOUNT-LEDGER'
                   PERFORM CONVERT-ACCOUNT-LEDGER
               WHEN 'TRANSACTION-LOG'
                   PERFORM CONVERT-TRANSACTION-LOG
               WHEN OTHER
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE.
       
       CONVERT-CUSTOMER-MASTER.
           MOVE LEGACY-RECORD TO WS-CUSTOMER-MASTER
           
           PERFORM EBCDIC-TO-ASCII-CUSTOMER
           
           STRING '{'
               '"customer_id":"' FUNCTION TRIM(CM-CUSTOMER-ID) '",'
               '"name":"' FUNCTION TRIM(CM-CUSTOMER-NAME) '",'
               '"address_1":"' FUNCTION TRIM(CM-ADDRESS-1) '",'
               '"address_2":"' FUNCTION TRIM(CM-ADDRESS-2) '",'
               '"city":"' FUNCTION TRIM(CM-CITY) '",'
               '"state":"' CM-STATE '",'
               '"zip":"' FUNCTION TRIM(CM-ZIP) '",'
               '"phone":"' FUNCTION TRIM(CM-PHONE) '",'
               '"email":"' FUNCTION TRIM(CM-EMAIL) '",'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           MOVE CM-BALANCE TO WS-FORMATTED-AMOUNT
           STRING WS-JSON-STRING
               '"balance":' FUNCTION TRIM(WS-FORMATTED-AMOUNT) ','
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           MOVE CM-CREDIT-LIMIT TO WS-FORMATTED-AMOUNT
           STRING WS-JSON-STRING
               '"credit_limit":' FUNCTION TRIM(WS-FORMATTED-AMOUNT) ','
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           PERFORM FORMAT-DATE USING CM-LAST-PAYMENT
               GIVING WS-FORMATTED-DATE
           
           STRING WS-JSON-STRING
               '"last_payment":"' WS-FORMATTED-DATE '",'
               '"status":"' CM-STATUS '"'
               '}'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           WRITE JSON-RECORD FROM WS-JSON-STRING.
       
       CONVERT-ACCOUNT-LEDGER.
           MOVE LEGACY-RECORD TO WS-ACCOUNT-LEDGER
           
           PERFORM EBCDIC-TO-ASCII-ACCOUNT
           
           STRING '{'
               '"account_number":"' FUNCTION TRIM(AL-ACCOUNT-NUMBER) '",'
               '"account_type":"' AL-ACCOUNT-TYPE '",'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           PERFORM FORMAT-DATE USING AL-OPENING-DATE
               GIVING WS-FORMATTED-DATE
           STRING WS-JSON-STRING
               '"opening_date":"' WS-FORMATTED-DATE '",'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           MOVE AL-CURRENT-BALANCE TO WS-FORMATTED-AMOUNT
           STRING WS-JSON-STRING
               '"current_balance":' FUNCTION TRIM(WS-FORMATTED-AMOUNT) ','
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           MOVE AL-AVAILABLE-BAL TO WS-FORMATTED-AMOUNT
           STRING WS-JSON-STRING
               '"available_balance":' FUNCTION TRIM(WS-FORMATTED-AMOUNT) ','
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           STRING WS-JSON-STRING
               '"interest_rate":' AL-INTEREST-RATE ','
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           PERFORM FORMAT-DATE USING AL-LAST-ACTIVITY
               GIVING WS-FORMATTED-DATE
           STRING WS-JSON-STRING
               '"last_activity":"' WS-FORMATTED-DATE '",'
               '"branch_code":"' FUNCTION TRIM(AL-BRANCH-CODE) '",'
               '"customer_id":"' FUNCTION TRIM(AL-CUSTOMER-ID) '"'
               '}'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           WRITE JSON-RECORD FROM WS-JSON-STRING.
       
       CONVERT-TRANSACTION-LOG.
           MOVE LEGACY-RECORD TO WS-TRANSACTION-LOG
           
           PERFORM EBCDIC-TO-ASCII-TRANSACTION
           
           STRING '{'
               '"transaction_id":"' FUNCTION TRIM(TL-TRANSACTION-ID) '",'
               '"account_number":"' FUNCTION TRIM(TL-ACCOUNT-NUMBER) '",'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           PERFORM FORMAT-DATE USING TL-TRANSACTION-DATE
               GIVING WS-FORMATTED-DATE
           STRING WS-JSON-STRING
               '"transaction_date":"' WS-FORMATTED-DATE '",'
               '"transaction_time":"' TL-TRANSACTION-TIME '",'
               '"transaction_type":"' TL-TRANSACTION-TYPE '",'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           MOVE TL-AMOUNT TO WS-FORMATTED-AMOUNT
           STRING WS-JSON-STRING
               '"amount":' FUNCTION TRIM(WS-FORMATTED-AMOUNT) ','
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           MOVE TL-BALANCE-AFTER TO WS-FORMATTED-AMOUNT
           STRING WS-JSON-STRING
               '"balance_after":' FUNCTION TRIM(WS-FORMATTED-AMOUNT) ','
               '"description":"' FUNCTION TRIM(TL-DESCRIPTION) '",'
               '"reference":"' FUNCTION TRIM(TL-REFERENCE) '"'
               '}'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           
           WRITE JSON-RECORD FROM WS-JSON-STRING.
       
       EBCDIC-TO-ASCII-CUSTOMER.
           PERFORM CONVERT-FIELD USING CM-CUSTOMER-NAME
           PERFORM CONVERT-FIELD USING CM-ADDRESS-1
           PERFORM CONVERT-FIELD USING CM-ADDRESS-2
           PERFORM CONVERT-FIELD USING CM-CITY
           PERFORM CONVERT-FIELD USING CM-ZIP
           PERFORM CONVERT-FIELD USING CM-PHONE
           PERFORM CONVERT-FIELD USING CM-EMAIL.
       
       EBCDIC-TO-ASCII-ACCOUNT.
           PERFORM CONVERT-FIELD USING AL-ACCOUNT-NUMBER
           PERFORM CONVERT-FIELD USING AL-BRANCH-CODE
           PERFORM CONVERT-FIELD USING AL-CUSTOMER-ID.
       
       EBCDIC-TO-ASCII-TRANSACTION.
           PERFORM CONVERT-FIELD USING TL-TRANSACTION-ID
           PERFORM CONVERT-FIELD USING TL-ACCOUNT-NUMBER
           PERFORM CONVERT-FIELD USING TL-DESCRIPTION
           PERFORM CONVERT-FIELD USING TL-REFERENCE.
       
       CONVERT-FIELD.
           EXIT.
       
       FORMAT-DATE USING DATE-IN GIVING DATE-OUT.
           IF DATE-IN = 0 OR DATE-IN = 99999999
               MOVE SPACES TO DATE-OUT
           ELSE
               STRING DATE-IN(1:4) '-' DATE-IN(5:2) '-' DATE-IN(7:2)
                   DELIMITED BY SIZE INTO DATE-OUT
           END-IF.
       
       FINALIZE-CONVERSION.
           WRITE JSON-RECORD FROM '],'
           STRING '"total_records":' WS-RECORD-COUNT ','
               '"errors":' WS-ERROR-COUNT ','
               '"status":"completed"}'
               DELIMITED BY SIZE INTO WS-JSON-STRING
           WRITE JSON-RECORD FROM WS-JSON-STRING
           CLOSE JSON-FILE.