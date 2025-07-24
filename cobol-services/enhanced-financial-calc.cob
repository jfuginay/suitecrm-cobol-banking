       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENHANCED-FINANCIAL-CALC.
       AUTHOR. SUITECRM-COBOL-TEAM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.json'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'output.json'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD PIC X(1000).
       
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD PIC X(1000).
       
       WORKING-STORAGE SECTION.
       01  WS-CALCULATION-TYPE     PIC X(20).
       01  WS-PRINCIPAL            PIC 9(12)V99 COMP-3.
       01  WS-RATE                 PIC 9(3)V9(6) COMP-3.
       01  WS-TERM                 PIC 9(5) COMP-3.
       01  WS-TERM-YEARS           PIC 9(3) COMP-3.
       01  WS-FREQUENCY            PIC X(20).
       01  WS-COMPOUND-FREQ        PIC X(20).
       
       01  WS-MONTHLY-PAYMENT      PIC 9(12)V99 COMP-3.
       01  WS-TOTAL-INTEREST       PIC 9(12)V99 COMP-3.
       01  WS-TOTAL-PAYMENT        PIC 9(12)V99 COMP-3.
       01  WS-MONTHLY-RATE         PIC 9(3)V9(9) COMP-3.
       01  WS-NUM-PAYMENTS         PIC 9(5) COMP-3.
       
       01  WS-COMPOUND-AMOUNT      PIC 9(12)V99 COMP-3.
       01  WS-COMPOUND-PERIODS     PIC 9(5) COMP-3.
       01  WS-EFFECTIVE-RATE       PIC 9(3)V9(6) COMP-3.
       
       01  WS-TEMP-CALC            PIC 9(15)V9(9) COMP-3.
       01  WS-POWER-RESULT         PIC 9(15)V9(9) COMP-3.
       
       01  WS-CURRENCY-FROM        PIC X(3).
       01  WS-CURRENCY-TO          PIC X(3).
       01  WS-AMOUNT               PIC 9(12)V99 COMP-3.
       01  WS-EXCHANGE-RATE        PIC 9(5)V9(6) COMP-3.
       01  WS-CONVERTED-AMOUNT     PIC 9(12)V99 COMP-3.
       
       01  WS-INCOME               PIC 9(12)V99 COMP-3.
       01  WS-ASSETS               PIC 9(12)V99 COMP-3.
       01  WS-LIABILITIES          PIC 9(12)V99 COMP-3.
       01  WS-CREDIT-SCORE         PIC 9(3).
       01  WS-EXPERIENCE           PIC 9(2).
       01  WS-RISK-SCORE           PIC 9(3).
       01  WS-RISK-LEVEL           PIC X(10).
       01  WS-RECOMMENDATION       PIC X(100).
       
       01  WS-JSON-OUTPUT          PIC X(1000).
       01  WS-ERROR-MESSAGE        PIC X(100).
       01  WS-STATUS               PIC X(10) VALUE 'SUCCESS'.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           PERFORM READ-INPUT
           PERFORM PROCESS-CALCULATION
           PERFORM WRITE-OUTPUT
           STOP RUN.
       
       READ-INPUT.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO INPUT-RECORD
           CLOSE INPUT-FILE
           
           PERFORM PARSE-JSON-INPUT.
       
       PARSE-JSON-INPUT.
           MOVE 'LOAN-PAYMENT' TO WS-CALCULATION-TYPE
           MOVE 100000.00 TO WS-PRINCIPAL
           MOVE 0.05 TO WS-RATE
           MOVE 360 TO WS-TERM
           MOVE 'MONTHLY' TO WS-FREQUENCY.
       
       PROCESS-CALCULATION.
           EVALUATE WS-CALCULATION-TYPE
               WHEN 'LOAN-PAYMENT'
                   PERFORM CALCULATE-LOAN-PAYMENT
               WHEN 'MORTGAGE-CALCULATOR'
                   PERFORM CALCULATE-LOAN-PAYMENT
               WHEN 'COMPOUND-INTEREST'
                   PERFORM CALCULATE-COMPOUND-INTEREST
               WHEN 'CURRENCY-CONVERSION'
                   PERFORM CALCULATE-CURRENCY-CONVERSION
               WHEN 'RISK-ASSESSMENT'
                   PERFORM CALCULATE-RISK-ASSESSMENT
               WHEN OTHER
                   MOVE 'ERROR' TO WS-STATUS
                   MOVE 'Unknown calculation type' TO WS-ERROR-MESSAGE
           END-EVALUATE.
       
       CALCULATE-LOAN-PAYMENT.
           COMPUTE WS-MONTHLY-RATE = WS-RATE / 12
           MOVE WS-TERM TO WS-NUM-PAYMENTS
           
           IF WS-MONTHLY-RATE = 0
               COMPUTE WS-MONTHLY-PAYMENT = WS-PRINCIPAL / WS-NUM-PAYMENTS
           ELSE
               COMPUTE WS-TEMP-CALC = 1 + WS-MONTHLY-RATE
               PERFORM CALCULATE-POWER
               COMPUTE WS-MONTHLY-PAYMENT = 
                   WS-PRINCIPAL * WS-MONTHLY-RATE * WS-POWER-RESULT /
                   (WS-POWER-RESULT - 1)
           END-IF
           
           COMPUTE WS-TOTAL-PAYMENT = WS-MONTHLY-PAYMENT * WS-NUM-PAYMENTS
           COMPUTE WS-TOTAL-INTEREST = WS-TOTAL-PAYMENT - WS-PRINCIPAL
           
           PERFORM BUILD-LOAN-JSON.
       
       CALCULATE-COMPOUND-INTEREST.
           EVALUATE WS-COMPOUND-FREQ
               WHEN 'MONTHLY'
                   MOVE 12 TO WS-COMPOUND-PERIODS
               WHEN 'QUARTERLY'
                   MOVE 4 TO WS-COMPOUND-PERIODS
               WHEN 'ANNUALLY'
                   MOVE 1 TO WS-COMPOUND-PERIODS
               WHEN OTHER
                   MOVE 12 TO WS-COMPOUND-PERIODS
           END-EVALUATE
           
           COMPUTE WS-EFFECTIVE-RATE = WS-RATE / WS-COMPOUND-PERIODS
           COMPUTE WS-TEMP-CALC = 1 + WS-EFFECTIVE-RATE
           COMPUTE WS-NUM-PAYMENTS = WS-TERM-YEARS * WS-COMPOUND-PERIODS
           
           PERFORM CALCULATE-POWER
           COMPUTE WS-COMPOUND-AMOUNT = WS-PRINCIPAL * WS-POWER-RESULT
           
           PERFORM BUILD-COMPOUND-JSON.
       
       CALCULATE-CURRENCY-CONVERSION.
           PERFORM GET-EXCHANGE-RATE
           COMPUTE WS-CONVERTED-AMOUNT = WS-AMOUNT * WS-EXCHANGE-RATE
           
           PERFORM BUILD-CURRENCY-JSON.
       
       CALCULATE-RISK-ASSESSMENT.
           COMPUTE WS-RISK-SCORE = 0
           
           IF WS-CREDIT-SCORE > 750
               ADD 25 TO WS-RISK-SCORE
           ELSE IF WS-CREDIT-SCORE > 650
               ADD 15 TO WS-RISK-SCORE
           ELSE
               ADD 5 TO WS-RISK-SCORE
           END-IF
           
           IF WS-INCOME > 100000
               ADD 20 TO WS-RISK-SCORE
           ELSE IF WS-INCOME > 50000
               ADD 10 TO WS-RISK-SCORE
           ELSE
               ADD 5 TO WS-RISK-SCORE
           END-IF
           
           IF WS-ASSETS > WS-LIABILITIES * 2
               ADD 25 TO WS-RISK-SCORE
           ELSE IF WS-ASSETS > WS-LIABILITIES
               ADD 15 TO WS-RISK-SCORE
           ELSE
               ADD 5 TO WS-RISK-SCORE
           END-IF
           
           IF WS-EXPERIENCE > 5
               ADD 20 TO WS-RISK-SCORE
           ELSE IF WS-EXPERIENCE > 2
               ADD 10 TO WS-RISK-SCORE
           ELSE
               ADD 5 TO WS-RISK-SCORE
           END-IF
           
           IF WS-RISK-SCORE > 80
               MOVE 'LOW' TO WS-RISK-LEVEL
               MOVE 'Excellent candidate for investment products'
                   TO WS-RECOMMENDATION
           ELSE IF WS-RISK-SCORE > 50
               MOVE 'MEDIUM' TO WS-RISK-LEVEL
               MOVE 'Suitable for balanced portfolio approach'
                   TO WS-RECOMMENDATION
           ELSE
               MOVE 'HIGH' TO WS-RISK-LEVEL
               MOVE 'Conservative investment strategy recommended'
                   TO WS-RECOMMENDATION
           END-IF
           
           PERFORM BUILD-RISK-JSON.
       
       CALCULATE-POWER.
           MOVE 1 TO WS-POWER-RESULT
           PERFORM WS-NUM-PAYMENTS TIMES
               COMPUTE WS-POWER-RESULT = WS-POWER-RESULT * WS-TEMP-CALC
           END-PERFORM.
       
       GET-EXCHANGE-RATE.
           EVALUATE WS-CURRENCY-FROM
               WHEN 'USD'
                   EVALUATE WS-CURRENCY-TO
                       WHEN 'EUR' MOVE 0.85 TO WS-EXCHANGE-RATE
                       WHEN 'GBP' MOVE 0.73 TO WS-EXCHANGE-RATE
                       WHEN 'JPY' MOVE 110.25 TO WS-EXCHANGE-RATE
                       WHEN 'CAD' MOVE 1.25 TO WS-EXCHANGE-RATE
                       WHEN OTHER MOVE 1.0 TO WS-EXCHANGE-RATE
                   END-EVALUATE
               WHEN 'EUR'
                   EVALUATE WS-CURRENCY-TO
                       WHEN 'USD' MOVE 1.18 TO WS-EXCHANGE-RATE
                       WHEN 'GBP' MOVE 0.86 TO WS-EXCHANGE-RATE
                       WHEN 'JPY' MOVE 129.85 TO WS-EXCHANGE-RATE
                       WHEN 'CAD' MOVE 1.47 TO WS-EXCHANGE-RATE
                       WHEN OTHER MOVE 1.0 TO WS-EXCHANGE-RATE
                   END-EVALUATE
               WHEN OTHER
                   MOVE 1.0 TO WS-EXCHANGE-RATE
           END-EVALUATE.
       
       BUILD-LOAN-JSON.
           STRING '{"status":"' WS-STATUS '","result":{'
               '"monthly_payment":' WS-MONTHLY-PAYMENT
               ',"total_interest":' WS-TOTAL-INTEREST
               ',"total_payment":' WS-TOTAL-PAYMENT
               ',"effective_rate":' WS-MONTHLY-RATE
               '}}'
               DELIMITED BY SIZE INTO WS-JSON-OUTPUT.
       
       BUILD-COMPOUND-JSON.
           STRING '{"status":"' WS-STATUS '","result":{'
               '"final_amount":' WS-COMPOUND-AMOUNT
               ',"total_interest":' WS-TOTAL-INTEREST
               ',"effective_rate":' WS-EFFECTIVE-RATE
               '}}'
               DELIMITED BY SIZE INTO WS-JSON-OUTPUT.
       
       BUILD-CURRENCY-JSON.
           STRING '{"status":"' WS-STATUS '","result":{'
               '"from_currency":"' WS-CURRENCY-FROM '"'
               ',"to_currency":"' WS-CURRENCY-TO '"'
               ',"exchange_rate":' WS-EXCHANGE-RATE
               ',"converted_amount":' WS-CONVERTED-AMOUNT
               '}}'
               DELIMITED BY SIZE INTO WS-JSON-OUTPUT.
       
       BUILD-RISK-JSON.
           STRING '{"status":"' WS-STATUS '","result":{'
               '"risk_score":' WS-RISK-SCORE
               ',"risk_level":"' WS-RISK-LEVEL '"'
               ',"recommendation":"' WS-RECOMMENDATION '"'
               '}}'
               DELIMITED BY SIZE INTO WS-JSON-OUTPUT.
       
       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE
           WRITE OUTPUT-RECORD FROM WS-JSON-OUTPUT
           CLOSE OUTPUT-FILE.