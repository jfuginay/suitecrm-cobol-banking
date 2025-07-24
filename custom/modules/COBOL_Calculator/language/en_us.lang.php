<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$mod_strings = array(
    'LBL_MODULE_NAME' => 'COBOL Calculator',
    'LBL_MODULE_TITLE' => 'COBOL Calculator',
    'LBL_MODULE_ID' => 'COBOL Calculator',
    'LBL_SEARCH_FORM_TITLE' => 'Search COBOL Calculations',
    'LBL_LIST_FORM_TITLE' => 'COBOL Calculations List',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'History',
    'LBL_NEW_FORM_TITLE' => 'New COBOL Calculation',
    
    // Fields
    'LBL_ID' => 'ID',
    'LBL_NAME' => 'Calculation Name',
    'LBL_DATE_ENTERED' => 'Date Created',
    'LBL_DATE_MODIFIED' => 'Date Modified',
    'LBL_CREATED' => 'Created By',
    'LBL_MODIFIED' => 'Modified By',
    'LBL_DESCRIPTION' => 'Description',
    'LBL_DELETED' => 'Deleted',
    'LBL_CALCULATION_TYPE' => 'Calculation Type',
    'LBL_INPUT_PARAMETERS' => 'Input Parameters',
    'LBL_CALCULATION_RESULT' => 'Calculation Result',
    'LBL_COBOL_RESPONSE' => 'COBOL Response',
    'LBL_EXECUTION_TIME' => 'Execution Time (seconds)',
    'LBL_STATUS' => 'Status',
    'LBL_ASSIGNED_TO' => 'Assigned To',
    'LBL_ACCOUNT' => 'Account',
    'LBL_ACCOUNT_ID' => 'Account ID',
    'LBL_CONTACT' => 'Contact',
    'LBL_CONTACT_ID' => 'Contact ID',
    
    // Calculator Types
    'LBL_LOAN_PAYMENT' => 'Loan Payment Calculator',
    'LBL_COMPOUND_INTEREST' => 'Compound Interest Calculator',
    'LBL_AMORTIZATION' => 'Amortization Schedule',
    'LBL_ROI_CALCULATOR' => 'Return on Investment',
    'LBL_MORTGAGE_CALCULATOR' => 'Mortgage Calculator',
    'LBL_SAVINGS_GOAL' => 'Savings Goal Calculator',
    'LBL_RETIREMENT_PLANNING' => 'Retirement Planning',
    'LBL_RISK_ASSESSMENT' => 'Risk Assessment Score',
    'LBL_CURRENCY_CONVERSION' => 'Currency Conversion',
    'LBL_TAX_CALCULATION' => 'Tax Calculation',
    
    // Calculator Interface
    'LBL_CALCULATOR_TITLE' => 'COBOL Financial Calculator',
    'LBL_SELECT_CALCULATION' => 'Select Calculation Type',
    'LBL_PRINCIPAL_AMOUNT' => 'Principal Amount',
    'LBL_INTEREST_RATE' => 'Interest Rate (%)',
    'LBL_LOAN_TERM' => 'Loan Term (months)',
    'LBL_LOAN_TERM_YEARS' => 'Loan Term (years)',
    'LBL_PAYMENT_FREQUENCY' => 'Payment Frequency',
    'LBL_CALCULATE' => 'Calculate',
    'LBL_CLEAR' => 'Clear',
    'LBL_MONTHLY_PAYMENT' => 'Monthly Payment',
    'LBL_TOTAL_INTEREST' => 'Total Interest',
    'LBL_TOTAL_PAYMENT' => 'Total Payment',
    'LBL_EFFECTIVE_RATE' => 'Effective Rate',
    'LBL_COMPOUND_FREQUENCY' => 'Compound Frequency',
    'LBL_INITIAL_INVESTMENT' => 'Initial Investment',
    'LBL_MONTHLY_CONTRIBUTION' => 'Monthly Contribution',
    'LBL_YEARS' => 'Years',
    'LBL_EXPECTED_RETURN' => 'Expected Return (%)',
    'LBL_RISK_TOLERANCE' => 'Risk Tolerance',
    'LBL_CURRENCY_FROM' => 'From Currency',
    'LBL_CURRENCY_TO' => 'To Currency',
    'LBL_AMOUNT' => 'Amount',
    'LBL_EXCHANGE_RATE' => 'Exchange Rate',
    'LBL_CONVERTED_AMOUNT' => 'Converted Amount',
    
    // Results
    'LBL_CALCULATION_RESULTS' => 'Calculation Results',
    'LBL_RESULT_SUMMARY' => 'Result Summary',
    'LBL_DETAILED_SCHEDULE' => 'Detailed Schedule',
    'LBL_EXPORT_RESULTS' => 'Export Results',
    'LBL_SAVE_CALCULATION' => 'Save Calculation',
    'LBL_PRINT_RESULTS' => 'Print Results',
    
    // Status
    'LBL_STATUS_PENDING' => 'Pending',
    'LBL_STATUS_PROCESSING' => 'Processing',
    'LBL_STATUS_COMPLETED' => 'Completed',
    'LBL_STATUS_ERROR' => 'Error',
    
    // Messages
    'LBL_CALCULATION_SUCCESS' => 'Calculation completed successfully',
    'LBL_CALCULATION_ERROR' => 'Error performing calculation',
    'LBL_COBOL_SERVICE_UNAVAILABLE' => 'COBOL service is currently unavailable',
    'LBL_INVALID_PARAMETERS' => 'Invalid calculation parameters',
    'LBL_CALCULATION_SAVED' => 'Calculation saved successfully',
    
    // Payment Frequencies
    'LBL_MONTHLY' => 'Monthly',
    'LBL_QUARTERLY' => 'Quarterly',
    'LBL_SEMI_ANNUALLY' => 'Semi-Annually',
    'LBL_ANNUALLY' => 'Annually',
    
    // Risk Levels
    'LBL_CONSERVATIVE' => 'Conservative',
    'LBL_MODERATE' => 'Moderate',
    'LBL_AGGRESSIVE' => 'Aggressive',
    
    // Actions
    'LBL_VIEW_HISTORY' => 'View Calculation History',
    'LBL_RUN_NEW_CALCULATION' => 'Run New Calculation',
    'LBL_COMPARE_CALCULATIONS' => 'Compare Calculations',
    'LBL_GENERATE_REPORT' => 'Generate Report',
    
    // Help Text
    'LBL_HELP_PRINCIPAL' => 'Enter the principal amount for the calculation',
    'LBL_HELP_INTEREST' => 'Enter the annual interest rate as a percentage',
    'LBL_HELP_TERM' => 'Enter the loan term in months',
    'LBL_HELP_CALCULATOR' => 'COBOL-powered financial calculator provides banking-grade precision for all calculations',
);

// Dropdown lists
$app_list_strings['cobol_calculation_type_list'] = array(
    'LOAN-PAYMENT' => 'Loan Payment',
    'COMPOUND-INTEREST' => 'Compound Interest',
    'AMORTIZATION' => 'Amortization Schedule',
    'ROI-CALCULATOR' => 'ROI Calculator',
    'MORTGAGE-CALCULATOR' => 'Mortgage Calculator',
    'SAVINGS-GOAL' => 'Savings Goal',
    'RETIREMENT-PLANNING' => 'Retirement Planning',
    'RISK-ASSESSMENT' => 'Risk Assessment',
    'CURRENCY-CONVERSION' => 'Currency Conversion',
    'TAX-CALCULATION' => 'Tax Calculation',
);

$app_list_strings['cobol_calculation_status_list'] = array(
    'pending' => 'Pending',
    'processing' => 'Processing',
    'completed' => 'Completed',
    'error' => 'Error',
);

$app_list_strings['payment_frequency_list'] = array(
    'monthly' => 'Monthly',
    'quarterly' => 'Quarterly',
    'semi_annually' => 'Semi-Annually',
    'annually' => 'Annually',
);

$app_list_strings['risk_tolerance_list'] = array(
    'conservative' => 'Conservative (3-5%)',
    'moderate' => 'Moderate (6-8%)',
    'aggressive' => 'Aggressive (9-12%)',
);