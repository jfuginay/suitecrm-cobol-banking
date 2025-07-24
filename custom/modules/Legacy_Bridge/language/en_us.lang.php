<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$mod_strings = array(
    'LBL_MODULE_NAME' => 'Legacy Bridge',
    'LBL_MODULE_TITLE' => 'Legacy System Data Bridge',
    'LBL_SEARCH_FORM_TITLE' => 'Search Legacy Bridges',
    'LBL_LIST_FORM_TITLE' => 'Legacy Bridge List',
    'LBL_NEW_FORM_TITLE' => 'New Legacy Bridge',
    
    // Import Wizard
    'LBL_IMPORT_WIZARD_TITLE' => 'Legacy Data Import Wizard',
    'LBL_EXPORT_WIZARD_TITLE' => 'Legacy Data Export Wizard',
    
    // Fields
    'LBL_ID' => 'ID',
    'LBL_NAME' => 'Bridge Name',
    'LBL_BRIDGE_TYPE' => 'Bridge Type',
    'LBL_SOURCE_SYSTEM' => 'Source System',
    'LBL_SOURCE_FORMAT' => 'Source Format',
    'LBL_COPYBOOK_DEFINITION' => 'COBOL Copybook Definition',
    'LBL_FIELD_MAPPINGS' => 'Field Mappings',
    'LBL_TRANSFORMATION_RULES' => 'Transformation Rules',
    'LBL_IMPORT_STATUS' => 'Import Status',
    'LBL_EXPORT_STATUS' => 'Export Status',
    'LBL_LAST_SYNC_DATE' => 'Last Sync Date',
    'LBL_RECORDS_PROCESSED' => 'Records Processed',
    'LBL_RECORDS_FAILED' => 'Records Failed',
    'LBL_ERROR_LOG' => 'Error Log',
    'LBL_EXECUTION_LOG' => 'Execution Log',
    'LBL_SCHEDULE_ACTIVE' => 'Schedule Active',
    'LBL_SCHEDULE_FREQUENCY' => 'Schedule Frequency',
    'LBL_NEXT_RUN_DATE' => 'Next Run Date',
    
    // Actions
    'LBL_IMPORT_DATA' => 'Import Data',
    'LBL_EXPORT_DATA' => 'Export Data',
    'LBL_VIEW_MAPPINGS' => 'View Mappings',
    'LBL_TEST_CONNECTION' => 'Test Connection',
    'LBL_PARSE_COPYBOOK' => 'Parse Copybook',
    'LBL_PREVIEW_DATA' => 'Preview Data',
    'LBL_START_IMPORT' => 'Start Import',
    'LBL_START_EXPORT' => 'Start Export',
    
    // Status
    'LBL_STATUS_PENDING' => 'Pending',
    'LBL_STATUS_PROCESSING' => 'Processing',
    'LBL_STATUS_COMPLETED' => 'Completed',
    'LBL_STATUS_ERROR' => 'Error',
    
    // Messages
    'LBL_IMPORT_SUCCESS' => 'Import completed successfully',
    'LBL_EXPORT_SUCCESS' => 'Export completed successfully',
    'LBL_COPYBOOK_PARSED' => 'Copybook parsed successfully',
    'LBL_NO_COPYBOOK' => 'No copybook definition found',
    'LBL_INVALID_FORMAT' => 'Invalid file format',
    
    // Help
    'LBL_HELP_COPYBOOK' => 'COBOL copybook defines the structure of legacy data files',
    'LBL_HELP_EBCDIC' => 'EBCDIC encoding is used by IBM mainframes',
    'LBL_HELP_MAPPING' => 'Map legacy fields to SuiteCRM fields for data transformation',
);

// Dropdown lists
$app_list_strings['legacy_bridge_type_list'] = array(
    'CUSTOMER-MASTER' => 'Customer Master File',
    'ACCOUNT-LEDGER' => 'Account Ledger',
    'TRANSACTION-LOG' => 'Transaction Log',
    'PRODUCT-CATALOG' => 'Product Catalog',
    'EMPLOYEE-RECORDS' => 'Employee Records',
    'VENDOR-MASTER' => 'Vendor Master File',
    'INVENTORY-FILE' => 'Inventory File',
    'CUSTOM' => 'Custom Format',
);

$app_list_strings['legacy_source_system_list'] = array(
    'IBM_MAINFRAME' => 'IBM Mainframe',
    'AS400' => 'AS/400',
    'UNISYS' => 'Unisys',
    'TANDEM' => 'Tandem',
    'VAX' => 'VAX/VMS',
    'OTHER' => 'Other',
);

$app_list_strings['legacy_source_format_list'] = array(
    'FIXED_LENGTH' => 'Fixed Length',
    'VARIABLE_LENGTH' => 'Variable Length',
    'VSAM' => 'VSAM',
    'SEQUENTIAL' => 'Sequential',
    'INDEXED' => 'Indexed',
    'RELATIVE' => 'Relative',
);

$app_list_strings['legacy_sync_status_list'] = array(
    'pending' => 'Pending',
    'processing' => 'Processing',
    'completed' => 'Completed',
    'error' => 'Error',
    'cancelled' => 'Cancelled',
);