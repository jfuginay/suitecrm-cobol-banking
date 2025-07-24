<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/SugarView.php');

class Legacy_BridgeViewImport extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }
    
    public function display()
    {
        global $mod_strings, $app_strings;
        
        // Add CSS and JavaScript
        echo '<link rel="stylesheet" type="text/css" href="custom/modules/Legacy_Bridge/css/legacy_bridge.css">';
        echo '<script src="custom/modules/Legacy_Bridge/js/legacy_bridge.js"></script>';
        echo '<script src="include/javascript/jquery/jquery-min.js"></script>';
        
        $this->displayImportWizard();
    }
    
    private function displayImportWizard()
    {
        global $mod_strings;
        
        echo '<div class="moduleTitle">
                <h2>' . $mod_strings['LBL_IMPORT_WIZARD_TITLE'] . '</h2>
                <div class="clear"></div>
              </div>';
        
        echo '<div id="legacy-import-wizard">';
        
        // Progress Steps
        echo '<div class="wizard-steps">
                <div class="step active" data-step="1">
                    <span class="step-number">1</span>
                    <span class="step-title">Select Source</span>
                </div>
                <div class="step" data-step="2">
                    <span class="step-number">2</span>
                    <span class="step-title">Define Format</span>
                </div>
                <div class="step" data-step="3">
                    <span class="step-number">3</span>
                    <span class="step-title">Map Fields</span>
                </div>
                <div class="step" data-step="4">
                    <span class="step-number">4</span>
                    <span class="step-title">Review & Import</span>
                </div>
              </div>';
        
        // Step 1: Select Source
        echo '<div class="wizard-content" id="step-1">
                <h3>Select Legacy Data Source</h3>
                <form id="import-form-step1">
                    <div class="form-group">
                        <label>Bridge Type:</label>
                        <select id="bridge_type" class="form-control">
                            <option value="">-- Select --</option>
                            <option value="CUSTOMER-MASTER">Customer Master File</option>
                            <option value="ACCOUNT-LEDGER">Account Ledger</option>
                            <option value="TRANSACTION-LOG">Transaction Log</option>
                            <option value="PRODUCT-CATALOG">Product Catalog</option>
                            <option value="EMPLOYEE-RECORDS">Employee Records</option>
                            <option value="CUSTOM">Custom Format</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>Source System:</label>
                        <select id="source_system" class="form-control">
                            <option value="IBM_MAINFRAME">IBM Mainframe</option>
                            <option value="AS400">AS/400</option>
                            <option value="UNISYS">Unisys</option>
                            <option value="TANDEM">Tandem</option>
                            <option value="OTHER">Other</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>File Format:</label>
                        <select id="source_format" class="form-control">
                            <option value="FIXED_LENGTH">Fixed Length</option>
                            <option value="VARIABLE_LENGTH">Variable Length</option>
                            <option value="VSAM">VSAM</option>
                            <option value="SEQUENTIAL">Sequential</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>Character Encoding:</label>
                        <select id="encoding" class="form-control">
                            <option value="EBCDIC">EBCDIC</option>
                            <option value="ASCII">ASCII</option>
                            <option value="UTF8">UTF-8</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>Upload Legacy File:</label>
                        <input type="file" id="legacy_file" accept=".dat,.txt,.ebcdic,.cobol" />
                        <div class="help-text">Supported formats: .dat, .txt, .ebcdic, .cobol</div>
                    </div>
                </form>
              </div>';
        
        // Step 2: Define Format
        echo '<div class="wizard-content" id="step-2" style="display:none;">
                <h3>Define Data Format</h3>
                <form id="import-form-step2">
                    <div class="form-group">
                        <label>Copybook Definition:</label>
                        <div class="copybook-options">
                            <input type="radio" name="copybook_source" value="upload" id="copybook_upload">
                            <label for="copybook_upload">Upload Copybook File</label>
                            <input type="file" id="copybook_file" accept=".cpy,.cob" style="display:none;">
                            
                            <input type="radio" name="copybook_source" value="manual" id="copybook_manual">
                            <label for="copybook_manual">Enter Manually</label>
                            
                            <input type="radio" name="copybook_source" value="template" id="copybook_template">
                            <label for="copybook_template">Use Template</label>
                        </div>
                    </div>
                    
                    <div class="form-group" id="copybook_editor" style="display:none;">
                        <label>Copybook Content:</label>
                        <textarea id="copybook_content" rows="15" class="form-control copybook-editor">
       01  CUSTOMER-RECORD.
           05  CUST-ID         PIC X(10).
           05  CUST-NAME       PIC X(30).
           05  CUST-ADDRESS    PIC X(50).
           05  CUST-CITY       PIC X(20).
           05  CUST-STATE      PIC X(2).
           05  CUST-ZIP        PIC X(10).
           05  CUST-BALANCE    PIC S9(9)V99 COMP-3.
                        </textarea>
                    </div>
                    
                    <div class="form-group">
                        <button type="button" class="button" onclick="LegacyBridge.parseCopybook()">
                            Parse Copybook
                        </button>
                    </div>
                    
                    <div id="copybook_preview" class="preview-container"></div>
                </form>
              </div>';
        
        // Step 3: Map Fields
        echo '<div class="wizard-content" id="step-3" style="display:none;">
                <h3>Map Fields to SuiteCRM</h3>
                <form id="import-form-step3">
                    <div class="form-group">
                        <label>Target Module:</label>
                        <select id="target_module" class="form-control" onchange="LegacyBridge.loadModuleFields()">
                            <option value="">-- Select --</option>
                            <option value="Accounts">Accounts</option>
                            <option value="Contacts">Contacts</option>
                            <option value="Leads">Leads</option>
                            <option value="Opportunities">Opportunities</option>
                            <option value="Cases">Cases</option>
                        </select>
                    </div>
                    
                    <div id="field_mapping_container">
                        <h4>Field Mappings</h4>
                        <table class="field-mapping-table">
                            <thead>
                                <tr>
                                    <th>Legacy Field</th>
                                    <th>SuiteCRM Field</th>
                                    <th>Transformation</th>
                                </tr>
                            </thead>
                            <tbody id="field_mapping_tbody">
                            </tbody>
                        </table>
                    </div>
                    
                    <div class="form-group">
                        <h4>Transformation Rules</h4>
                        <button type="button" class="button" onclick="LegacyBridge.addTransformationRule()">
                            Add Rule
                        </button>
                        <div id="transformation_rules"></div>
                    </div>
                </form>
              </div>';
        
        // Step 4: Review & Import
        echo '<div class="wizard-content" id="step-4" style="display:none;">
                <h3>Review & Import</h3>
                <div id="import-summary">
                    <h4>Import Summary</h4>
                    <div class="summary-item">
                        <label>Bridge Type:</label>
                        <span id="summary_bridge_type"></span>
                    </div>
                    <div class="summary-item">
                        <label>Source File:</label>
                        <span id="summary_file_name"></span>
                    </div>
                    <div class="summary-item">
                        <label>Target Module:</label>
                        <span id="summary_target_module"></span>
                    </div>
                    <div class="summary-item">
                        <label>Field Mappings:</label>
                        <span id="summary_field_count"></span>
                    </div>
                </div>
                
                <div id="preview-data">
                    <h4>Preview Data (First 5 Records)</h4>
                    <div id="preview-table-container"></div>
                </div>
                
                <div class="import-options">
                    <h4>Import Options</h4>
                    <label>
                        <input type="checkbox" id="validate_data" checked>
                        Validate data before import
                    </label>
                    <label>
                        <input type="checkbox" id="check_duplicates" checked>
                        Check for duplicates
                    </label>
                    <label>
                        <input type="checkbox" id="create_backup">
                        Create backup before import
                    </label>
                </div>
              </div>';
        
        // Navigation Buttons
        echo '<div class="wizard-navigation">
                <button type="button" class="button" id="btn-previous" onclick="LegacyBridge.previousStep()" style="display:none;">
                    Previous
                </button>
                <button type="button" class="button primary" id="btn-next" onclick="LegacyBridge.nextStep()">
                    Next
                </button>
                <button type="button" class="button primary" id="btn-import" onclick="LegacyBridge.startImport()" style="display:none;">
                    Start Import
                </button>
              </div>';
        
        // Progress Bar
        echo '<div id="import-progress" style="display:none;">
                <h3>Import Progress</h3>
                <div class="progress-bar">
                    <div class="progress-fill" id="progress-fill"></div>
                </div>
                <div class="progress-stats">
                    <span>Records Processed: <span id="records-processed">0</span></span>
                    <span>Errors: <span id="records-errors">0</span></span>
                </div>
                <div id="import-log" class="import-log"></div>
              </div>';
        
        echo '</div>'; // End wizard container
    }
}