<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/SugarView.php');

class Business_RulesViewManage extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }
    
    public function display()
    {
        global $mod_strings, $app_strings;
        
        // Add CSS and JavaScript
        echo '<link rel="stylesheet" type="text/css" href="custom/modules/Business_Rules/css/validation.css">';
        echo '<script src="custom/modules/Business_Rules/js/rule_manager.js"></script>';
        echo '<script src="include/javascript/jquery/jquery-min.js"></script>';
        
        $this->displayRuleManager();
    }
    
    private function displayRuleManager()
    {
        global $mod_strings;
        
        echo '<div class="moduleTitle">
                <h2>' . $mod_strings['LBL_BUSINESS_RULES_MANAGER'] . '</h2>
                <div class="clear"></div>
              </div>';
        
        echo '<div id="business-rules-container">';
        
        // Tabs for different views
        echo '<div class="rule-tabs">
                <ul class="nav nav-tabs">
                    <li class="active"><a href="#rules-list" data-toggle="tab">Active Rules</a></li>
                    <li><a href="#create-rule" data-toggle="tab">Create Rule</a></li>
                    <li><a href="#test-rule" data-toggle="tab">Test Rules</a></li>
                    <li><a href="#rule-analytics" data-toggle="tab">Analytics</a></li>
                </ul>
              </div>';
        
        echo '<div class="tab-content">';
        
        // Tab 1: Active Rules List
        echo '<div id="rules-list" class="tab-pane active">
                <h3>Active Business Rules</h3>
                
                <div class="rule-filters">
                    <select id="filter-module" onchange="RuleManager.filterRules()">
                        <option value="">All Modules</option>
                        <option value="Accounts">Accounts</option>
                        <option value="Contacts">Contacts</option>
                        <option value="Opportunities">Opportunities</option>
                        <option value="Quotes">Quotes</option>
                    </select>
                    
                    <select id="filter-type" onchange="RuleManager.filterRules()">
                        <option value="">All Types</option>
                        <option value="COBOL_VALIDATION">COBOL Validation</option>
                        <option value="REGEX_PATTERN">Regex Pattern</option>
                        <option value="RANGE_CHECK">Range Check</option>
                        <option value="LOOKUP_VALIDATION">Lookup</option>
                        <option value="CROSS_FIELD">Cross-Field</option>
                    </select>
                    
                    <button class="button" onclick="RuleManager.refreshRules()">
                        Refresh
                    </button>
                </div>
                
                <table class="rules-list-table">
                    <thead>
                        <tr>
                            <th>Priority</th>
                            <th>Rule Name</th>
                            <th>Module</th>
                            <th>Field</th>
                            <th>Type</th>
                            <th>Real-time</th>
                            <th>Status</th>
                            <th>Actions</th>
                        </tr>
                    </thead>
                    <tbody id="rules-tbody">
                        <!-- Rules will be loaded here -->
                    </tbody>
                </table>
              </div>';
        
        // Tab 2: Create Rule
        echo '<div id="create-rule" class="tab-pane">
                <h3>Create New Business Rule</h3>
                
                <form id="create-rule-form" class="rule-config-form">
                    <div class="form-group">
                        <label>Rule Name *</label>
                        <input type="text" id="rule_name" required>
                    </div>
                    
                    <div class="form-group">
                        <label>Description</label>
                        <textarea id="rule_description" rows="3"></textarea>
                    </div>
                    
                    <div class="form-group">
                        <label>Rule Type *</label>
                        <select id="rule_type" onchange="RuleManager.updateRuleForm()" required>
                            <option value="">-- Select --</option>
                            <option value="COBOL_VALIDATION">COBOL Program Validation</option>
                            <option value="REGEX_PATTERN">Regular Expression Pattern</option>
                            <option value="RANGE_CHECK">Numeric Range Check</option>
                            <option value="LOOKUP_VALIDATION">Database Lookup</option>
                            <option value="CROSS_FIELD">Cross-Field Validation</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>Target Module *</label>
                        <select id="rule_module" onchange="RuleManager.loadModuleFields()" required>
                            <option value="">-- Select --</option>
                            <option value="Accounts">Accounts</option>
                            <option value="Contacts">Contacts</option>
                            <option value="Opportunities">Opportunities</option>
                            <option value="Quotes">Quotes</option>
                            <option value="Leads">Leads</option>
                            <option value="*">All Modules</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>Target Field *</label>
                        <select id="rule_field" required>
                            <option value="">-- Select Module First --</option>
                            <option value="*">All Fields</option>
                        </select>
                    </div>
                    
                    <div id="rule-type-config">
                        <!-- Dynamic configuration based on rule type -->
                    </div>
                    
                    <div class="form-group">
                        <label>Validation Message *</label>
                        <input type="text" id="validation_message" required>
                    </div>
                    
                    <div class="form-group">
                        <label>Priority</label>
                        <select id="priority">
                            <option value="1">High (1)</option>
                            <option value="5" selected>Medium (5)</option>
                            <option value="10">Low (10)</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>
                            <input type="checkbox" id="real_time" checked>
                            Enable real-time validation
                        </label>
                    </div>
                    
                    <div class="form-group">
                        <label>
                            <input type="checkbox" id="active" checked>
                            Active
                        </label>
                    </div>
                    
                    <div class="form-actions">
                        <button type="submit" class="button primary">Create Rule</button>
                        <button type="button" class="button" onclick="RuleManager.clearForm()">Clear</button>
                    </div>
                </form>
              </div>';
        
        // Tab 3: Test Rules
        echo '<div id="test-rule" class="tab-pane">
                <h3>Test Business Rules</h3>
                
                <div class="test-rule-form">
                    <div class="form-group">
                        <label>Select Module</label>
                        <select id="test_module" onchange="RuleManager.loadTestFields()">
                            <option value="">-- Select --</option>
                            <option value="Accounts">Accounts</option>
                            <option value="Contacts">Contacts</option>
                            <option value="Opportunities">Opportunities</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>Select Field</label>
                        <select id="test_field">
                            <option value="">-- Select Module First --</option>
                        </select>
                    </div>
                    
                    <div class="form-group">
                        <label>Test Value</label>
                        <input type="text" id="test_value">
                    </div>
                    
                    <div class="form-group">
                        <label>Additional Context (JSON)</label>
                        <textarea id="test_context" rows="5">{}</textarea>
                    </div>
                    
                    <button class="button primary" onclick="RuleManager.testValidation()">
                        Test Validation
                    </button>
                    
                    <div id="test-results" style="margin-top: 20px;">
                        <!-- Test results will appear here -->
                    </div>
                </div>
              </div>';
        
        // Tab 4: Analytics
        echo '<div id="rule-analytics" class="tab-pane">
                <h3>Validation Analytics</h3>
                
                <div class="analytics-summary">
                    <div class="stat-box">
                        <h4>Total Rules</h4>
                        <span class="stat-value" id="total-rules">0</span>
                    </div>
                    
                    <div class="stat-box">
                        <h4>Active Rules</h4>
                        <span class="stat-value" id="active-rules">0</span>
                    </div>
                    
                    <div class="stat-box">
                        <h4>Validations Today</h4>
                        <span class="stat-value" id="validations-today">0</span>
                    </div>
                    
                    <div class="stat-box">
                        <h4>Failed Validations</h4>
                        <span class="stat-value" id="failed-validations">0</span>
                    </div>
                </div>
                
                <div class="chart-container">
                    <h4>Validation Performance</h4>
                    <canvas id="validation-chart"></canvas>
                </div>
                
                <div class="top-failures">
                    <h4>Top Validation Failures</h4>
                    <table class="table">
                        <thead>
                            <tr>
                                <th>Rule</th>
                                <th>Module</th>
                                <th>Field</th>
                                <th>Failures</th>
                            </tr>
                        </thead>
                        <tbody id="top-failures-tbody">
                            <!-- Data will be loaded here -->
                        </tbody>
                    </table>
                </div>
              </div>';
        
        echo '</div>'; // End tab-content
        echo '</div>'; // End container
        
        // Initialize
        echo '<script>
                $(document).ready(function() {
                    RuleManager.init();
                });
              </script>';
    }
}