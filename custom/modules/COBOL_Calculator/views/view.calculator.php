<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/SugarView.php');

class COBOL_CalculatorViewCalculator extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }
    
    public function display()
    {
        global $mod_strings, $app_strings;
        
        // Add CSS and JavaScript
        echo '<link rel="stylesheet" type="text/css" href="custom/modules/COBOL_Calculator/css/calculator.css">';
        echo '<script src="custom/modules/COBOL_Calculator/js/calculator.js"></script>';
        echo '<script src="include/javascript/jquery/jquery-min.js"></script>';
        
        // Display calculator interface
        $this->displayCalculatorInterface();
    }
    
    private function displayCalculatorInterface()
    {
        global $mod_strings;
        
        echo '<div class="moduleTitle">
                <h2>' . $mod_strings['LBL_CALCULATOR_TITLE'] . '</h2>
                <div class="clear"></div>
              </div>';
        
        echo '<div id="cobol-calculator-container">';
        
        // Calculator Type Selection
        echo '<div class="calculator-section">
                <h3>' . $mod_strings['LBL_SELECT_CALCULATION'] . '</h3>
                <select id="calculation-type" class="form-control" onchange="COBOLCalculator.updateForm()">
                    <option value="">-- Select --</option>
                    <option value="LOAN-PAYMENT">' . $mod_strings['LBL_LOAN_PAYMENT'] . '</option>
                    <option value="COMPOUND-INTEREST">' . $mod_strings['LBL_COMPOUND_INTEREST'] . '</option>
                    <option value="AMORTIZATION">' . $mod_strings['LBL_AMORTIZATION'] . '</option>
                    <option value="ROI-CALCULATOR">' . $mod_strings['LBL_ROI_CALCULATOR'] . '</option>
                    <option value="MORTGAGE-CALCULATOR">' . $mod_strings['LBL_MORTGAGE_CALCULATOR'] . '</option>
                    <option value="SAVINGS-GOAL">' . $mod_strings['LBL_SAVINGS_GOAL'] . '</option>
                    <option value="RETIREMENT-PLANNING">' . $mod_strings['LBL_RETIREMENT_PLANNING'] . '</option>
                    <option value="RISK-ASSESSMENT">' . $mod_strings['LBL_RISK_ASSESSMENT'] . '</option>
                    <option value="CURRENCY-CONVERSION">' . $mod_strings['LBL_CURRENCY_CONVERSION'] . '</option>
                    <option value="TAX-CALCULATION">' . $mod_strings['LBL_TAX_CALCULATION'] . '</option>
                </select>
              </div>';
        
        // Dynamic Form Container
        echo '<div id="calculation-form" class="calculator-form" style="display:none;">
                <form id="cobol-calc-form">
                    <div id="form-fields"></div>
                    <div class="form-actions">
                        <button type="button" class="button primary" onclick="COBOLCalculator.calculate()">
                            ' . $mod_strings['LBL_CALCULATE'] . '
                        </button>
                        <button type="button" class="button" onclick="COBOLCalculator.clear()">
                            ' . $mod_strings['LBL_CLEAR'] . '
                        </button>
                    </div>
                </form>
              </div>';
        
        // Results Container
        echo '<div id="calculation-results" class="results-container" style="display:none;">
                <h3>' . $mod_strings['LBL_CALCULATION_RESULTS'] . '</h3>
                <div id="results-content"></div>
                <div class="results-actions">
                    <button type="button" class="button" onclick="COBOLCalculator.saveCalculation()">
                        ' . $mod_strings['LBL_SAVE_CALCULATION'] . '
                    </button>
                    <button type="button" class="button" onclick="COBOLCalculator.exportResults()">
                        ' . $mod_strings['LBL_EXPORT_RESULTS'] . '
                    </button>
                    <button type="button" class="button" onclick="window.print()">
                        ' . $mod_strings['LBL_PRINT_RESULTS'] . '
                    </button>
                </div>
              </div>';
        
        // History Panel
        echo '<div id="calculation-history" class="history-panel">
                <h3>' . $mod_strings['LBL_VIEW_HISTORY'] . '</h3>
                <div id="history-content">
                    <table class="list view">
                        <thead>
                            <tr>
                                <th>Date</th>
                                <th>Type</th>
                                <th>Status</th>
                                <th>Result</th>
                                <th>Actions</th>
                            </tr>
                        </thead>
                        <tbody id="history-tbody">
                        </tbody>
                    </table>
                </div>
              </div>';
        
        echo '</div>'; // End container
        
        // Initialize JavaScript
        echo '<script>
                $(document).ready(function() {
                    COBOLCalculator.init();
                    COBOLCalculator.loadHistory();
                });
              </script>';
    }
}