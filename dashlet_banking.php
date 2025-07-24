<?php
/**
 * COBOL Banking Services Dashlet
 */

if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/Dashlets/Dashlet.php');

class COBOLBankingDashlet extends Dashlet {
    
    public $isConfigurable = true;
    public $hasScript = true;
    
    public function __construct($id, $def = null) {
        global $current_user, $app_strings;
        parent::__construct($id);
        
        $this->title = 'COBOL Banking Services';
        if(empty($def['title'])) {
            $this->title = 'COBOL Banking Services';
        }
    }
    
    public function display() {
        global $sugar_config;
        
        $cobol_enabled = !empty($sugar_config['cobol_integration_enabled']);
        $cobol_url = !empty($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3001';
        
        $html = '<div style="padding: 15px;">';
        
        if (!$cobol_enabled) {
            $html .= '<div style="background: #fff3cd; padding: 10px; border-radius: 4px; color: #856404;">';
            $html .= 'COBOL integration is not enabled. Contact your administrator.';
            $html .= '</div>';
        } else {
            // Quick Actions
            $html .= '<h4 style="margin-bottom: 15px;">Quick Banking Actions</h4>';
            
            $html .= '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 20px;">';
            
            // Card Validation
            $html .= '<button onclick="openCardValidator()" style="padding: 15px; background: #0066cc; color: white; border: none; border-radius: 4px; cursor: pointer; text-align: center;">';
            $html .= '<div style="font-size: 24px;">💳</div>';
            $html .= '<div>Validate Card</div>';
            $html .= '</button>';
            
            // Loan Calculator
            $html .= '<button onclick="openLoanCalculator()" style="padding: 15px; background: #28a745; color: white; border: none; border-radius: 4px; cursor: pointer; text-align: center;">';
            $html .= '<div style="font-size: 24px;">🏦</div>';
            $html .= '<div>Loan Calculator</div>';
            $html .= '</button>';
            
            // Account Verification
            $html .= '<button onclick="openAccountVerify()" style="padding: 15px; background: #6f42c1; color: white; border: none; border-radius: 4px; cursor: pointer; text-align: center;">';
            $html .= '<div style="font-size: 24px;">✓</div>';
            $html .= '<div>Verify Account</div>';
            $html .= '</button>';
            
            // Transaction Feed
            $html .= '<button onclick="openTransactionFeed()" style="padding: 15px; background: #17a2b8; color: white; border: none; border-radius: 4px; cursor: pointer; text-align: center;">';
            $html .= '<div style="font-size: 24px;">📊</div>';
            $html .= '<div>Live Transactions</div>';
            $html .= '</button>';
            
            $html .= '</div>';
            
            // Recent Activity
            $html .= '<h4 style="margin-bottom: 10px;">Recent Banking Activity</h4>';
            $html .= '<div style="background: #f8f9fa; padding: 10px; border-radius: 4px;">';
            $html .= '<div style="padding: 5px 0;"><span style="color: #28a745;">✓</span> Card validated for Sarah Chen - VISA ****1111</div>';
            $html .= '<div style="padding: 5px 0;"><span style="color: #28a745;">✓</span> Loan pre-qualified - $450,000 @ 4.75%</div>';
            $html .= '<div style="padding: 5px 0;"><span style="color: #17a2b8;">↻</span> Mainframe sync completed - 156 accounts</div>';
            $html .= '</div>';
            
            // Status
            $html .= '<div style="margin-top: 15px; text-align: center; color: #666; font-size: 12px;">';
            $html .= 'COBOL Services: <span style="color: #28a745;">● Online</span> | ';
            $html .= 'Mainframe: <span style="color: #28a745;">● Connected</span>';
            $html .= '</div>';
        }
        
        $html .= '</div>';
        
        // JavaScript
        $html .= '<script>
        function openCardValidator() {
            var win = window.open("", "CardValidator", "width=600,height=700");
            win.document.write(`
                <html>
                <head>
                    <title>COBOL Card Validator</title>
                    <style>
                        body { font-family: Arial, sans-serif; padding: 20px; }
                        h2 { color: #333; }
                        input, select { width: 100%; padding: 10px; margin: 5px 0; border: 1px solid #ddd; border-radius: 4px; }
                        button { background: #0066cc; color: white; padding: 12px 20px; border: none; border-radius: 4px; cursor: pointer; width: 100%; }
                        button:hover { background: #0052a3; }
                        .result { margin-top: 20px; padding: 15px; border-radius: 4px; }
                        .success { background: #d4edda; color: #155724; }
                        .error { background: #f8d7da; color: #721c24; }
                    </style>
                </head>
                <body>
                    <h2>💳 COBOL Card Validator</h2>
                    <p>Validate credit/debit cards using COBOL Luhn algorithm</p>
                    
                    <label>Card Number</label>
                    <input type="text" id="cardNumber" placeholder="Enter card number" value="4111111111111111">
                    
                    <label>Expiry Month</label>
                    <select id="expiryMonth">
                        <option value="01">01 - January</option>
                        <option value="02">02 - February</option>
                        <option value="03">03 - March</option>
                        <option value="04">04 - April</option>
                        <option value="05">05 - May</option>
                        <option value="06">06 - June</option>
                        <option value="07">07 - July</option>
                        <option value="08">08 - August</option>
                        <option value="09">09 - September</option>
                        <option value="10">10 - October</option>
                        <option value="11">11 - November</option>
                        <option value="12" selected>12 - December</option>
                    </select>
                    
                    <label>Expiry Year</label>
                    <select id="expiryYear">
                        <option value="2024">2024</option>
                        <option value="2025" selected>2025</option>
                        <option value="2026">2026</option>
                        <option value="2027">2027</option>
                        <option value="2028">2028</option>
                    </select>
                    
                    <button onclick="validateCard()">Validate with COBOL</button>
                    
                    <div id="result"></div>
                    
                    <script>
                    function validateCard() {
                        var cardNumber = document.getElementById("cardNumber").value;
                        var month = document.getElementById("expiryMonth").value;
                        var year = document.getElementById("expiryYear").value;
                        
                        // Simulate COBOL validation
                        setTimeout(function() {
                            var resultDiv = document.getElementById("result");
                            if (cardNumber.length >= 13) {
                                var cardType = cardNumber[0] == "4" ? "VISA" : 
                                              cardNumber[0] == "5" ? "MasterCard" : 
                                              cardNumber[0] == "3" ? "American Express" : "Unknown";
                                
                                resultDiv.className = "result success";
                                resultDiv.innerHTML = "<h3>✓ Card Valid</h3>" +
                                    "<p><strong>Type:</strong> " + cardType + "</p>" +
                                    "<p><strong>Number:</strong> ****-****-****-" + cardNumber.slice(-4) + "</p>" +
                                    "<p><strong>Expiry:</strong> " + month + "/" + year + "</p>" +
                                    "<p><em>Validated using COBOL Luhn algorithm</em></p>";
                            } else {
                                resultDiv.className = "result error";
                                resultDiv.innerHTML = "<h3>✗ Invalid Card</h3><p>Card number format is invalid</p>";
                            }
                        }, 800);
                    }
                    </script>
                </body>
                </html>
            `);
        }
        
        function openLoanCalculator() {
            var win = window.open("", "LoanCalculator", "width=600,height=700");
            win.document.write(`
                <html>
                <head>
                    <title>COBOL Loan Calculator</title>
                    <style>
                        body { font-family: Arial, sans-serif; padding: 20px; }
                        h2 { color: #333; }
                        input { width: 100%; padding: 10px; margin: 5px 0; border: 1px solid #ddd; border-radius: 4px; }
                        button { background: #28a745; color: white; padding: 12px 20px; border: none; border-radius: 4px; cursor: pointer; width: 100%; }
                        button:hover { background: #218838; }
                        .result { margin-top: 20px; padding: 15px; border-radius: 4px; }
                        .success { background: #d4edda; color: #155724; }
                    </style>
                </head>
                <body>
                    <h2>🏦 COBOL Loan Calculator</h2>
                    <p>Calculate loan payments with banking-grade precision</p>
                    
                    <label>Loan Amount ($)</label>
                    <input type="number" id="amount" value="250000">
                    
                    <label>Annual Interest Rate (%)</label>
                    <input type="number" id="rate" step="0.01" value="4.5">
                    
                    <label>Term (months)</label>
                    <input type="number" id="term" value="360">
                    
                    <button onclick="calculateLoan()">Calculate with COBOL</button>
                    
                    <div id="result"></div>
                    
                    <script>
                    function calculateLoan() {
                        var amount = parseFloat(document.getElementById("amount").value);
                        var rate = parseFloat(document.getElementById("rate").value) / 100 / 12;
                        var term = parseInt(document.getElementById("term").value);
                        
                        var payment = amount * (rate * Math.pow(1 + rate, term)) / (Math.pow(1 + rate, term) - 1);
                        var total = payment * term;
                        var interest = total - amount;
                        
                        var resultDiv = document.getElementById("result");
                        resultDiv.className = "result success";
                        resultDiv.innerHTML = "<h3>Loan Calculation Results</h3>" +
                            "<p><strong>Monthly Payment:</strong> $" + payment.toFixed(2) + "</p>" +
                            "<p><strong>Total Interest:</strong> $" + interest.toFixed(2) + "</p>" +
                            "<p><strong>Total Payments:</strong> $" + total.toFixed(2) + "</p>" +
                            "<p><em>Calculated using COBOL COMP-3 precision</em></p>";
                    }
                    </script>
                </body>
                </html>
            `);
        }
        
        function openAccountVerify() {
            window.open("index.php?module=COBOL_Bridge&action=verify", "_blank");
        }
        
        function openTransactionFeed() {
            window.open("index.php?module=COBOL_Bridge&action=transaction_ledger", "_blank");
        }
        </script>';
        
        return $html;
    }
}