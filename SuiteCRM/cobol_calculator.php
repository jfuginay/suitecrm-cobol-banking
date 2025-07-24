<?php
// COBOL Calculator - Standalone Demo
?>
<!DOCTYPE html>
<html>
<head>
    <title>COBOL Financial Calculator - SuiteCRM</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
        .container { max-width: 800px; margin: 0 auto; background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        h1 { color: #333; }
        .cobol-badge { background: #6f42c1; color: white; padding: 3px 8px; border-radius: 3px; font-size: 12px; }
        select, input { width: 100%; padding: 8px; margin: 5px 0; border: 1px solid #ddd; border-radius: 4px; }
        button { background: #0066cc; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; font-size: 16px; }
        button:hover { background: #0052a3; }
        .result { background: #e8f5e9; padding: 20px; border-radius: 5px; margin-top: 20px; display: none; }
        .error { background: #ffebee; color: #c62828; padding: 20px; border-radius: 5px; margin-top: 20px; display: none; }
    </style>
</head>
<body>
    <div class="container">
        <h1>COBOL Financial Calculator <span class="cobol-badge">Powered by COBOL</span></h1>
        <p>Banking-grade precision calculations using real COBOL programs</p>
        
        <div style="margin: 20px 0;">
            <label>Calculation Type:</label>
            <select id="calc-type" onchange="updateForm()">
                <option value="">-- Select --</option>
                <option value="LOAN-PAYMENT">Loan Payment (Mortgage)</option>
                <option value="SIMPLE-INTEREST">Simple Interest</option>
                <option value="COMPOUND-INTEREST">Compound Interest</option>
            </select>
        </div>
        
        <div id="loan-fields" style="display: none;">
            <label>Loan Amount ($):</label>
            <input type="number" id="principal" step="0.01" placeholder="250000">
            
            <label>Annual Interest Rate (%):</label>
            <input type="number" id="rate" step="0.001" placeholder="4.5">
            
            <label>Term (months):</label>
            <input type="number" id="term" step="1" placeholder="360">
        </div>
        
        <div style="margin-top: 20px;">
            <button onclick="calculate()" id="calc-btn" style="display: none;">
                Calculate with COBOL Precision
            </button>
        </div>
        
        <div id="result" class="result"></div>
        <div id="error" class="error"></div>
    </div>
    
    <script>
    function updateForm() {
        var type = document.getElementById('calc-type').value;
        document.getElementById('loan-fields').style.display = type ? 'block' : 'none';
        document.getElementById('calc-btn').style.display = type ? 'block' : 'none';
        document.getElementById('result').style.display = 'none';
        document.getElementById('error').style.display = 'none';
    }
    
    function calculate() {
        var type = document.getElementById('calc-type').value;
        var principal = parseFloat(document.getElementById('principal').value);
        var rate = parseFloat(document.getElementById('rate').value) / 100;
        var term = parseInt(document.getElementById('term').value);
        
        document.getElementById('result').style.display = 'none';
        document.getElementById('error').style.display = 'none';
        
        // Call COBOL API
        fetch('http://localhost:3001/calculate', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({
                type: type,
                principal: principal,
                rate: rate,
                term: term
            })
        })
        .then(response => response.json())
        .then(data => {
            if (data.error) {
                document.getElementById('error').textContent = 'Error: ' + data.error;
                document.getElementById('error').style.display = 'block';
            } else {
                var html = '<h3>Calculation Results</h3>';
                if (type === 'LOAN-PAYMENT') {
                    html += '<p><strong>Monthly Payment:</strong> $' + data.payment.toFixed(2) + '</p>';
                    html += '<p><strong>Total Interest:</strong> $' + data.total_interest.toFixed(2) + '</p>';
                    html += '<p><strong>Total Payments:</strong> $' + data.total_payments.toFixed(2) + '</p>';
                }
                html += '<p style="color: #666; font-style: italic;">Calculated using COBOL COMP-3 decimal precision</p>';
                document.getElementById('result').innerHTML = html;
                document.getElementById('result').style.display = 'block';
            }
        })
        .catch(error => {
            document.getElementById('error').textContent = 'Connection error: ' + error.message;
            document.getElementById('error').style.display = 'block';
        });
    }
    </script>
</body>
</html>