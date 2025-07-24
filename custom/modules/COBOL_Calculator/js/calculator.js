/**
 * COBOL Calculator JavaScript Module
 * Handles UI interactions and AJAX calls to COBOL calculation service
 */

var COBOLCalculator = {
    
    currentType: null,
    currentResult: null,
    
    init: function() {
        console.log('COBOL Calculator initialized');
    },
    
    updateForm: function() {
        var type = $('#calculation-type').val();
        if (!type) {
            $('#calculation-form').hide();
            return;
        }
        
        this.currentType = type;
        var formHtml = this.getFormFields(type);
        $('#form-fields').html(formHtml);
        $('#calculation-form').show();
        $('#calculation-results').hide();
    },
    
    getFormFields: function(type) {
        var html = '';
        
        switch(type) {
            case 'LOAN-PAYMENT':
            case 'MORTGAGE-CALCULATOR':
                html = `
                    <div class="form-row">
                        <label>Principal Amount ($)</label>
                        <input type="number" id="principal" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>Annual Interest Rate (%)</label>
                        <input type="number" id="rate" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>Loan Term (years)</label>
                        <input type="number" id="term_years" required>
                    </div>
                    <div class="form-row">
                        <label>Payment Frequency</label>
                        <select id="frequency">
                            <option value="monthly">Monthly</option>
                            <option value="quarterly">Quarterly</option>
                            <option value="annually">Annually</option>
                        </select>
                    </div>
                `;
                break;
                
            case 'COMPOUND-INTEREST':
                html = `
                    <div class="form-row">
                        <label>Principal Amount ($)</label>
                        <input type="number" id="principal" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>Annual Interest Rate (%)</label>
                        <input type="number" id="rate" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>Investment Period (years)</label>
                        <input type="number" id="years" required>
                    </div>
                    <div class="form-row">
                        <label>Compound Frequency</label>
                        <select id="compound_frequency">
                            <option value="monthly">Monthly</option>
                            <option value="quarterly">Quarterly</option>
                            <option value="annually">Annually</option>
                        </select>
                    </div>
                `;
                break;
                
            case 'CURRENCY-CONVERSION':
                html = `
                    <div class="form-row">
                        <label>Amount</label>
                        <input type="number" id="amount" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>From Currency</label>
                        <select id="from_currency">
                            <option value="USD">USD</option>
                            <option value="EUR">EUR</option>
                            <option value="GBP">GBP</option>
                            <option value="JPY">JPY</option>
                            <option value="CAD">CAD</option>
                        </select>
                    </div>
                    <div class="form-row">
                        <label>To Currency</label>
                        <select id="to_currency">
                            <option value="EUR">EUR</option>
                            <option value="USD">USD</option>
                            <option value="GBP">GBP</option>
                            <option value="JPY">JPY</option>
                            <option value="CAD">CAD</option>
                        </select>
                    </div>
                `;
                break;
                
            case 'RISK-ASSESSMENT':
                html = `
                    <div class="form-row">
                        <label>Annual Income ($)</label>
                        <input type="number" id="income" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>Total Assets ($)</label>
                        <input type="number" id="assets" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>Total Liabilities ($)</label>
                        <input type="number" id="liabilities" step="0.01" required>
                    </div>
                    <div class="form-row">
                        <label>Credit Score</label>
                        <input type="number" id="credit_score" min="300" max="850" required>
                    </div>
                    <div class="form-row">
                        <label>Investment Experience (years)</label>
                        <input type="number" id="experience" min="0" required>
                    </div>
                `;
                break;
                
            default:
                html = '<p>Form fields for this calculation type are being developed.</p>';
        }
        
        return html;
    },
    
    calculate: function() {
        var params = this.collectParameters();
        if (!params) return;
        
        // Show loading
        $('#calculation-results').show();
        $('#results-content').html('<div class="loading">Calculating...</div>');
        
        // Make AJAX call
        $.ajax({
            url: 'index.php?module=COBOL_Calculator&action=calculate',
            type: 'POST',
            data: {
                type: this.currentType,
                parameters: JSON.stringify(params)
            },
            success: function(response) {
                COBOLCalculator.displayResults(response);
            },
            error: function(xhr, status, error) {
                COBOLCalculator.displayError('Calculation failed: ' + error);
            }
        });
    },
    
    collectParameters: function() {
        var params = {};
        var valid = true;
        
        $('#form-fields input, #form-fields select').each(function() {
            var value = $(this).val();
            if ($(this).prop('required') && !value) {
                alert('Please fill in all required fields');
                valid = false;
                return false;
            }
            params[$(this).attr('id')] = value;
        });
        
        return valid ? params : null;
    },
    
    displayResults: function(response) {
        var data = typeof response === 'string' ? JSON.parse(response) : response;
        
        if (!data.success) {
            this.displayError(data.error || 'Calculation failed');
            return;
        }
        
        this.currentResult = data.result;
        var html = '<div class="result-success">';
        
        // Display based on calculation type
        switch(this.currentType) {
            case 'LOAN-PAYMENT':
            case 'MORTGAGE-CALCULATOR':
                html += `
                    <div class="result-item">
                        <label>Monthly Payment:</label>
                        <span class="value">$${this.formatNumber(data.result.monthly_payment)}</span>
                    </div>
                    <div class="result-item">
                        <label>Total Interest:</label>
                        <span class="value">$${this.formatNumber(data.result.total_interest)}</span>
                    </div>
                    <div class="result-item">
                        <label>Total Payment:</label>
                        <span class="value">$${this.formatNumber(data.result.total_payment)}</span>
                    </div>
                `;
                break;
                
            case 'CURRENCY-CONVERSION':
                html += `
                    <div class="result-item">
                        <label>Exchange Rate:</label>
                        <span class="value">${data.result.exchange_rate}</span>
                    </div>
                    <div class="result-item">
                        <label>Converted Amount:</label>
                        <span class="value">${data.result.to_currency} ${this.formatNumber(data.result.converted_amount)}</span>
                    </div>
                `;
                break;
                
            case 'RISK-ASSESSMENT':
                html += `
                    <div class="result-item">
                        <label>Risk Score:</label>
                        <span class="value">${data.result.risk_score}/100</span>
                    </div>
                    <div class="result-item">
                        <label>Risk Level:</label>
                        <span class="value risk-${data.result.risk_level.toLowerCase()}">${data.result.risk_level}</span>
                    </div>
                    <div class="result-item">
                        <label>Recommendation:</label>
                        <span class="value">${data.result.recommendation}</span>
                    </div>
                `;
                break;
                
            default:
                // Generic result display
                for (var key in data.result) {
                    html += `
                        <div class="result-item">
                            <label>${this.formatLabel(key)}:</label>
                            <span class="value">${this.formatValue(data.result[key])}</span>
                        </div>
                    `;
                }
        }
        
        html += `
            <div class="result-footer">
                <small>Calculated in ${data.execution_time.toFixed(3)} seconds using COBOL precision engine</small>
            </div>
        </div>`;
        
        $('#results-content').html(html);
    },
    
    displayError: function(error) {
        $('#results-content').html(
            '<div class="result-error">' +
            '<h4>Calculation Error</h4>' +
            '<p>' + error + '</p>' +
            '</div>'
        );
    },
    
    formatNumber: function(num) {
        return parseFloat(num).toFixed(2).replace(/\B(?=(\d{3})+(?!\d))/g, ",");
    },
    
    formatLabel: function(key) {
        return key.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase());
    },
    
    formatValue: function(value) {
        if (typeof value === 'number') {
            return this.formatNumber(value);
        }
        return value;
    },
    
    clear: function() {
        $('#form-fields input').val('');
        $('#calculation-results').hide();
        this.currentResult = null;
    },
    
    saveCalculation: function() {
        if (!this.currentResult) {
            alert('No calculation to save');
            return;
        }
        
        var name = prompt('Enter a name for this calculation:');
        if (!name) return;
        
        $.ajax({
            url: 'index.php?module=COBOL_Calculator&action=save',
            type: 'POST',
            data: {
                name: name,
                type: this.currentType,
                result: JSON.stringify(this.currentResult)
            },
            success: function(response) {
                alert('Calculation saved successfully');
                COBOLCalculator.loadHistory();
            },
            error: function() {
                alert('Failed to save calculation');
            }
        });
    },
    
    exportResults: function() {
        if (!this.currentResult) {
            alert('No results to export');
            return;
        }
        
        // Create CSV content
        var csv = 'Field,Value\n';
        for (var key in this.currentResult) {
            csv += '"' + this.formatLabel(key) + '","' + this.currentResult[key] + '"\n';
        }
        
        // Download CSV
        var blob = new Blob([csv], { type: 'text/csv' });
        var url = window.URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = 'cobol_calculation_' + this.currentType + '_' + new Date().getTime() + '.csv';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        window.URL.revokeObjectURL(url);
    },
    
    loadHistory: function() {
        $.ajax({
            url: 'index.php?module=COBOL_Calculator&action=history',
            type: 'GET',
            success: function(response) {
                COBOLCalculator.displayHistory(response);
            }
        });
    },
    
    displayHistory: function(response) {
        var data = typeof response === 'string' ? JSON.parse(response) : response;
        var html = '';
        
        if (data.history && data.history.length > 0) {
            data.history.forEach(function(item) {
                html += `
                    <tr>
                        <td>${item.date}</td>
                        <td>${item.type}</td>
                        <td><span class="status-${item.status}">${item.status}</span></td>
                        <td>${item.execution_time}s</td>
                        <td>
                            <a href="#" onclick="COBOLCalculator.viewCalculation('${item.id}')">View</a>
                        </td>
                    </tr>
                `;
            });
        } else {
            html = '<tr><td colspan="5">No calculation history found</td></tr>';
        }
        
        $('#history-tbody').html(html);
    },
    
    viewCalculation: function(id) {
        window.location.href = 'index.php?module=COBOL_Calculator&action=DetailView&record=' + id;
    }
};