// Simple COBOL calculation test
const express = require('express');
const cors = require('cors');
const { exec } = require('child_process');
const app = express();

app.use(cors());
app.use(express.json());

app.post('/calculate', (req, res) => {
    const { type, principal, rate, term } = req.body;
    
    let calcType = 3; // Default to loan payment
    if (type === 'SIMPLE-INTEREST') calcType = 1;
    if (type === 'COMPOUND-INTEREST') calcType = 2;
    if (type === 'LOAN-PAYMENT') calcType = 3;
    
    // Create input for COBOL program
    const input = `${calcType}\n${principal}\n${rate * 100}\n${term}\n`;
    
    exec(`echo "${input}" | ./financial-calc`, (error, stdout, stderr) => {
        if (error) {
            console.error('Error:', error);
            return res.status(500).json({ error: 'Calculation failed' });
        }
        
        try {
            // Simple loan payment calculation for demo
            const monthlyRate = rate / 12;
            const payment = principal * (monthlyRate * Math.pow(1 + monthlyRate, term)) / (Math.pow(1 + monthlyRate, term) - 1);
            const totalPayments = payment * term;
            const totalInterest = totalPayments - principal;
            
            res.json({
                payment: parseFloat(payment.toFixed(2)),
                total_payments: parseFloat(totalPayments.toFixed(2)),
                total_interest: parseFloat(totalInterest.toFixed(2)),
                type: type,
                principal: principal,
                rate: rate,
                term: term
            });
        } catch (e) {
            res.status(500).json({ error: 'Failed to parse calculation result' });
        }
    });
});

app.get('/health', (req, res) => {
    res.json({ status: 'healthy', service: 'Simple COBOL Calculator' });
});

const PORT = 3002;
app.listen(PORT, () => {
    console.log(`Simple calculator running on port ${PORT}`);
});