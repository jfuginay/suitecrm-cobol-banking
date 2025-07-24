-- Banking Demo Data for SuiteCRM + COBOL Integration
-- Regional Bank: First National Bank of Riverside

-- Create sample accounts (banking customers)
INSERT INTO accounts (id, name, account_type, billing_address_street, billing_address_city, billing_address_state, billing_address_postalcode, phone_office, website, annual_revenue, employees, industry, description, date_entered, date_modified, deleted, assigned_user_id) VALUES
('acc-001', 'Johnson Family Trust', 'Customer', '123 Oak Street', 'Riverside', 'CA', '92501', '951-555-0100', '', '250000', '4', 'Banking', 'High net worth family trust account. Premium banking customer since 1985.', NOW(), NOW(), 0, '1'),
('acc-002', 'Riverside Medical Group', 'Customer', '456 Health Plaza', 'Riverside', 'CA', '92502', '951-555-0200', 'www.riversidemedical.com', '5000000', '45', 'Healthcare', 'Medical practice with multiple physician partners. Needs payroll and merchant services.', NOW(), NOW(), 0, '1'),
('acc-003', 'Martinez Construction LLC', 'Customer', '789 Industrial Blvd', 'Riverside', 'CA', '92503', '951-555-0300', 'www.martinezbuilt.com', '2500000', '25', 'Construction', 'Commercial construction company. Regular equipment loans and lines of credit.', NOW(), NOW(), 0, '1'),
('acc-004', 'Sarah Chen', 'Customer', '321 Maple Avenue', 'Riverside', 'CA', '92504', '951-555-0400', '', '85000', '1', 'Banking', 'Young professional, first-time homebuyer. Excellent credit score 780+.', NOW(), NOW(), 0, '1'),
('acc-005', 'Riverside Coffee Roasters', 'Customer', '654 Main Street', 'Riverside', 'CA', '92501', '951-555-0500', 'www.riversidecoffee.com', '750000', '12', 'Food & Beverage', 'Local coffee chain with 3 locations. Needs merchant services and expansion loan.', NOW(), NOW(), 0, '1');

-- Create contacts for these accounts
INSERT INTO contacts (id, first_name, last_name, account_id, title, phone_work, phone_mobile, email1, primary_address_street, primary_address_city, primary_address_state, primary_address_postalcode, description, date_entered, date_modified, deleted, assigned_user_id) VALUES
('con-001', 'Robert', 'Johnson', 'acc-001', 'Trustee', '951-555-0100', '951-555-0101', 'rjohnson@email.com', '123 Oak Street', 'Riverside', 'CA', '92501', 'Primary trustee for Johnson Family Trust', NOW(), NOW(), 0, '1'),
('con-002', 'Dr. Amanda', 'Williams', 'acc-002', 'Managing Partner', '951-555-0200', '951-555-0201', 'awilliams@riversidemedical.com', '456 Health Plaza', 'Riverside', 'CA', '92502', 'Decision maker for all financial services', NOW(), NOW(), 0, '1'),
('con-003', 'Carlos', 'Martinez', 'acc-003', 'Owner', '951-555-0300', '951-555-0301', 'carlos@martinezbuilt.com', '789 Industrial Blvd', 'Riverside', 'CA', '92503', 'Owner and primary decision maker', NOW(), NOW(), 0, '1'),
('con-004', 'Sarah', 'Chen', 'acc-004', 'Account Holder', '951-555-0400', '951-555-0401', 'sarahchen@email.com', '321 Maple Avenue', 'Riverside', 'CA', '92504', 'Mortgage applicant, checking and savings customer', NOW(), NOW(), 0, '1'),
('con-005', 'James', 'Park', 'acc-005', 'CEO', '951-555-0500', '951-555-0501', 'james@riversidecoffee.com', '654 Main Street', 'Riverside', 'CA', '92501', 'Founder and CEO', NOW(), NOW(), 0, '1');

-- Create opportunities (loan applications, new accounts)
INSERT INTO opportunities (id, name, amount, sales_stage, probability, date_closed, account_id, description, date_entered, date_modified, deleted, assigned_user_id) VALUES
('opp-001', 'Johnson Trust - Investment Property Loan', '450000', 'Proposal/Price Quote', '75', DATE_ADD(NOW(), INTERVAL 30 DAY), 'acc-001', 'Investment property purchase loan. 20% down payment available. Excellent credit history.', NOW(), NOW(), 0, '1'),
('opp-002', 'Riverside Medical - Equipment Financing', '250000', 'Negotiation/Review', '90', DATE_ADD(NOW(), INTERVAL 15 DAY), 'acc-002', 'New MRI equipment financing. 5-year term requested.', NOW(), NOW(), 0, '1'),
('opp-003', 'Martinez Construction - Line of Credit Increase', '500000', 'Proposal/Price Quote', '60', DATE_ADD(NOW(), INTERVAL 45 DAY), 'acc-003', 'Increase business line of credit from $250k to $500k for upcoming projects.', NOW(), NOW(), 0, '1'),
('opp-004', 'Sarah Chen - First Mortgage', '400000', 'Closed Won', '100', NOW(), 'acc-004', 'First-time homebuyer mortgage. 30-year fixed at 4.5%. Closed successfully.', NOW(), NOW(), 0, '1'),
('opp-005', 'Coffee Roasters - Expansion Loan', '300000', 'Id. Decision Makers', '40', DATE_ADD(NOW(), INTERVAL 60 DAY), 'acc-005', 'Loan for fourth location. Reviewing cash flow projections.', NOW(), NOW(), 0, '1');

-- Create custom table for banking-specific data
CREATE TABLE IF NOT EXISTS banking_accounts (
    id CHAR(36) PRIMARY KEY,
    account_id CHAR(36),
    account_number VARCHAR(20),
    routing_number VARCHAR(9),
    account_type VARCHAR(20),
    balance DECIMAL(19,2),
    credit_limit DECIMAL(19,2),
    interest_rate DECIMAL(5,3),
    opened_date DATE,
    last_transaction_date DATETIME,
    fico_score INT,
    date_entered DATETIME,
    date_modified DATETIME,
    deleted TINYINT(1) DEFAULT 0,
    INDEX idx_account_id (account_id)
);

-- Insert banking account data
INSERT INTO banking_accounts VALUES
('bank-001', 'acc-001', '1234567890', '123456789', 'TRUST', 1250000.00, 0, 2.5, '1985-03-15', NOW(), 820, NOW(), NOW(), 0),
('bank-002', 'acc-002', '2345678901', '123456789', 'BUSINESS_CHECKING', 425000.00, 100000.00, 0.5, '2010-06-20', NOW(), 780, NOW(), NOW(), 0),
('bank-003', 'acc-003', '3456789012', '123456789', 'BUSINESS_LOC', 125000.00, 250000.00, 6.5, '2015-09-10', NOW(), 720, NOW(), NOW(), 0),
('bank-004', 'acc-004', '4567890123', '123456789', 'PERSONAL_CHECKING', 12500.00, 10000.00, 0.1, '2019-01-15', NOW(), 785, NOW(), NOW(), 0),
('bank-005', 'acc-005', '5678901234', '123456789', 'BUSINESS_SAVINGS', 85000.00, 0, 1.5, '2018-04-22', NOW(), 740, NOW(), NOW(), 0);

-- Create table for credit cards
CREATE TABLE IF NOT EXISTS credit_cards (
    id CHAR(36) PRIMARY KEY,
    contact_id CHAR(36),
    card_number_masked VARCHAR(20),
    card_type VARCHAR(20),
    expiry_date VARCHAR(7),
    is_valid TINYINT(1),
    validation_date DATETIME,
    date_entered DATETIME,
    date_modified DATETIME,
    deleted TINYINT(1) DEFAULT 0,
    INDEX idx_contact_id (contact_id)
);

-- Sample credit card data (masked)
INSERT INTO credit_cards VALUES
('card-001', 'con-001', '4111-XXXX-XXXX-1111', 'VISA', '12/2025', 1, NOW(), NOW(), NOW(), 0),
('card-002', 'con-002', '5500-XXXX-XXXX-4444', 'MASTERCARD', '08/2024', 1, NOW(), NOW(), NOW(), 0),
('card-003', 'con-003', '3700-XXXX-XXXX-0000', 'AMEX', '03/2026', 1, NOW(), NOW(), NOW(), 0),
('card-004', 'con-004', '4222-XXXX-XXXX-2222', 'VISA', '06/2025', 1, NOW(), NOW(), NOW(), 0),
('card-005', 'con-005', '6011-XXXX-XXXX-4444', 'DISCOVER', '10/2024', 1, NOW(), NOW(), NOW(), 0);

-- Transaction history for demo
INSERT INTO cobol_transaction_ledger (id, account_number, transaction_type, amount, balance, description, reference_number, status, posted_date, created_at) VALUES
(UUID(), '1234567890', 'DEPOSIT', 50000.00, 1250000.00, 'Wire Transfer - Investment Income', 'WT20240722001', 'POSTED', NOW(), NOW()),
(UUID(), '1234567890', 'WITHDRAWAL', -10000.00, 1240000.00, 'Property Tax Payment', 'CHK9001', 'POSTED', DATE_SUB(NOW(), INTERVAL 1 DAY), NOW()),
(UUID(), '2345678901', 'DEPOSIT', 125000.00, 425000.00, 'Insurance Reimbursements', 'DEP20240722001', 'POSTED', NOW(), NOW()),
(UUID(), '2345678901', 'WITHDRAWAL', -45000.00, 380000.00, 'Payroll - Biweekly', 'PAY20240722', 'POSTED', DATE_SUB(NOW(), INTERVAL 2 DAY), NOW()),
(UUID(), '3456789012', 'WITHDRAWAL', -25000.00, 125000.00, 'Equipment Purchase', 'LOC20240722', 'POSTED', NOW(), NOW()),
(UUID(), '4567890123', 'DEPOSIT', 5250.00, 12500.00, 'Direct Deposit - Salary', 'DD20240722', 'POSTED', NOW(), NOW()),
(UUID(), '4567890123', 'WITHDRAWAL', -1200.00, 11300.00, 'Mortgage Payment', 'AUTO20240722', 'POSTED', DATE_SUB(NOW(), INTERVAL 1 DAY), NOW()),
(UUID(), '5678901234', 'DEPOSIT', 8500.00, 85000.00, 'Daily Cash Deposits', 'CASH20240722', 'POSTED', NOW(), NOW());

-- Loan applications in progress
CREATE TABLE IF NOT EXISTS loan_applications (
    id CHAR(36) PRIMARY KEY,
    account_id CHAR(36),
    loan_type VARCHAR(50),
    requested_amount DECIMAL(19,2),
    approved_amount DECIMAL(19,2),
    interest_rate DECIMAL(5,3),
    term_months INT,
    monthly_payment DECIMAL(19,2),
    debt_to_income DECIMAL(5,2),
    status VARCHAR(20),
    underwriter_notes TEXT,
    date_entered DATETIME,
    date_modified DATETIME,
    INDEX idx_account_id (account_id)
);

INSERT INTO loan_applications VALUES
('loan-001', 'acc-001', 'INVESTMENT_PROPERTY', 450000.00, NULL, 5.25, 360, NULL, 28.5, 'UNDERWRITING', 'Strong application. Awaiting appraisal.', NOW(), NOW()),
('loan-002', 'acc-002', 'EQUIPMENT', 250000.00, 250000.00, 4.75, 60, 4684.37, 15.2, 'APPROVED', 'Approved. Excellent cash flow.', NOW(), NOW()),
('loan-003', 'acc-003', 'BUSINESS_LOC', 500000.00, NULL, 6.50, 0, NULL, 42.3, 'REVIEW', 'Reviewing recent contracts for cash flow analysis.', NOW(), NOW()),
('loan-004', 'acc-004', 'MORTGAGE', 400000.00, 400000.00, 4.50, 360, 2026.74, 28.0, 'CLOSED', 'First-time buyer. PMI required.', NOW(), NOW()),
('loan-005', 'acc-005', 'BUSINESS_EXPANSION', 300000.00, NULL, 5.75, 84, NULL, 35.5, 'APPLICATION', 'Initial application received. Pending financials.', NOW(), NOW());