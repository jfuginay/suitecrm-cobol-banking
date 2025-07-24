# SuiteCRM COBOL Integration - User Guide

## Table of Contents
1. [Overview](#overview)
2. [Getting Started](#getting-started)
3. [COBOL Calculator Module](#cobol-calculator-module)
4. [Legacy Bridge Module](#legacy-bridge-module)
5. [Installation Guide](#installation-guide)
6. [Troubleshooting](#troubleshooting)
7. [API Reference](#api-reference)

## Overview

The SuiteCRM COBOL Integration brings mainframe-grade financial calculations and legacy data import/export capabilities to your CRM system. This guide covers the two main features currently implemented:

- **COBOL Calculator**: Banking-precision financial calculations
- **Legacy Bridge**: Import/export data from mainframe systems

## Getting Started

### Prerequisites
- SuiteCRM 7.14.6 or higher
- Docker and Docker Compose
- Node.js 16+ (for development)
- GnuCOBOL (included in Docker images)

### Quick Start with Docker

```bash
# Clone the repository
git clone [repository-url]
cd suitecrm-cobol

# Start all services
docker-compose up -d

# Access SuiteCRM
# URL: http://localhost:8080
# Username: admin
# Password: admin
```

### Service URLs
- **SuiteCRM**: http://localhost:8080
- **COBOL API**: http://localhost:3000
- **WebSocket Server**: ws://localhost:8080

## COBOL Calculator Module

The COBOL Calculator provides banking-grade precision for financial calculations directly within SuiteCRM.

### Accessing the Calculator

1. Log into SuiteCRM
2. Navigate to **COBOL Calculator** from the main menu
3. Click **Financial Calculator** to open the calculator interface

### Available Calculations

#### 1. Loan Payment Calculator
Calculate monthly payments for loans with compound interest.

**Input Fields:**
- Principal Amount ($)
- Annual Interest Rate (%)
- Loan Term (years)
- Payment Frequency

**Example:**
```
Principal: $100,000
Interest Rate: 5.5%
Term: 30 years
Result: Monthly Payment = $567.79
```

#### 2. Compound Interest Calculator
Calculate future value of investments with compound interest.

**Input Fields:**
- Principal Amount ($)
- Annual Interest Rate (%)
- Investment Period (years)
- Compound Frequency

#### 3. Currency Conversion
Convert between major currencies using COBOL precision.

**Supported Currencies:**
- USD, EUR, GBP, JPY, CAD

#### 4. Risk Assessment
Evaluate financial risk based on multiple factors.

**Input Fields:**
- Annual Income
- Total Assets
- Total Liabilities
- Credit Score (300-850)
- Investment Experience (years)

**Output:**
- Risk Score (0-100)
- Risk Level (Low/Medium/High)
- Recommendation

### Saving and Exporting Calculations

1. After performing a calculation, click **Save Calculation**
2. Enter a descriptive name
3. View saved calculations in the History panel
4. Export results as CSV using the **Export Results** button

### Viewing Calculation History

1. Navigate to **COBOL Calculator** → **View List**
2. See all past calculations with:
   - Date/time
   - Calculation type
   - Status
   - Execution time
   - Results

## Legacy Bridge Module

The Legacy Bridge enables import/export of data between SuiteCRM and mainframe systems.

### Import Wizard

The import process is divided into 4 steps:

#### Step 1: Select Source
1. Navigate to **Legacy Bridge** → **Import Data**
2. Select:
   - **Bridge Type**: Customer Master, Account Ledger, etc.
   - **Source System**: IBM Mainframe, AS/400, etc.
   - **File Format**: Fixed Length, Variable Length, VSAM
   - **Character Encoding**: EBCDIC, ASCII, UTF-8
3. Upload your legacy data file (.dat, .txt, .ebcdic)

#### Step 2: Define Format
Define the structure of your legacy data using COBOL copybook:

**Option 1: Upload Copybook File**
- Upload a .cpy or .cob file containing the record structure

**Option 2: Enter Manually**
```cobol
       01  CUSTOMER-RECORD.
           05  CUST-ID         PIC X(10).
           05  CUST-NAME       PIC X(30).
           05  CUST-BALANCE    PIC S9(9)V99 COMP-3.
```

**Option 3: Use Template**
- Select from pre-defined templates for common formats

Click **Parse Copybook** to validate and preview the structure.

#### Step 3: Map Fields
1. Select target SuiteCRM module (Accounts, Contacts, etc.)
2. Map legacy fields to SuiteCRM fields
3. Apply transformations:
   - Uppercase/Lowercase
   - Trim spaces
   - Date formatting
   - Currency formatting

#### Step 4: Review & Import
1. Review import summary
2. Preview first 5 records
3. Set import options:
   - ✓ Validate data before import
   - ✓ Check for duplicates
   - ✓ Create backup
4. Click **Start Import**

### Export Process

1. Navigate to **Legacy Bridge** → **Export Data**
2. Select:
   - Module to export
   - Target format (matching your mainframe)
   - Field mappings
3. Configure transformation rules
4. Execute export
5. Download the generated EBCDIC/fixed-length file

### Supported Legacy Formats

- **Customer Master File**: Standard customer records
- **Account Ledger**: Financial account information
- **Transaction Log**: Transaction history
- **Product Catalog**: Product listings
- **Custom Format**: Define your own structure

## Installation Guide

### Manual Installation

1. **Database Setup**
```sql
-- Run the SQL scripts
mysql -u root -p suitecrm < custom/modules/COBOL_Calculator/sql/create_table.sql
mysql -u root -p suitecrm < custom/modules/Legacy_Bridge/sql/create_table.sql
```

2. **Copy Module Files**
```bash
# Copy custom modules
cp -r custom/modules/COBOL_Calculator /path/to/suitecrm/custom/modules/
cp -r custom/modules/Legacy_Bridge /path/to/suitecrm/custom/modules/
```

3. **Start COBOL Services**
```bash
cd cobol-services
npm install
./compile-cobol.sh
./start-servers.sh
```

4. **Configure SuiteCRM**
Add to `config_override.php`:
```php
$sugar_config['cobol_api_url'] = 'http://localhost:3000';
```

5. **Repair and Rebuild**
- Admin → Repair → Quick Repair and Rebuild
- Admin → Repair → Rebuild Relationships

### Module Loader Installation

1. Package the module:
```bash
zip -r COBOL_Integration.zip manifest.php custom/ cobol-services/
```

2. In SuiteCRM Admin:
   - Navigate to **Admin** → **Module Loader**
   - Upload COBOL_Integration.zip
   - Install the package

## Troubleshooting

### Common Issues

#### COBOL Service Not Responding
```bash
# Check if services are running
docker ps

# View logs
docker-compose logs cobol-engine

# Restart services
docker-compose restart
```

#### Calculation Errors
1. Verify COBOL API URL in config
2. Check Node.js server logs:
```bash
docker logs suitecrm-cobol_api-gateway_1
```

#### Import Failures
1. Verify copybook syntax
2. Check file encoding matches selection
3. Review error logs in Legacy Bridge record

#### Permission Issues
```bash
# Fix permissions
chmod -R 755 custom/modules/COBOL_Calculator
chmod -R 755 custom/modules/Legacy_Bridge
chown -R www-data:www-data custom/
```

## API Reference

### COBOL Calculation API

**Endpoint**: `POST /calculate`

**Request**:
```json
{
  "type": "LOAN-PAYMENT",
  "parameters": {
    "principal": 100000,
    "rate": 0.055,
    "term_years": 30,
    "frequency": "monthly"
  }
}
```

**Response**:
```json
{
  "success": true,
  "result": {
    "monthly_payment": 567.79,
    "total_interest": 104404.40,
    "total_payment": 204404.40
  },
  "execution_time": 0.023
}
```

### Legacy Import API

**Endpoint**: `POST /legacy/import`

**Request**:
```json
{
  "bridge_type": "CUSTOMER-MASTER",
  "source_format": "FIXED_LENGTH",
  "copybook": "...",
  "data": "base64_encoded_data",
  "encoding": "EBCDIC",
  "field_mappings": {}
}
```

### Available Calculation Types

- `LOAN-PAYMENT`: Loan payment calculation
- `COMPOUND-INTEREST`: Compound interest
- `AMORTIZATION`: Amortization schedule
- `ROI-CALCULATOR`: Return on investment
- `MORTGAGE-CALCULATOR`: Mortgage calculations
- `SAVINGS-GOAL`: Savings goal planning
- `RETIREMENT-PLANNING`: Retirement calculations
- `RISK-ASSESSMENT`: Risk scoring
- `CURRENCY-CONVERSION`: Currency exchange
- `TAX-CALCULATION`: Tax calculations

## Best Practices

1. **Data Validation**: Always validate copybook definitions before import
2. **Backup**: Create backups before large imports
3. **Testing**: Test with small data sets first
4. **Monitoring**: Check execution logs for performance metrics
5. **Security**: Use SSL for production deployments

## Support

For issues or questions:
1. Check the troubleshooting section
2. Review logs in `docker-compose logs`
3. Consult the technical documentation in `/docs`