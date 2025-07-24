# SuiteCRM COBOL Service API Documentation

## Overview

The SuiteCRM COBOL Service API provides programmatic access to all COBOL-powered features including financial calculations, data validation, legacy system integration, reporting, and batch processing.

## Base URL

```
https://your-suitecrm-instance.com/api/v1/cobol
```

## Authentication

All API requests require authentication using OAuth 2.0 or API key.

### OAuth 2.0
```http
Authorization: Bearer YOUR_ACCESS_TOKEN
```

### API Key
```http
X-API-Key: YOUR_API_KEY
```

## REST API Endpoints

### 1. Financial Calculations

#### POST /api/v1/cobol/calculate

Execute COBOL-powered financial calculations with banking precision.

**Request Body:**
```json
{
  "type": "LOAN-PAYMENT",
  "parameters": {
    "principal": 250000,
    "rate": 0.045,
    "term_years": 30,
    "frequency": "monthly"
  },
  "currency": "USD"
}
```

**Response:**
```json
{
  "success": true,
  "calculation_id": "calc_123456",
  "result": {
    "monthly_payment": 1266.71,
    "total_interest": 206015.60,
    "total_payment": 456015.60,
    "amortization_schedule": [...]
  },
  "execution_time": 0.023
}
```

**Calculation Types:**
- `LOAN-PAYMENT`: Loan payment calculation
- `COMPOUND-INTEREST`: Compound interest calculation
- `AMORTIZATION`: Full amortization schedule
- `ROI-CALCULATOR`: Return on investment
- `MORTGAGE-CALCULATOR`: Mortgage calculations
- `CURRENCY-CONVERSION`: Currency exchange
- `RISK-ASSESSMENT`: Risk scoring
- `TAX-CALCULATION`: Tax calculations

### 2. Data Validation

#### POST /api/v1/cobol/validate

Validate field values using COBOL business rules engine.

**Request Body:**
```json
{
  "module": "Accounts",
  "field": "account_number",
  "value": "1234567890",
  "record": {
    "name": "Acme Corp",
    "account_type": "Business"
  }
}
```

**Response:**
```json
{
  "valid": true,
  "errors": [],
  "warnings": []
}
```

### 3. Legacy Data Import

#### POST /api/v1/cobol/import

Import data from mainframe systems with EBCDIC conversion.

**Request Body:**
```json
{
  "bridge_type": "CUSTOMER-MASTER",
  "data": "base64_encoded_ebcdic_data",
  "source_format": "FIXED_LENGTH",
  "copybook": "01 CUSTOMER-RECORD...",
  "field_mappings": {
    "CUST-ID": "account_number",
    "CUST-NAME": "name"
  },
  "options": {
    "encoding": "EBCDIC",
    "record_length": 500
  }
}
```

**Response:**
```json
{
  "success": true,
  "bridge_id": "bridge_123456",
  "records_processed": 1000,
  "records_failed": 3,
  "details": []
}
```

### 4. Legacy Data Export

#### POST /api/v1/cobol/export

Export SuiteCRM data to legacy mainframe format.

**Request Body:**
```json
{
  "module": "Accounts",
  "bridge_type": "CUSTOMER-MASTER",
  "target_format": "FIXED_LENGTH",
  "filters": {
    "account_type": "Customer"
  },
  "field_mappings": {
    "account_number": "CUST-ID",
    "name": "CUST-NAME"
  }
}
```

**Response:**
```json
{
  "success": true,
  "data": "base64_encoded_ebcdic_data",
  "format": "FIXED_LENGTH",
  "encoding": "EBCDIC",
  "records_exported": 500,
  "file_size": 250000
}
```

### 5. Report Generation

#### POST /api/v1/cobol/report

Generate reports using COBOL processing engine.

**Request Body:**
```json
{
  "report_type": "FINANCIAL_SUMMARY",
  "format": "pdf",
  "parameters": {
    "start_date": "2024-01-01",
    "end_date": "2024-12-31",
    "include_charts": true
  },
  "return_data": false
}
```

**Response:**
```json
{
  "success": true,
  "report_id": "report_123456",
  "download_url": "https://your-instance.com/api/v1/cobol/report/download/report_123456",
  "execution_time": 2.456
}
```

### 6. Batch Job Management

#### POST /api/v1/cobol/batch/submit

Submit batch job for processing.

**Request Body:**
```json
{
  "job_type": "DATA_RECONCILIATION",
  "name": "Daily Reconciliation",
  "parameters": {
    "source": "mainframe",
    "target": "crm"
  },
  "priority": 5
}
```

**Response:**
```json
{
  "success": true,
  "job_id": "job_123456",
  "queue_position": 3,
  "estimated_start": "2024-01-20T14:30:00Z"
}
```

#### GET /api/v1/cobol/batch/status/{job_id}

Get batch job status.

**Response:**
```json
{
  "job_id": "job_123456",
  "name": "Daily Reconciliation",
  "type": "DATA_RECONCILIATION",
  "status": "running",
  "progress": 45,
  "last_run": "2024-01-20T14:30:00Z",
  "run_count": 10,
  "success_count": 9,
  "failure_count": 1,
  "average_runtime": 324.5
}
```

### 7. Health Check

#### GET /api/v1/cobol/health

Check service health status.

**Response:**
```json
{
  "status": "healthy",
  "services": {
    "cobol_api": {
      "status": "healthy",
      "response_time": 0.023
    },
    "database": {
      "status": "healthy"
    },
    "file_system": {
      "status": "healthy"
    }
  },
  "timestamp": "2024-01-20T15:00:00Z"
}
```

## GraphQL API

The GraphQL API provides a modern, flexible interface to COBOL services.

### Endpoint

```
POST https://your-suitecrm-instance.com/api/graphql
```

### Example Queries

#### Financial Calculation
```graphql
mutation Calculate {
  executeCalculation(input: {
    type: LOAN_PAYMENT
    parameters: {
      principal: 250000
      rate: 0.045
      term_years: 30
    }
  }) {
    success
    result
    executionTime
  }
}
```

#### Batch Job Monitoring
```graphql
query MonitorJobs {
  listBatchJobs(filter: { status: RUNNING }) {
    items {
      id
      name
      type
      progress
      startedAt
    }
  }
  getJobQueueStatus {
    running
    queued
    capacityUsed
  }
}
```

#### Real-time Subscriptions
```graphql
subscription TransactionUpdates {
  transactionAdded {
    id
    timestamp
    type
    amount
    account
  }
}
```

## WebSocket API

For real-time features, connect to the WebSocket endpoint.

### Connection

```javascript
const ws = new WebSocket('wss://your-instance.com/api/ws/cobol');

ws.on('open', () => {
  ws.send(JSON.stringify({
    action: 'subscribe',
    channels: ['transactions', 'validations', 'jobs']
  }));
});

ws.on('message', (data) => {
  const event = JSON.parse(data);
  console.log('Event:', event);
});
```

### Event Types

- `transaction.created`: New transaction processed
- `validation.failed`: Validation failure occurred
- `job.status_changed`: Batch job status update
- `report.completed`: Report generation finished

## Error Handling

All API endpoints use standard HTTP status codes and return detailed error messages.

### Error Response Format

```json
{
  "error": {
    "code": "VALIDATION_FAILED",
    "message": "Account number validation failed",
    "details": {
      "field": "account_number",
      "value": "invalid",
      "rules": ["ACCOUNT-VALIDATOR"]
    }
  }
}
```

### Common Error Codes

- `400 Bad Request`: Invalid request parameters
- `401 Unauthorized`: Authentication required
- `403 Forbidden`: Insufficient permissions
- `404 Not Found`: Resource not found
- `429 Too Many Requests`: Rate limit exceeded
- `500 Internal Server Error`: Server error

## Rate Limiting

API requests are rate limited to ensure service stability.

- **Default limit**: 1000 requests per hour
- **Batch operations**: 100 requests per hour
- **Real-time endpoints**: 10,000 requests per hour

Rate limit headers:
```http
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 950
X-RateLimit-Reset: 1642684800
```

## SDK Examples

### JavaScript/Node.js

```javascript
const SuiteCRMCobol = require('@suitecrm/cobol-sdk');

const client = new SuiteCRMCobol({
  baseUrl: 'https://your-instance.com',
  apiKey: 'YOUR_API_KEY'
});

// Calculate loan payment
const result = await client.calculate({
  type: 'LOAN-PAYMENT',
  parameters: {
    principal: 250000,
    rate: 0.045,
    term_years: 30
  }
});

console.log('Monthly payment:', result.monthly_payment);
```

### Python

```python
from suitecrm_cobol import Client

client = Client(
    base_url='https://your-instance.com',
    api_key='YOUR_API_KEY'
)

# Import legacy data
result = client.import_legacy_data(
    bridge_type='CUSTOMER-MASTER',
    data=ebcdic_data,
    copybook=copybook_definition
)

print(f"Imported {result['records_processed']} records")
```

### PHP

```php
use SuiteCRM\COBOL\Client;

$client = new Client([
    'base_url' => 'https://your-instance.com',
    'api_key' => 'YOUR_API_KEY'
]);

// Generate report
$report = $client->generateReport([
    'report_type' => 'FINANCIAL_SUMMARY',
    'format' => 'pdf',
    'parameters' => [
        'start_date' => '2024-01-01',
        'end_date' => '2024-12-31'
    ]
]);

echo "Report available at: " . $report['download_url'];
```

## Webhooks

Configure webhooks to receive real-time notifications.

### Webhook Configuration

```json
POST /api/v1/cobol/webhooks
{
  "url": "https://your-app.com/webhook",
  "events": [
    "calculation.completed",
    "import.finished",
    "job.failed",
    "validation.error"
  ],
  "secret": "your_webhook_secret"
}
```

### Webhook Payload

```json
{
  "event": "calculation.completed",
  "timestamp": "2024-01-20T15:30:00Z",
  "data": {
    "calculation_id": "calc_123456",
    "type": "LOAN-PAYMENT",
    "result": {...}
  },
  "signature": "sha256=..."
}
```

## Testing

Use the sandbox environment for testing:

- **Base URL**: `https://sandbox.your-instance.com/api/v1/cobol`
- **Test API Key**: `test_api_key_123456`

## Support

- **Documentation**: https://docs.suitecrm-cobol.com
- **API Status**: https://status.suitecrm-cobol.com
- **Support**: api-support@suitecrm-cobol.com

## Changelog

### v1.0.0 (2024-01-20)
- Initial release
- REST API endpoints
- GraphQL API
- WebSocket support
- SDK libraries