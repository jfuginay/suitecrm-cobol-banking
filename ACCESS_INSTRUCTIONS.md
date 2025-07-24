# How to Access Your SuiteCRM + COBOL Integration

## 🚀 Everything is Running!

Your complete SuiteCRM + COBOL integration is now up and running. Here's how to access it:

### 1. Access SuiteCRM
Open your browser and go to: **http://localhost:8082**

You'll be redirected to the installation page. Follow these steps:

#### Installation Steps:
1. **License Agreement**: Accept the license
2. **System Environment**: All checks should pass ✅
3. **Database Configuration**:
   - Database Type: `MySQL`
   - Database Name: `suitecrm`
   - Host Name: `mysql`
   - User: `suitecrm`
   - Password: `suitecrm_pass`
   
4. **Site Configuration**:
   - Admin Username: `admin`
   - Admin Password: Choose a secure password
   - URL: `http://localhost:8082`

5. Click **Install** and wait for completion

### 2. Test COBOL Integration

Once SuiteCRM is installed, test the COBOL features:

#### A. Test API Health
```bash
curl http://localhost:3001/health
```
Expected response:
```json
{"status":"healthy","service":"COBOL Financial Services","timestamp":"2025-07-21T22:14:22.369Z"}
```

#### B. Test Financial Calculation
```bash
curl -X POST http://localhost:3001/calculate \
  -H "Content-Type: application/json" \
  -d '{
    "type": "SIMPLE-INTEREST",
    "principal": 10000,
    "rate": 0.05,
    "term": 365
  }'
```

#### C. Test WebSocket Connection
```bash
# Install wscat if needed
npm install -g wscat

# Connect to WebSocket
wscat -c ws://localhost:8081
```

### 3. Enable COBOL Modules in SuiteCRM

After installation:

1. Login to SuiteCRM as admin
2. Go to **Admin → Module Loader**
3. The COBOL modules should appear under custom modules
4. If not visible, go to **Admin → Display Modules and Subpanels**
5. Enable:
   - COBOL Bridge
   - Mainframe Sync
   - Transaction Ledger

### 4. Add Transaction Ledger to Dashboard

1. Navigate to **Home** page
2. Click **Create Dashboard** (top right)
3. Click **Add Dashlet**
4. Select **Transaction Ledger**
5. Configure settings:
   - Refresh Interval: 5 seconds
   - Account Filter: Leave empty for all
   - Transaction Types: Select all

### 5. Test Quote Calculations

1. Go to **Quotes** module
2. Create a new quote
3. Add line items
4. Notice the calculations use COBOL precision

## 🔍 Verify Everything is Working

### Check All Services:
```bash
docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
```

You should see:
- `suitecrm-app` → Port 8082
- `cobol-calculation-engine` → Ports 3001, 8081
- `suitecrm-mysql` → Port 3306
- `suitecrm-redis` → Port 6379
- `suitecrm-api-gateway` → Port 80

### View Logs:
```bash
# SuiteCRM logs
docker logs suitecrm-app

# COBOL engine logs
docker logs cobol-calculation-engine

# All logs
docker-compose logs -f
```

## 🎬 Demo Scripts

### Demo 1: Show COBOL Precision
```bash
# In one terminal, show COBOL calculations
docker exec cobol-calculation-engine ./financial-calc

# In browser, create a quote in SuiteCRM
# Show how totals match exactly
```

### Demo 2: Real-time Streaming
```bash
# Terminal 1: Connect to WebSocket
wscat -c ws://localhost:8081

# Terminal 2: Trigger transactions
curl -X POST http://localhost:3001/transaction \
  -H "Content-Type: application/json" \
  -d '{"account":"ACC123","amount":1000,"type":"DEPOSIT"}'

# Show real-time updates in Terminal 1
```

### Demo 3: Architecture Overview
```bash
# Show running containers
docker ps

# Show COBOL programs
docker exec cobol-calculation-engine ls -la *.cob

# Show API endpoints
curl http://localhost:3001/health
```

## 📝 Important URLs

- **SuiteCRM**: http://localhost:8082
- **COBOL API**: http://localhost:3001
- **API Gateway**: http://localhost:80
- **WebSocket**: ws://localhost:8081

## 🆘 Troubleshooting

### If SuiteCRM shows errors:
```bash
# Clear cache
docker exec suitecrm-app rm -rf /var/www/html/cache/*

# Fix permissions
docker exec suitecrm-app chown -R www-data:www-data /var/www/html
```

### If COBOL API isn't responding:
```bash
# Restart the service
docker-compose restart cobol-engine

# Check logs
docker logs cobol-calculation-engine
```

### If you need to start over:
```bash
# Stop everything
docker-compose down

# Remove volumes (careful - this deletes data!)
docker-compose down -v

# Start fresh
docker-compose up -d
```

## 🎉 Success!

You now have a fully functional SuiteCRM with COBOL integration running locally. This demonstrates how AI can bridge 60-year-old technology with modern systems!

For the full documentation and PR, see:
- GitHub PR: https://github.com/jfuginay/SuiteCRM/pull/1
- Full Project: https://github.com/jfuginay/suitecrm-cobol-banking