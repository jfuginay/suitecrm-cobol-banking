# SuiteCRM + COBOL Demo Setup Guide

## 🚀 Quick Setup for Demo Videos

### Step 1: Access SuiteCRM Installation
Open your browser and go to: **http://localhost:8082**

You'll be redirected to the installation wizard.

### Step 2: Installation Wizard

#### Screen 1: License Agreement
- Click **"I Accept"**
- Click **"Next"**

#### Screen 2: System Check
All items should show green checkmarks. Click **"Next"**

#### Screen 3: Database Configuration
Enter these exact values:
- **Database Type**: MySQL
- **Database Name**: `suitecrm`
- **Host Name**: `mysql` (NOT localhost!)
- **User**: `suitecrm`
- **Password**: `suitecrm_pass`

Click **"Next"**

#### Screen 4: Site Configuration
- **Admin Username**: `admin`
- **Admin Password**: `admin123` (or your choice)
- **Re-enter Password**: (same as above)
- **URL**: `http://localhost:8082`
- **Email**: `admin@demo.com`
- **Name**: `Demo Admin`

Leave other settings as default. Click **"Next"**

#### Screen 5: Confirm Settings
Review and click **"Install"**

#### Screen 6: Installation Progress
Wait 2-3 minutes for installation to complete.

### Step 3: First Login
1. Click **"Next"** after installation completes
2. Login with:
   - Username: `admin`
   - Password: `admin123` (or what you chose)

### Step 4: Enable COBOL Modules

#### Option A: Quick Manual Setup
1. Navigate to **Admin** (top right)
2. Click **"Display Modules and Subpanels"**
3. Find and move these to "Displayed Modules":
   - COBOL Bridge
   - Mainframe Sync
   - Transaction Ledger
4. Click **"Save"**

#### Option B: Direct Database Update (Faster)
Run this in a new terminal:
```bash
docker exec -it suitecrm-mysql mysql -usuitecrm -psuitecrm_pass suitecrm -e "
INSERT INTO config (category, name, value) VALUES 
('cobol', 'cobol_integration_enabled', '1'),
('cobol', 'cobol_api_url', 'http://cobol-engine:3000'),
('cobol', 'cobol_websocket_url', 'ws://localhost:8081')
ON DUPLICATE KEY UPDATE value=VALUES(value);"
```

### Step 5: Clear Cache
```bash
docker exec suitecrm-app rm -rf cache/*
docker exec suitecrm-app php repair.php
```

### Step 6: Access COBOL Features

After login, you should see in the top navigation:
- **COBOL Banking** (main menu)

Click on it to see:
- Financial Calculator
- Mainframe Sync
- Transaction Ledger
- Batch Processing

## 📹 Demo Recording Tips

### 1. Financial Calculator Demo
1. Navigate to **COBOL Banking → Financial Calculator**
2. Select "Loan Payment" from dropdown
3. Enter:
   - Principal: $250,000
   - Rate: 4.5%
   - Term: 360 months
4. Click "Calculate with COBOL Precision"
5. Show the result: Monthly payment calculated by COBOL

### 2. Transaction Ledger Demo
1. Navigate to **COBOL Banking → Transaction Ledger**
2. Click **"Connect Live Feed"**
3. Watch transactions appear in real-time
4. Use filters to show specific accounts
5. Click "Export" to download CSV

### 3. Mainframe Sync Demo
1. Navigate to **COBOL Banking → Mainframe Sync**
2. Click **"Sync All Accounts"**
3. Watch the progress bar
4. Show the sync history table
5. Point out the sample account cards

### 4. Batch Processing Demo
1. Navigate to **COBOL Banking → Batch Processing**
2. Click on "Daily Interest Calculation"
3. Configure the date
4. Click "Run Batch Job"
5. Watch the COBOL processing logs

## 🎯 Key Points to Emphasize

1. **It's Real COBOL**: Show that actual COBOL programs are running
2. **Inside SuiteCRM**: Everything is accessed through the CRM interface
3. **Banking Precision**: No floating-point errors in calculations
4. **Live Data**: WebSocket streaming for real-time updates
5. **Enterprise Ready**: Batch processing, audit logs, etc.

## 🛠️ Troubleshooting

### If modules don't appear:
```bash
# Rebuild module cache
docker exec suitecrm-app php repair.php

# Check module registration
docker exec suitecrm-app ls -la custom/modules/
```

### If COBOL service isn't responding:
```bash
# Check service health
curl http://localhost:3001/health

# View COBOL logs
docker logs cobol-calculation-engine
```

### If you need to start over:
```bash
# Reset database
docker exec suitecrm-mysql mysql -uroot -proot_pass -e "DROP DATABASE suitecrm; CREATE DATABASE suitecrm;"

# Restart containers
docker-compose restart
```

## 📸 Screenshots to Capture

1. **Login Screen** - Shows it's real SuiteCRM
2. **COBOL Banking Menu** - Proves integration
3. **Calculator in Action** - COBOL calculations
4. **Live Transactions** - Real-time streaming
5. **Sync Progress** - Mainframe integration
6. **Batch Processing** - Enterprise features

---

Ready to record! The system is running and waiting for you at **http://localhost:8082** 🎬