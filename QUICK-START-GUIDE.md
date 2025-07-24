# SuiteCRM COBOL Integration - Quick Start Guide

## 🚀 5-Minute Setup

### Prerequisites Check
```bash
# Check Docker
docker --version  # Need 20.10+

# Check Docker Compose
docker-compose --version  # Need 1.29+

# Check available ports
lsof -i :8080  # Should be free
lsof -i :3000  # Should be free
```

### Step 1: Clone and Start
```bash
# Clone the repository
git clone [your-repo-url] suitecrm-cobol
cd suitecrm-cobol

# Start everything with Docker
docker-compose up -d

# Wait for services (about 2-3 minutes)
docker-compose logs -f
# Press Ctrl+C when you see "SuiteCRM is ready"
```

### Step 2: Access SuiteCRM
1. Open browser: http://localhost:8080
2. Login:
   - Username: `admin`
   - Password: `admin`

### Step 3: Try COBOL Calculator
1. Click **COBOL Calculator** in top menu
2. Click **Financial Calculator**
3. Try a loan calculation:
   - Select: "Loan Payment Calculator"
   - Principal: 250000
   - Interest Rate: 4.5
   - Term: 30 years
   - Click **Calculate**

### Step 4: Import Legacy Data (Demo)
1. Click **Legacy Bridge** in top menu
2. Click **Import Data**
3. Use the sample data:
   - Bridge Type: "Customer Master File"
   - Source System: "IBM Mainframe"
   - Upload: Use any .txt file as demo
   - Follow the wizard steps

## 🎯 Quick Test Scenarios

### Test 1: Mortgage Calculation
```
Purpose: Calculate monthly mortgage payment
Module: COBOL Calculator
Type: Mortgage Calculator
Inputs:
  - Principal: $350,000
  - Rate: 3.75%
  - Term: 30 years
Expected: Monthly payment ~$1,620
```

### Test 2: Currency Conversion
```
Purpose: Convert USD to EUR
Module: COBOL Calculator
Type: Currency Conversion
Inputs:
  - Amount: $1,000
  - From: USD
  - To: EUR
Expected: ~€850 (varies by rate)
```

### Test 3: Risk Assessment
```
Purpose: Evaluate investment risk
Module: COBOL Calculator
Type: Risk Assessment
Inputs:
  - Income: $75,000
  - Assets: $250,000
  - Liabilities: $100,000
  - Credit Score: 750
Expected: Low Risk rating
```

## 🛠️ Common Commands

### Service Management
```bash
# Stop all services
docker-compose down

# Restart services
docker-compose restart

# View logs
docker-compose logs -f [service-name]
# Services: suitecrm, cobol-engine, api-gateway, mysql

# Reset everything
docker-compose down -v
docker-compose up -d
```

### Direct API Testing
```bash
# Test COBOL calculation API
curl -X POST http://localhost:3000/calculate \
  -H "Content-Type: application/json" \
  -d '{
    "type": "LOAN-PAYMENT",
    "parameters": {
      "principal": 100000,
      "rate": 0.05,
      "term_years": 30
    }
  }'

# Check API health
curl http://localhost:3000/health
```

## 📱 Mobile Access
The interface is mobile-responsive. Access from your phone:
1. Find your computer's IP: `ifconfig` or `ipconfig`
2. On mobile browser: http://[your-ip]:8080

## 🔧 Quick Troubleshooting

### SuiteCRM Won't Load
```bash
# Check if MySQL is ready
docker-compose logs mysql | grep "ready for connections"

# Restart just SuiteCRM
docker-compose restart suitecrm
```

### Calculations Not Working
```bash
# Check COBOL engine
docker-compose logs cobol-engine

# Check API gateway
docker-compose logs api-gateway

# Test COBOL compilation
docker exec -it suitecrm-cobol_cobol-engine_1 cobc --version
```

### Import Wizard Issues
1. Clear browser cache
2. Check browser console (F12)
3. Verify file size < 10MB for demos

## 📊 What's Working

✅ **COBOL Calculator**
- All 10 calculation types
- Real-time results
- History tracking
- CSV export

✅ **Legacy Bridge**
- Import wizard (4 steps)
- EBCDIC conversion
- Field mapping
- Progress tracking

✅ **Infrastructure**
- Docker containers
- COBOL compilation
- API gateway
- Database

## 🎓 Next Steps

1. Read the full [USER-GUIDE.md](USER-GUIDE.md)
2. Review [ARCHITECTURE.md](ARCHITECTURE.md)
3. Check [VERIFICATION-REPORT.md](VERIFICATION-REPORT.md)
4. Explore the COBOL programs in `cobol-services/`

## 💡 Pro Tips

1. **Faster Calculations**: The first calculation may be slow (COBOL compilation). Subsequent ones are instant.

2. **Sample Data**: Find EBCDIC sample files at:
   - https://github.com/topics/ebcdic
   - https://github.com/topics/cobol-copybook

3. **Development Mode**: Edit files in `custom/modules/` and refresh browser - no restart needed!

4. **Database Access**:
   ```bash
   docker exec -it suitecrm-cobol_mysql_1 mysql -u root -p
   # Password: root
   ```

---

**Need Help?** 
- Check logs: `docker-compose logs`
- Review documentation in `/docs`
- All services should show as "Up" in `docker ps`

**Ready to code?** The COBOL sources are in `cobol-services/` - edit and run `./compile-cobol.sh`!