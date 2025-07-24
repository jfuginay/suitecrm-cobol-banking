# Local Installation Guide for SuiteCRM with COBOL Integration

## Option 1: Using Docker (Recommended - Already Configured)

Since your project already has Docker configuration, this is the easiest way:

```bash
# From your project directory
cd /Users/jfuginay/Documents/dev/suitecrm-cobol

# Start all services
docker-compose up -d

# Access SuiteCRM at http://localhost:8080
# Default credentials: admin / admin
```

## Option 2: Manual Local Installation (Without Docker)

### Prerequisites

1. **Install required software on macOS**:
   ```bash
   # Install Homebrew if not already installed
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   
   # Install PHP and required extensions
   brew install php@8.1
   brew install php@8.1-gd php@8.1-zip php@8.1-xml php@8.1-mbstring php@8.1-curl
   
   # Install MySQL
   brew install mysql
   brew services start mysql
   
   # Install Apache (or use PHP's built-in server)
   brew install httpd
   brew services start httpd
   
   # Install Node.js for COBOL services
   brew install node
   
   # Install GnuCOBOL
   brew install gnu-cobol
   ```

2. **Create database**:
   ```bash
   mysql -u root -p
   CREATE DATABASE suitecrm;
   CREATE USER 'suitecrm'@'localhost' IDENTIFIED BY 'suitecrm_password';
   GRANT ALL PRIVILEGES ON suitecrm.* TO 'suitecrm'@'localhost';
   FLUSH PRIVILEGES;
   EXIT;
   ```

### Installation Steps

1. **Copy SuiteCRM files to web directory**:
   ```bash
   # For Apache on macOS
   sudo cp -r /Users/jfuginay/Documents/dev/SuiteCRM-fork/* /usr/local/var/www/
   
   # Or create a symlink
   sudo ln -s /Users/jfuginay/Documents/dev/SuiteCRM-fork /usr/local/var/www/suitecrm
   ```

2. **Set permissions**:
   ```bash
   cd /usr/local/var/www/suitecrm
   sudo chown -R _www:_www .
   sudo chmod -R 755 .
   sudo chmod -R 775 cache custom modules themes data upload
   sudo chmod 775 config_override.php 2>/dev/null
   ```

3. **Configure Apache** (create `/usr/local/etc/httpd/extra/suitecrm.conf`):
   ```apache
   <VirtualHost *:80>
       ServerName suitecrm.local
       DocumentRoot "/usr/local/var/www/suitecrm"
       
       <Directory "/usr/local/var/www/suitecrm">
           Options Indexes FollowSymLinks
           AllowOverride All
           Require all granted
       </Directory>
   </VirtualHost>
   ```

4. **Add to /etc/hosts**:
   ```bash
   echo "127.0.0.1 suitecrm.local" | sudo tee -a /etc/hosts
   ```

5. **Start COBOL services**:
   ```bash
   cd /Users/jfuginay/Documents/dev/suitecrm-cobol/cobol-services
   
   # Install Node dependencies
   npm install
   
   # Compile COBOL programs
   cobc -x -o financial-calc financial-calc.cob
   cobc -x -o legacy-auth legacy-auth.cob
   cobc -x -o mainframe-sync mainframe-sync.cob
   cobc -x -o transaction-stream transaction-stream.cob
   
   # Start the services
   ./start-servers.sh
   ```

6. **Access installation wizard**:
   - Navigate to http://suitecrm.local/install.php
   - Follow the installation wizard
   - Database settings:
     - Database Name: suitecrm
     - Host Name: localhost
     - User: suitecrm
     - Password: suitecrm_password

## Option 3: Quick Test with PHP Built-in Server

For a quick test without Apache:

```bash
cd /Users/jfuginay/Documents/dev/SuiteCRM-fork

# Start PHP server
php -S localhost:8000

# In another terminal, start COBOL services
cd /Users/jfuginay/Documents/dev/suitecrm-cobol/cobol-services
npm install
npm start

# Access http://localhost:8000/install.php
```

## Post-Installation Configuration

1. **Enable COBOL Integration**:
   After installation, add to `config_override.php`:
   ```php
   $sugar_config['cobol_services'] = array(
       'api_endpoint' => 'http://localhost:3000',
       'websocket_endpoint' => 'ws://localhost:8080',
       'enabled' => true,
   );
   ```

2. **Test COBOL Services**:
   ```bash
   # Test calculation service
   curl http://localhost:3000/health
   
   # Test a calculation
   curl -X POST http://localhost:3000/calculate \
     -H "Content-Type: application/json" \
     -d '{"type": "LOAN-PAYMENT", "principal": 100000, "rate": 0.05, "term": 360}'
   ```

3. **Access SuiteCRM**:
   - Login with your admin credentials
   - Navigate to Quotes module to test COBOL calculations
   - Check Home dashboard for Transaction Ledger dashlet

## Troubleshooting

1. **Permission Issues**: Make sure web server user has write access to required directories
2. **COBOL Compilation**: Ensure GnuCOBOL is properly installed: `cobc --version`
3. **Database Connection**: Verify MySQL is running: `brew services list`
4. **Port Conflicts**: Ensure ports 80, 3000, and 8080 are available

## Quick Start Script

Create a `start-local.sh` script:

```bash
#!/bin/bash

# Start MySQL
brew services start mysql

# Start Apache
brew services start httpd

# Start COBOL services
cd cobol-services && npm start &

# Open browser
open http://suitecrm.local

echo "SuiteCRM with COBOL integration is starting..."
echo "Access at: http://suitecrm.local"
echo "COBOL API at: http://localhost:3000"
```

Make it executable: `chmod +x start-local.sh`

---

**Recommended**: Use the Docker option (Option 1) as it's already configured and ready to go!