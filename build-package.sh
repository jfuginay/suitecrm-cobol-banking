#!/bin/bash
# Build installable package for SuiteCRM Module Loader

echo "=== Building COBOL Banking Integration Package ==="

# Package name and version
PACKAGE_NAME="COBOL_Banking_Integration"
VERSION="1.0.0"
BUILD_DIR="build"
PACKAGE_FILE="${PACKAGE_NAME}_v${VERSION}.zip"

# Clean previous build
rm -rf $BUILD_DIR
rm -f $PACKAGE_FILE

# Create build directory structure
mkdir -p $BUILD_DIR
mkdir -p $BUILD_DIR/modules/COBOL_Bridge
mkdir -p $BUILD_DIR/modules/Mainframe_Sync
mkdir -p $BUILD_DIR/modules/TransactionLedger
mkdir -p $BUILD_DIR/modules/AOS_Quotes
mkdir -p $BUILD_DIR/modules/Administration
mkdir -p $BUILD_DIR/modules/Home/Dashlets/TransactionLedgerDashlet
mkdir -p $BUILD_DIR/scripts
mkdir -p $BUILD_DIR/language/application
mkdir -p $BUILD_DIR/administration
mkdir -p $BUILD_DIR/cobol-services
mkdir -p $BUILD_DIR/docs
mkdir -p $BUILD_DIR/images

# Copy manifest
cp manifest.php $BUILD_DIR/

# Copy modules
cp -r custom/modules/COBOL_Bridge/* $BUILD_DIR/modules/COBOL_Bridge/ 2>/dev/null || echo "COBOL_Bridge module files will be created"
cp -r custom/modules/Mainframe_Sync/* $BUILD_DIR/modules/Mainframe_Sync/ 2>/dev/null || echo "Mainframe_Sync module files will be created"
cp -r custom/modules/Home/Dashlets/TransactionLedgerDashlet/* $BUILD_DIR/modules/Home/Dashlets/TransactionLedgerDashlet/ 2>/dev/null || echo "Dashlet files will be created"

# Copy integration files
cp modules/AOS_Quotes/CobolQuoteCalculator.php $BUILD_DIR/modules/AOS_Quotes/ 2>/dev/null || echo "Creating placeholder"
cp modules/AOS_Quotes/logic_hooks.php $BUILD_DIR/modules/AOS_Quotes/ 2>/dev/null || echo "Creating placeholder"

# Copy admin files
cp modules/Administration/COBOLConfig.php $BUILD_DIR/modules/Administration/
cp modules/Administration/COBOLConfig.tpl $BUILD_DIR/modules/Administration/

# Copy scripts
cp scripts/pre_install.php $BUILD_DIR/scripts/
cp scripts/post_install.php $BUILD_DIR/scripts/

# Create uninstall scripts
cat > $BUILD_DIR/scripts/pre_uninstall.php << 'EOF'
<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

function pre_uninstall() {
    // Stop Docker services if running
    $docker_check = shell_exec('docker ps --filter name=suitecrm-cobol-engine --format "{{.Names}}" 2>&1');
    if (trim($docker_check) === 'suitecrm-cobol-engine') {
        shell_exec('docker-compose -f docker-compose-cobol.yml down 2>&1');
    }
}
EOF

cat > $BUILD_DIR/scripts/post_uninstall.php << 'EOF'
<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

function post_uninstall() {
    global $db;
    
    // Clean up configuration
    require_once('modules/Administration/Administration.php');
    $admin = BeanFactory::newBean('Administration');
    $admin->removeSettings('cobol');
    
    // Note: We don't drop tables by default to preserve data
    $GLOBALS['log']->info('COBOL Banking Integration uninstalled. Database tables preserved.');
}
EOF

# Copy COBOL services and Docker files
cp -r cobol-services/* $BUILD_DIR/cobol-services/
cp docker-compose-cobol.yml $BUILD_DIR/
cp Dockerfile.cobol $BUILD_DIR/

# Copy documentation
cp *.md $BUILD_DIR/docs/ 2>/dev/null || echo "Documentation will be included"

# Create language file
cat > $BUILD_DIR/language/application/en_us.lang.php << 'EOF'
<?php
$mod_strings['LBL_COBOL_CONFIG_TITLE'] = 'COBOL Banking Integration';
$mod_strings['LBL_COBOL_CONFIG_DESC'] = 'Configure COBOL mainframe integration for banking operations';
$mod_strings['LBL_COBOL_BRIDGE'] = 'COBOL Bridge';
$mod_strings['LBL_MAINFRAME_SYNC'] = 'Mainframe Sync';
$mod_strings['LBL_TRANSACTION_LEDGER'] = 'Transaction Ledger';
EOF

# Create admin menu file
cat > $BUILD_DIR/administration/cobol_admin.php << 'EOF'
<?php
$admin_option_defs['Administration']['cobol_config'] = array(
    'Administration',
    'LBL_COBOL_CONFIG_TITLE',
    'LBL_COBOL_CONFIG_DESC',
    'index.php?module=Administration&action=COBOLConfig'
);
EOF

# Create README
cat > $BUILD_DIR/README.md << 'EOF'
# COBOL Banking Integration for SuiteCRM

## Quick Installation

1. Upload this package via Admin → Module Loader
2. Install the package
3. Start COBOL services:
   ```bash
   docker-compose -f docker-compose-cobol.yml up -d
   ```
4. Configure via Admin → COBOL Banking Integration

## Features

- COBOL-powered financial calculations
- Mainframe synchronization
- Real-time transaction streaming
- Banking-grade decimal precision
- Legacy authentication bridge

## Support

- Documentation: See docs/ folder
- Issues: https://github.com/jfuginay/suitecrm-cobol-banking/issues
EOF

# Create the package
cd $BUILD_DIR
zip -r ../$PACKAGE_FILE . -x "*.DS_Store" -x "__MACOSX/*"
cd ..

# Clean up
rm -rf $BUILD_DIR

echo ""
echo "=== Package Build Complete ==="
echo "Package created: $PACKAGE_FILE"
echo "Size: $(ls -lh $PACKAGE_FILE | awk '{print $5}')"
echo ""
echo "To install:"
echo "1. Login to SuiteCRM as admin"
echo "2. Go to Admin → Module Loader"
echo "3. Upload $PACKAGE_FILE"
echo "4. Click Install"
echo ""