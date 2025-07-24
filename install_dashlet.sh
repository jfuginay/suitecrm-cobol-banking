#!/bin/bash

# Install COBOL Banking Dashlet to SuiteCRM

SUITECRM_PATH="/Users/jfuginay/Documents/dev/SuiteCRM-fork"
DASHLET_PATH="$SUITECRM_PATH/custom/modules/Home/Dashlets/COBOLBankingDashlet"

echo "Installing COBOL Banking Dashlet..."

# Create directory structure
mkdir -p "$DASHLET_PATH"

# Copy dashlet files
cp COBOLBankingDashlet.php "$DASHLET_PATH/"
cp COBOLBankingDashlet.meta.php "$DASHLET_PATH/"

# Also copy to modules/Home/Dashlets for backward compatibility
CORE_DASHLET_PATH="$SUITECRM_PATH/modules/Home/Dashlets/COBOLBankingDashlet"
mkdir -p "$CORE_DASHLET_PATH"
cp COBOLBankingDashlet.php "$CORE_DASHLET_PATH/"
cp COBOLBankingDashlet.meta.php "$CORE_DASHLET_PATH/"

# Clear cache
rm -rf "$SUITECRM_PATH/cache/dashlets/*"
rm -rf "$SUITECRM_PATH/cache/modules/*"

echo "Dashlet installed successfully!"
echo ""
echo "To use the dashlet:"
echo "1. Go to your SuiteCRM homepage"
echo "2. Click 'Add Dashlets' button"
echo "3. Look for 'COBOL Banking Services' in the Tools category"
echo "4. Drag it to your dashboard"
echo ""
echo "You may need to do a Quick Repair and Rebuild from Admin panel."