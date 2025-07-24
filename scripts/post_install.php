<?php
/**
 * Post-installation script for COBOL Banking Integration
 * Creates necessary database tables and configuration
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

function post_install() {
    global $db, $sugar_config;
    
    $GLOBALS['log']->info('COBOL Banking Integration: Starting post-install process');
    
    // Create necessary database tables
    create_cobol_tables();
    
    // Set default configuration
    set_default_config();
    
    // Create admin menu entry
    create_admin_menu();
    
    // Clear cache
    require_once('include/SugarCache/SugarCache.php');
    SugarCache::cleanOpcodes();
    
    // Start Docker services if configured
    if (!empty($sugar_config['cobol_docker_auto_start'])) {
        start_docker_services();
    }
    
    $GLOBALS['log']->info('COBOL Banking Integration: Post-install completed successfully');
}

function create_cobol_tables() {
    global $db;
    
    $GLOBALS['log']->info('Creating COBOL integration tables...');
    
    // Table for caching COBOL calculations
    $sql1 = "CREATE TABLE IF NOT EXISTS cobol_calculation_cache (
        id CHAR(36) NOT NULL PRIMARY KEY,
        request_hash VARCHAR(64) NOT NULL,
        request_params TEXT,
        result TEXT,
        error_message TEXT,
        created_at DATETIME,
        expires_at DATETIME,
        INDEX idx_request_hash (request_hash),
        INDEX idx_expires_at (expires_at)
    )";
    
    // Table for mainframe sync logging
    $sql2 = "CREATE TABLE IF NOT EXISTS mainframe_sync_log (
        id CHAR(36) NOT NULL PRIMARY KEY,
        sync_type VARCHAR(50),
        sync_direction VARCHAR(20),
        status VARCHAR(20),
        records_synced INT DEFAULT 0,
        records_failed INT DEFAULT 0,
        error_message TEXT,
        started_at DATETIME,
        completed_at DATETIME,
        duration_seconds INT,
        INDEX idx_sync_type (sync_type),
        INDEX idx_status (status),
        INDEX idx_started_at (started_at)
    )";
    
    // Table for COBOL service audit log
    $sql3 = "CREATE TABLE IF NOT EXISTS cobol_audit_log (
        id CHAR(36) NOT NULL PRIMARY KEY,
        user_id CHAR(36),
        action_type VARCHAR(50),
        module_name VARCHAR(50),
        record_id CHAR(36),
        cobol_program VARCHAR(50),
        input_params TEXT,
        output_result TEXT,
        execution_time DECIMAL(10,3),
        ip_address VARCHAR(45),
        created_at DATETIME,
        INDEX idx_user_id (user_id),
        INDEX idx_action_type (action_type),
        INDEX idx_created_at (created_at)
    )";
    
    // Table for transaction ledger
    $sql4 = "CREATE TABLE IF NOT EXISTS cobol_transaction_ledger (
        id CHAR(36) NOT NULL PRIMARY KEY,
        account_number VARCHAR(20),
        transaction_type VARCHAR(20),
        amount DECIMAL(19,4),
        balance DECIMAL(19,4),
        description VARCHAR(255),
        reference_number VARCHAR(50),
        status VARCHAR(20),
        posted_date DATETIME,
        created_at DATETIME,
        INDEX idx_account_number (account_number),
        INDEX idx_transaction_type (transaction_type),
        INDEX idx_posted_date (posted_date)
    )";
    
    // Execute SQL statements
    $db->query($sql1);
    $db->query($sql2);
    $db->query($sql3);
    $db->query($sql4);
    
    $GLOBALS['log']->info('COBOL integration tables created successfully');
}

function set_default_config() {
    require_once('modules/Administration/Administration.php');
    $admin = BeanFactory::newBean('Administration');
    
    $defaults = array(
        'cobol_api_url' => 'http://localhost:3001',
        'cobol_websocket_url' => 'ws://localhost:8081',
        'cobol_integration_enabled' => '1',
        'cobol_cache_enabled' => '1',
        'cobol_cache_ttl' => '3600',
        'cobol_mainframe_host' => '',
        'cobol_mainframe_port' => '3270',
        'cobol_auth_type' => 'LDAP',
        'cobol_docker_auto_start' => '0',
        'cobol_log_level' => 'INFO',
        'cobol_max_batch_size' => '1000',
        'cobol_timeout_seconds' => '30'
    );
    
    foreach ($defaults as $key => $value) {
        // Only set if not already configured
        if (empty($admin->settings[$key])) {
            $admin->saveSetting('cobol', $key, $value);
        }
    }
    
    // Also update config_override.php
    require_once('modules/Configurator/Configurator.php');
    $configurator = new Configurator();
    $configurator->loadConfig();
    
    $configurator->config['cobol_api_url'] = $defaults['cobol_api_url'];
    $configurator->config['cobol_websocket_url'] = $defaults['cobol_websocket_url'];
    $configurator->config['cobol_integration_enabled'] = true;
    
    $configurator->saveConfig();
    
    $GLOBALS['log']->info('COBOL default configuration set');
}

function create_admin_menu() {
    // Add COBOL configuration to admin menu
    $admin_option_defs = array();
    $admin_option_defs['Administration']['cobol_config'] = array(
        'Administration',
        'LBL_COBOL_CONFIG_TITLE',
        'LBL_COBOL_CONFIG_DESC',
        'index.php?module=Administration&action=COBOLConfig'
    );
    
    // Save to custom directory
    $filename = 'custom/Extension/modules/Administration/Ext/Administration/cobol_admin.php';
    $dir = dirname($filename);
    if (!is_dir($dir)) {
        mkdir($dir, 0755, true);
    }
    
    $content = "<?php\n";
    $content .= "\$admin_option_defs['Administration']['cobol_config'] = array(\n";
    $content .= "    'Administration',\n";
    $content .= "    'LBL_COBOL_CONFIG_TITLE',\n";
    $content .= "    'LBL_COBOL_CONFIG_DESC',\n";
    $content .= "    'index.php?module=Administration&action=COBOLConfig'\n";
    $content .= ");\n";
    
    file_put_contents($filename, $content);
    
    $GLOBALS['log']->info('COBOL admin menu created');
}

function start_docker_services() {
    $GLOBALS['log']->info('Attempting to start Docker services...');
    
    // Check if docker-compose file exists
    $docker_compose_path = 'docker-compose-cobol.yml';
    if (file_exists($docker_compose_path)) {
        // Check if Docker is available
        $docker_check = shell_exec('docker --version 2>&1');
        if (strpos($docker_check, 'Docker version') !== false) {
            // Start services in background
            $cmd = "docker-compose -f {$docker_compose_path} up -d 2>&1";
            $output = shell_exec($cmd);
            $GLOBALS['log']->info("Docker services start output: " . $output);
        } else {
            $GLOBALS['log']->warn('Docker not found, skipping service start');
        }
    } else {
        $GLOBALS['log']->warn('docker-compose-cobol.yml not found');
    }
}

// Run the post-install
post_install();