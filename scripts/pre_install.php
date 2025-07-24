<?php
/**
 * Pre-installation script for COBOL Banking Integration
 * Checks system requirements before installation
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

function pre_install() {
    global $mod_strings;
    
    $errors = array();
    
    // Check PHP version (need 7.2+)
    if (version_compare(PHP_VERSION, '7.2.0', '<')) {
        $errors[] = 'PHP 7.2 or higher is required. Current version: ' . PHP_VERSION;
    }
    
    // Check required PHP extensions
    $required_extensions = array('curl', 'json', 'zip');
    foreach ($required_extensions as $ext) {
        if (!extension_loaded($ext)) {
            $errors[] = "PHP extension '{$ext}' is required but not installed";
        }
    }
    
    // Check if custom directory is writable
    $custom_dir = 'custom/modules';
    if (!is_writable($custom_dir)) {
        $errors[] = "Directory '{$custom_dir}' must be writable";
    }
    
    // Check port availability (warning only)
    $ports_to_check = array(3001, 8081, 6379);
    $port_warnings = array();
    foreach ($ports_to_check as $port) {
        $connection = @fsockopen('localhost', $port, $errno, $errstr, 1);
        if ($connection) {
            $port_warnings[] = "Port {$port} is already in use. You may need to adjust COBOL service ports.";
            fclose($connection);
        }
    }
    
    // Check Docker availability (warning only)
    $docker_available = false;
    $docker_version = shell_exec('docker --version 2>&1');
    if (strpos($docker_version, 'Docker version') !== false) {
        $docker_available = true;
    }
    
    // Display results
    if (!empty($errors)) {
        echo '<div style="color: red; font-weight: bold;">Installation Cannot Proceed:</div>';
        echo '<ul>';
        foreach ($errors as $error) {
            echo '<li>' . $error . '</li>';
        }
        echo '</ul>';
        die();
    }
    
    // Display warnings
    if (!empty($port_warnings) || !$docker_available) {
        echo '<div style="background: #fffacd; padding: 10px; margin: 10px 0; border: 1px solid #ddd;">';
        echo '<strong>Warnings:</strong><br>';
        
        if (!$docker_available) {
            echo '• Docker is not installed or not accessible. You will need Docker to run COBOL services.<br>';
        }
        
        foreach ($port_warnings as $warning) {
            echo '• ' . $warning . '<br>';
        }
        
        echo '<br><em>Installation can continue, but you may need to address these issues for full functionality.</em>';
        echo '</div>';
    }
    
    // Success message
    echo '<div style="background: #d4edda; padding: 10px; margin: 10px 0; border: 1px solid #c3e6cb; color: #155724;">';
    echo '<strong>✓ System requirements check passed!</strong><br>';
    echo 'COBOL Banking Integration is ready to install.';
    echo '</div>';
    
    return true;
}