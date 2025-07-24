<?php
/**
 * COBOL Banking Integration Module for SuiteCRM
 * This manifest file enables one-click installation via Module Loader
 */

$manifest = array(
    'acceptable_sugar_versions' => array(
        'exact_matches' => array(
            '6.5.0', '6.5.1', '6.5.2', '6.5.3', '6.5.4', '6.5.5', '6.5.6', '6.5.7', '6.5.8', '6.5.9',
            '6.5.10', '6.5.11', '6.5.12', '6.5.13', '6.5.14', '6.5.15', '6.5.16', '6.5.17', '6.5.18',
            '6.5.19', '6.5.20', '6.5.21', '6.5.22', '6.5.23', '6.5.24', '6.5.25',
        ),
        'regex_matches' => array(
            '7\..*',
            '8\..*'
        ),
    ),
    'acceptable_sugar_flavors' => array(
        'CE',
        'PRO',
        'ENT'
    ),
    'readme' => 'README.md',
    'key' => 'COBOL_Banking',
    'author' => 'SuiteCRM COBOL Banking Team',
    'description' => 'Enterprise COBOL Integration for Banking - Adds mainframe connectivity, financial calculations with banking precision, and real-time transaction streaming',
    'icon' => 'images/cobol_banking_logo.png',
    'is_uninstallable' => true,
    'name' => 'COBOL Banking Integration',
    'published_date' => '2024-07-22',
    'type' => 'module',
    'version' => '1.0.0',
    'remove_tables' => 'prompt',
);

$installdefs = array(
    'id' => 'COBOL_Banking',
    'beans' => array(
        array(
            'module' => 'COBOL_Bridge',
            'class' => 'COBOL_Bridge',
            'path' => 'modules/COBOL_Bridge/COBOL_Bridge.php',
            'tab' => true,
        ),
        array(
            'module' => 'Mainframe_Sync',
            'class' => 'Mainframe_Sync',
            'path' => 'modules/Mainframe_Sync/Mainframe_Sync.php',
            'tab' => true,
        ),
    ),
    'layoutdefs' => array(),
    'relationships' => array(),
    'vardefs' => array(),
    'copy' => array(
        // Core modules
        array(
            'from' => '<basepath>/modules/COBOL_Bridge',
            'to' => 'custom/modules/COBOL_Bridge',
        ),
        array(
            'from' => '<basepath>/modules/Mainframe_Sync',
            'to' => 'custom/modules/Mainframe_Sync',
        ),
        array(
            'from' => '<basepath>/modules/TransactionLedger',
            'to' => 'custom/modules/TransactionLedger',
        ),
        
        // Integration files
        array(
            'from' => '<basepath>/modules/AOS_Quotes/CobolQuoteCalculator.php',
            'to' => 'custom/modules/AOS_Quotes/CobolQuoteCalculator.php',
        ),
        array(
            'from' => '<basepath>/modules/AOS_Quotes/logic_hooks.php',
            'to' => 'custom/modules/AOS_Quotes/logic_hooks.php',
        ),
        
        // Admin panel
        array(
            'from' => '<basepath>/modules/Administration/COBOLConfig.php',
            'to' => 'custom/modules/Administration/COBOLConfig.php',
        ),
        array(
            'from' => '<basepath>/modules/Administration/COBOLConfig.tpl',
            'to' => 'custom/modules/Administration/COBOLConfig.tpl',
        ),
        
        // Dashlets
        array(
            'from' => '<basepath>/modules/Home/Dashlets/TransactionLedgerDashlet',
            'to' => 'custom/modules/Home/Dashlets/TransactionLedgerDashlet',
        ),
        
        // Docker and COBOL services
        array(
            'from' => '<basepath>/cobol-services',
            'to' => 'cobol-services',
        ),
        array(
            'from' => '<basepath>/docker-compose-cobol.yml',
            'to' => 'docker-compose-cobol.yml',
        ),
        array(
            'from' => '<basepath>/Dockerfile.cobol',
            'to' => 'Dockerfile.cobol',
        ),
        
        // Documentation
        array(
            'from' => '<basepath>/docs',
            'to' => 'custom/modules/COBOL_Bridge/docs',
        ),
        
        // Images
        array(
            'from' => '<basepath>/images',
            'to' => 'custom/themes/default/images/cobol_banking',
        ),
    ),
    'language' => array(
        array(
            'from' => '<basepath>/language/application/en_us.lang.php',
            'to_module' => 'application',
            'language' => 'en_us',
        ),
    ),
    'administration' => array(
        array(
            'from' => '<basepath>/administration/cobol_admin.php',
        ),
    ),
    'pre_install' => '<basepath>/scripts/pre_install.php',
    'post_install' => '<basepath>/scripts/post_install.php',
    'pre_uninstall' => '<basepath>/scripts/pre_uninstall.php',
    'post_uninstall' => '<basepath>/scripts/post_uninstall.php',
);