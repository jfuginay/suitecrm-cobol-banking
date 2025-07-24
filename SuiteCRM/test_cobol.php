<?php
// Test COBOL Module Access
if(!defined('sugarEntry'))define('sugarEntry', true);
require_once('include/entryPoint.php');
require_once('custom/modules/COBOL_Bridge/COBOL_Bridge.php');
require_once('custom/modules/COBOL_Bridge/controller.php');

// Set module
$_REQUEST['module'] = 'COBOL_Bridge';
$_REQUEST['action'] = 'calculator';

// Create controller
$controller = new COBOL_BridgeController();
$controller->action_calculator();