<?php
// COBOL Banking Home Page Integration
if(!defined('sugarEntry')) define('sugarEntry', true);
require_once('include/entryPoint.php');
require_once('include/MVC/SugarApplication.php');

// Check if user is logged in
if(empty($current_user->id)) {
    SugarApplication::redirect('index.php?action=Login&module=Users');
}

// Include the banking integration
include('banking_integration.php');