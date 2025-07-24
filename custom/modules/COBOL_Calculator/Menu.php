<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

global $mod_strings, $app_strings, $sugar_config;

if (ACLController::checkAccess('COBOL_Calculator', 'edit', true)) {
    $module_menu[] = array(
        'index.php?module=COBOL_Calculator&action=EditView&return_module=COBOL_Calculator&return_action=DetailView',
        $mod_strings['LBL_NEW_FORM_TITLE'],
        'Create',
        'COBOL_Calculator'
    );
}

if (ACLController::checkAccess('COBOL_Calculator', 'list', true)) {
    $module_menu[] = array(
        'index.php?module=COBOL_Calculator&action=index&return_module=COBOL_Calculator&return_action=DetailView',
        $mod_strings['LBL_LIST_FORM_TITLE'],
        'List',
        'COBOL_Calculator'
    );
}

if (ACLController::checkAccess('COBOL_Calculator', 'view', true)) {
    $module_menu[] = array(
        'index.php?module=COBOL_Calculator&action=calculator',
        $mod_strings['LBL_CALCULATOR_TITLE'],
        'Calculator',
        'COBOL_Calculator'
    );
    
    $module_menu[] = array(
        'index.php?module=COBOL_Calculator&action=history',
        $mod_strings['LBL_VIEW_HISTORY'],
        'History',
        'COBOL_Calculator'
    );
}