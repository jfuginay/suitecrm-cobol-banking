<?php
/**
 * COBOL Bridge Module Variable Definitions
 */

$dictionary['COBOL_Bridge'] = array(
    'table' => 'cobol_bridge',
    'audited' => true,
    'duplicate_merge' => true,
    'fields' => array(
        'id' => array(
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'id',
            'required' => true,
            'reportable' => true,
        ),
        'name' => array(
            'name' => 'name',
            'vname' => 'LBL_NAME',
            'type' => 'name',
            'dbType' => 'varchar',
            'len' => '255',
            'required' => true,
        ),
        'date_entered' => array(
            'name' => 'date_entered',
            'vname' => 'LBL_DATE_ENTERED',
            'type' => 'datetime',
        ),
        'date_modified' => array(
            'name' => 'date_modified',
            'vname' => 'LBL_DATE_MODIFIED',
            'type' => 'datetime',
        ),
        'description' => array(
            'name' => 'description',
            'vname' => 'LBL_DESCRIPTION',
            'type' => 'text',
        ),
        'deleted' => array(
            'name' => 'deleted',
            'vname' => 'LBL_DELETED',
            'type' => 'bool',
            'default' => '0',
        ),
    ),
    'indices' => array(
        array('name' => 'cobol_bridgepk', 'type' => 'primary', 'fields' => array('id')),
    ),
);