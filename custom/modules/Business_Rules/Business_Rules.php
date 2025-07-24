<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('data/SugarBean.php');
require_once('include/utils.php');

class Business_Rules extends SugarBean
{
    public $module_dir = 'Business_Rules';
    public $object_name = 'Business_Rules';
    public $table_name = 'business_rules';
    public $new_schema = true;
    public $module_name = 'Business_Rules';
    
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $description;
    public $deleted;
    public $rule_type;
    public $rule_module;
    public $rule_field;
    public $rule_condition;
    public $rule_value;
    public $cobol_program;
    public $validation_message;
    public $active;
    public $priority;
    public $real_time;
    public $async_validation;
    
    // Cache for loaded rules
    private static $rulesCache = array();
    private static $cacheLoaded = false;
    
    public function Business_Rules()
    {
        parent::__construct();
    }
    
    /**
     * Get all active rules for a module
     */
    public static function getModuleRules($module, $realTimeOnly = true)
    {
        if (!self::$cacheLoaded) {
            self::loadRulesCache();
        }
        
        $moduleRules = array();
        
        if (isset(self::$rulesCache[$module])) {
            foreach (self::$rulesCache[$module] as $rule) {
                if (!$realTimeOnly || $rule['real_time'] == 1) {
                    $moduleRules[] = $rule;
                }
            }
        }
        
        // Sort by priority
        usort($moduleRules, function($a, $b) {
            return $a['priority'] - $b['priority'];
        });
        
        return $moduleRules;
    }
    
    /**
     * Load all active rules into cache
     */
    private static function loadRulesCache()
    {
        global $db;
        
        $query = "SELECT * FROM business_rules 
                  WHERE deleted = 0 AND active = 1 
                  ORDER BY rule_module, priority";
        
        $result = $db->query($query);
        
        while ($row = $db->fetchByAssoc($result)) {
            if (!isset(self::$rulesCache[$row['rule_module']])) {
                self::$rulesCache[$row['rule_module']] = array();
            }
            self::$rulesCache[$row['rule_module']][] = $row;
        }
        
        self::$cacheLoaded = true;
    }
    
    /**
     * Validate a field value using COBOL
     */
    public function validateField($module, $field, $value, $record = array())
    {
        $rules = self::getModuleRules($module);
        $errors = array();
        
        foreach ($rules as $rule) {
            if ($rule['rule_field'] == $field || $rule['rule_field'] == '*') {
                $result = $this->executeRule($rule, $value, $record);
                if (!$result['valid']) {
                    $errors[] = array(
                        'field' => $field,
                        'message' => $result['message'],
                        'rule_name' => $rule['name']
                    );
                }
            }
        }
        
        return array(
            'valid' => empty($errors),
            'errors' => $errors
        );
    }
    
    /**
     * Execute a single rule
     */
    private function executeRule($rule, $value, $record = array())
    {
        switch ($rule['rule_type']) {
            case 'COBOL_VALIDATION':
                return $this->executeCOBOLValidation($rule, $value, $record);
                
            case 'REGEX_PATTERN':
                return $this->executeRegexValidation($rule, $value);
                
            case 'RANGE_CHECK':
                return $this->executeRangeValidation($rule, $value);
                
            case 'LOOKUP_VALIDATION':
                return $this->executeLookupValidation($rule, $value);
                
            case 'CROSS_FIELD':
                return $this->executeCrossFieldValidation($rule, $value, $record);
                
            default:
                return array('valid' => true);
        }
    }
    
    /**
     * Execute COBOL validation
     */
    private function executeCOBOLValidation($rule, $value, $record)
    {
        $cobol_api_url = $this->getCobolApiUrl();
        $endpoint = $cobol_api_url . '/validate';
        
        $payload = array(
            'program' => $rule['cobol_program'],
            'rule_type' => $rule['rule_condition'],
            'value' => $value,
            'context' => $record,
            'parameters' => json_decode($rule['rule_value'], true)
        );
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Accept: application/json'
        ));
        curl_setopt($ch, CURLOPT_TIMEOUT, 2); // 2 second timeout for real-time
        
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($http_code == 200 && $response) {
            $result = json_decode($response, true);
            return array(
                'valid' => $result['valid'],
                'message' => $result['message'] ?? $rule['validation_message']
            );
        }
        
        // Fallback if COBOL service fails
        return array(
            'valid' => true,
            'message' => 'Validation service temporarily unavailable'
        );
    }
    
    /**
     * Execute regex pattern validation
     */
    private function executeRegexValidation($rule, $value)
    {
        $pattern = $rule['rule_value'];
        $valid = preg_match($pattern, $value);
        
        return array(
            'valid' => (bool)$valid,
            'message' => $rule['validation_message']
        );
    }
    
    /**
     * Execute range validation
     */
    private function executeRangeValidation($rule, $value)
    {
        $range = json_decode($rule['rule_value'], true);
        $numValue = floatval($value);
        
        $valid = true;
        if (isset($range['min']) && $numValue < $range['min']) {
            $valid = false;
        }
        if (isset($range['max']) && $numValue > $range['max']) {
            $valid = false;
        }
        
        return array(
            'valid' => $valid,
            'message' => $rule['validation_message']
        );
    }
    
    /**
     * Execute lookup validation
     */
    private function executeLookupValidation($rule, $value)
    {
        global $db;
        
        $lookup = json_decode($rule['rule_value'], true);
        $table = $lookup['table'];
        $field = $lookup['field'];
        
        $query = sprintf(
            "SELECT COUNT(*) as count FROM %s WHERE %s = %s AND deleted = 0",
            $table,
            $field,
            $db->quoted($value)
        );
        
        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);
        
        return array(
            'valid' => $row['count'] > 0,
            'message' => $rule['validation_message']
        );
    }
    
    /**
     * Execute cross-field validation
     */
    private function executeCrossFieldValidation($rule, $value, $record)
    {
        $condition = json_decode($rule['rule_value'], true);
        $valid = true;
        
        foreach ($condition['rules'] as $crossRule) {
            $otherField = $crossRule['field'];
            $operator = $crossRule['operator'];
            $compareValue = $record[$otherField] ?? null;
            
            switch ($operator) {
                case 'greater_than':
                    if (floatval($value) <= floatval($compareValue)) {
                        $valid = false;
                    }
                    break;
                    
                case 'less_than':
                    if (floatval($value) >= floatval($compareValue)) {
                        $valid = false;
                    }
                    break;
                    
                case 'equals':
                    if ($value != $compareValue) {
                        $valid = false;
                    }
                    break;
                    
                case 'not_equals':
                    if ($value == $compareValue) {
                        $valid = false;
                    }
                    break;
            }
            
            if (!$valid) break;
        }
        
        return array(
            'valid' => $valid,
            'message' => $rule['validation_message']
        );
    }
    
    /**
     * Clear rules cache
     */
    public static function clearCache()
    {
        self::$rulesCache = array();
        self::$cacheLoaded = false;
    }
    
    /**
     * Get COBOL API URL
     */
    private function getCobolApiUrl()
    {
        global $sugar_config;
        return $sugar_config['cobol_api_url'] ?? 'http://localhost:3000';
    }
    
    /**
     * Get available rule types
     */
    public static function getRuleTypes()
    {
        return array(
            'COBOL_VALIDATION' => 'COBOL Program Validation',
            'REGEX_PATTERN' => 'Regular Expression Pattern',
            'RANGE_CHECK' => 'Numeric Range Check',
            'LOOKUP_VALIDATION' => 'Database Lookup',
            'CROSS_FIELD' => 'Cross-Field Validation'
        );
    }
    
    /**
     * Get available COBOL validators
     */
    public static function getCOBOLValidators()
    {
        return array(
            'ACCOUNT-VALIDATOR' => 'Account Number Validation',
            'ROUTING-VALIDATOR' => 'Routing Number Validation',
            'SSN-VALIDATOR' => 'Social Security Number',
            'TAX-ID-VALIDATOR' => 'Tax ID Validation',
            'IBAN-VALIDATOR' => 'International Bank Account',
            'SWIFT-VALIDATOR' => 'SWIFT Code Validation',
            'CREDIT-LIMIT' => 'Credit Limit Rules',
            'COMPLIANCE-CHECK' => 'Regulatory Compliance'
        );
    }
}