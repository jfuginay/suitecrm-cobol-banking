<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('data/SugarBean.php');
require_once('include/utils.php');

class Legacy_Bridge extends SugarBean
{
    public $module_dir = 'Legacy_Bridge';
    public $object_name = 'Legacy_Bridge';
    public $table_name = 'legacy_bridge';
    public $new_schema = true;
    public $module_name = 'Legacy_Bridge';
    
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $description;
    public $deleted;
    public $bridge_type;
    public $source_system;
    public $source_format;
    public $copybook_definition;
    public $field_mappings;
    public $transformation_rules;
    public $import_status;
    public $export_status;
    public $last_sync_date;
    public $records_processed;
    public $records_failed;
    public $error_log;
    public $execution_log;
    
    public function Legacy_Bridge()
    {
        parent::__construct();
    }
    
    /**
     * Import data from legacy COBOL system
     */
    public function importFromLegacy($file_path, $options = array())
    {
        $start_time = microtime(true);
        $this->import_status = 'processing';
        $this->save();
        
        try {
            // Prepare COBOL service request
            $cobol_api_url = $this->getCobolApiUrl();
            $endpoint = $cobol_api_url . '/legacy/import';
            
            // Read file content
            $file_content = file_get_contents($file_path);
            if (!$file_content) {
                throw new Exception('Unable to read file: ' . $file_path);
            }
            
            // Build request payload
            $payload = array(
                'bridge_type' => $this->bridge_type,
                'source_format' => $this->source_format,
                'copybook' => $this->copybook_definition,
                'data' => base64_encode($file_content),
                'encoding' => $options['encoding'] ?? 'EBCDIC',
                'record_length' => $options['record_length'] ?? 0,
                'field_mappings' => json_decode($this->field_mappings, true)
            );
            
            // Execute COBOL import
            $ch = curl_init($endpoint);
            curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
            curl_setopt($ch, CURLOPT_POST, true);
            curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
            curl_setopt($ch, CURLOPT_HTTPHEADER, array(
                'Content-Type: application/json',
                'Accept: application/json'
            ));
            curl_setopt($ch, CURLOPT_TIMEOUT, 300); // 5 minute timeout for large files
            
            $response = curl_exec($ch);
            $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
            curl_close($ch);
            
            if ($http_code == 200 && $response) {
                $result = json_decode($response, true);
                
                // Process converted records
                $processed = $this->processImportedRecords($result['records']);
                
                // Update status
                $this->import_status = 'completed';
                $this->last_sync_date = date('Y-m-d H:i:s');
                $this->records_processed = $processed['success'];
                $this->records_failed = $processed['failed'];
                $this->execution_log = json_encode(array(
                    'execution_time' => microtime(true) - $start_time,
                    'file_size' => strlen($file_content),
                    'records_total' => count($result['records']),
                    'conversion_time' => $result['processing_time']
                ));
                $this->save();
                
                return array(
                    'success' => true,
                    'records_processed' => $processed['success'],
                    'records_failed' => $processed['failed'],
                    'details' => $processed['details']
                );
            } else {
                throw new Exception('COBOL service error: HTTP ' . $http_code);
            }
            
        } catch (Exception $e) {
            $this->import_status = 'error';
            $this->error_log = $e->getMessage();
            $this->save();
            
            return array(
                'success' => false,
                'error' => $e->getMessage()
            );
        }
    }
    
    /**
     * Export data to legacy COBOL format
     */
    public function exportToLegacy($module, $filters = array())
    {
        $start_time = microtime(true);
        $this->export_status = 'processing';
        $this->save();
        
        try {
            // Fetch records from SuiteCRM
            $records = $this->fetchRecordsForExport($module, $filters);
            
            if (empty($records)) {
                throw new Exception('No records found for export');
            }
            
            // Prepare COBOL service request
            $cobol_api_url = $this->getCobolApiUrl();
            $endpoint = $cobol_api_url . '/legacy/export';
            
            // Build request payload
            $payload = array(
                'bridge_type' => $this->bridge_type,
                'target_format' => $this->source_format,
                'copybook' => $this->copybook_definition,
                'records' => $records,
                'encoding' => 'EBCDIC',
                'field_mappings' => json_decode($this->field_mappings, true),
                'transformation_rules' => json_decode($this->transformation_rules, true)
            );
            
            // Execute COBOL export
            $ch = curl_init($endpoint);
            curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
            curl_setopt($ch, CURLOPT_POST, true);
            curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
            curl_setopt($ch, CURLOPT_HTTPHEADER, array(
                'Content-Type: application/json',
                'Accept: application/json'
            ));
            curl_setopt($ch, CURLOPT_TIMEOUT, 300);
            
            $response = curl_exec($ch);
            $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
            curl_close($ch);
            
            if ($http_code == 200 && $response) {
                $result = json_decode($response, true);
                
                // Save exported file
                $export_path = 'upload/legacy_exports/' . date('Ymd_His') . '_export.dat';
                if (!is_dir('upload/legacy_exports')) {
                    mkdir('upload/legacy_exports', 0755, true);
                }
                
                file_put_contents($export_path, base64_decode($result['data']));
                
                // Update status
                $this->export_status = 'completed';
                $this->last_sync_date = date('Y-m-d H:i:s');
                $this->records_processed = count($records);
                $this->records_failed = 0;
                $this->execution_log = json_encode(array(
                    'execution_time' => microtime(true) - $start_time,
                    'records_exported' => count($records),
                    'file_path' => $export_path,
                    'file_size' => filesize($export_path)
                ));
                $this->save();
                
                return array(
                    'success' => true,
                    'file_path' => $export_path,
                    'records_exported' => count($records)
                );
            } else {
                throw new Exception('COBOL service error: HTTP ' . $http_code);
            }
            
        } catch (Exception $e) {
            $this->export_status = 'error';
            $this->error_log = $e->getMessage();
            $this->save();
            
            return array(
                'success' => false,
                'error' => $e->getMessage()
            );
        }
    }
    
    /**
     * Parse COBOL copybook definition
     */
    public function parseCopybook($copybook_content)
    {
        $cobol_api_url = $this->getCobolApiUrl();
        $endpoint = $cobol_api_url . '/legacy/parse-copybook';
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode(array(
            'copybook' => $copybook_content
        )));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Accept: application/json'
        ));
        
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($http_code == 200 && $response) {
            return json_decode($response, true);
        }
        
        return null;
    }
    
    /**
     * Process imported records
     */
    private function processImportedRecords($records)
    {
        $success = 0;
        $failed = 0;
        $details = array();
        
        $mappings = json_decode($this->field_mappings, true);
        $target_module = $mappings['target_module'] ?? 'Accounts';
        
        foreach ($records as $record) {
            try {
                $bean = BeanFactory::newBean($target_module);
                
                // Map fields
                foreach ($mappings['field_map'] as $source => $target) {
                    if (isset($record[$source])) {
                        $bean->$target = $this->transformValue($record[$source], $source, $target);
                    }
                }
                
                // Apply transformation rules
                if ($this->transformation_rules) {
                    $rules = json_decode($this->transformation_rules, true);
                    foreach ($rules as $rule) {
                        $this->applyTransformationRule($bean, $rule);
                    }
                }
                
                $bean->save();
                $success++;
                
            } catch (Exception $e) {
                $failed++;
                $details[] = array(
                    'record' => $record,
                    'error' => $e->getMessage()
                );
            }
        }
        
        return array(
            'success' => $success,
            'failed' => $failed,
            'details' => $details
        );
    }
    
    /**
     * Fetch records for export
     */
    private function fetchRecordsForExport($module, $filters)
    {
        $bean = BeanFactory::newBean($module);
        $query = new SugarQuery();
        $query->from($bean);
        
        // Apply filters
        foreach ($filters as $field => $value) {
            $query->where()->equals($field, $value);
        }
        
        $results = $query->execute();
        $records = array();
        
        $mappings = json_decode($this->field_mappings, true);
        
        foreach ($results as $row) {
            $record = array();
            foreach ($mappings['field_map'] as $target => $source) {
                if (isset($row[$source])) {
                    $record[$target] = $row[$source];
                }
            }
            $records[] = $record;
        }
        
        return $records;
    }
    
    /**
     * Transform value based on field type
     */
    private function transformValue($value, $source_field, $target_field)
    {
        // Trim COBOL spaces
        $value = trim($value);
        
        // Convert based on known patterns
        if (strpos($source_field, 'DATE') !== false) {
            // Convert YYYYMMDD to Y-m-d
            if (strlen($value) == 8 && is_numeric($value)) {
                $value = substr($value, 0, 4) . '-' . substr($value, 4, 2) . '-' . substr($value, 6, 2);
            }
        }
        
        if (strpos($source_field, 'AMOUNT') !== false || strpos($source_field, 'BALANCE') !== false) {
            // Handle implied decimal (last 2 digits are decimals)
            $value = number_format($value / 100, 2, '.', '');
        }
        
        return $value;
    }
    
    /**
     * Apply transformation rule
     */
    private function applyTransformationRule(&$bean, $rule)
    {
        switch ($rule['type']) {
            case 'uppercase':
                $bean->{$rule['field']} = strtoupper($bean->{$rule['field']});
                break;
                
            case 'lowercase':
                $bean->{$rule['field']} = strtolower($bean->{$rule['field']});
                break;
                
            case 'trim':
                $bean->{$rule['field']} = trim($bean->{$rule['field']});
                break;
                
            case 'default':
                if (empty($bean->{$rule['field']})) {
                    $bean->{$rule['field']} = $rule['value'];
                }
                break;
                
            case 'replace':
                $bean->{$rule['field']} = str_replace($rule['search'], $rule['replace'], $bean->{$rule['field']});
                break;
        }
    }
    
    /**
     * Get COBOL API URL from configuration
     */
    private function getCobolApiUrl()
    {
        global $sugar_config;
        return $sugar_config['cobol_api_url'] ?? 'http://localhost:3000';
    }
    
    /**
     * Get available bridge types
     */
    public static function getBridgeTypes()
    {
        return array(
            'CUSTOMER-MASTER' => 'Customer Master File',
            'ACCOUNT-LEDGER' => 'Account Ledger',
            'TRANSACTION-LOG' => 'Transaction Log',
            'PRODUCT-CATALOG' => 'Product Catalog',
            'EMPLOYEE-RECORDS' => 'Employee Records',
            'VENDOR-MASTER' => 'Vendor Master File',
            'INVENTORY-FILE' => 'Inventory File',
            'CUSTOM' => 'Custom Format'
        );
    }
}