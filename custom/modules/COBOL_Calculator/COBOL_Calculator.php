<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('data/SugarBean.php');
require_once('include/utils.php');

class COBOL_Calculator extends SugarBean
{
    public $module_dir = 'COBOL_Calculator';
    public $object_name = 'COBOL_Calculator';
    public $table_name = 'cobol_calculator';
    public $new_schema = true;
    public $module_name = 'COBOL_Calculator';
    
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $description;
    public $deleted;
    public $calculation_type;
    public $input_parameters;
    public $calculation_result;
    public $cobol_response;
    public $execution_time;
    public $status;
    
    public function COBOL_Calculator()
    {
        parent::__construct();
    }
    
    /**
     * Execute COBOL calculation
     */
    public function executeCalculation($type, $params)
    {
        $start_time = microtime(true);
        
        // Prepare COBOL service request
        $cobol_api_url = $this->getCobolApiUrl();
        $endpoint = $cobol_api_url . '/calculate';
        
        // Build request payload
        $payload = array(
            'type' => strtoupper($type),
            'parameters' => $params,
            'precision' => 6, // Banking precision
            'currency' => $params['currency'] ?? 'USD'
        );
        
        // Execute COBOL calculation
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Accept: application/json'
        ));
        curl_setopt($ch, CURLOPT_TIMEOUT, 30);
        
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        $execution_time = microtime(true) - $start_time;
        
        // Process response
        if ($http_code == 200 && $response) {
            $result = json_decode($response, true);
            
            // Store calculation record
            $this->name = $type . ' - ' . date('Y-m-d H:i:s');
            $this->calculation_type = $type;
            $this->input_parameters = json_encode($params);
            $this->calculation_result = json_encode($result['result']);
            $this->cobol_response = $response;
            $this->execution_time = $execution_time;
            $this->status = 'completed';
            $this->save();
            
            return array(
                'success' => true,
                'result' => $result['result'],
                'execution_time' => $execution_time,
                'calculation_id' => $this->id
            );
        } else {
            // Handle error
            $this->name = $type . ' - ERROR - ' . date('Y-m-d H:i:s');
            $this->calculation_type = $type;
            $this->input_parameters = json_encode($params);
            $this->calculation_result = 'ERROR';
            $this->cobol_response = $response ?: 'Connection failed';
            $this->execution_time = $execution_time;
            $this->status = 'error';
            $this->save();
            
            return array(
                'success' => false,
                'error' => 'COBOL calculation failed',
                'http_code' => $http_code,
                'calculation_id' => $this->id
            );
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
     * Available calculation types
     */
    public static function getCalculationTypes()
    {
        return array(
            'LOAN-PAYMENT' => 'Loan Payment Calculator',
            'COMPOUND-INTEREST' => 'Compound Interest Calculator',
            'AMORTIZATION' => 'Amortization Schedule',
            'ROI-CALCULATOR' => 'Return on Investment',
            'MORTGAGE-CALCULATOR' => 'Mortgage Calculator',
            'SAVINGS-GOAL' => 'Savings Goal Calculator',
            'RETIREMENT-PLANNING' => 'Retirement Planning',
            'RISK-ASSESSMENT' => 'Risk Assessment Score',
            'CURRENCY-CONVERSION' => 'Currency Conversion',
            'TAX-CALCULATION' => 'Tax Calculation'
        );
    }
    
    /**
     * Get calculation history for a specific type
     */
    public function getCalculationHistory($type = null, $limit = 10)
    {
        $query = "SELECT * FROM {$this->table_name} WHERE deleted = 0";
        if ($type) {
            $query .= " AND calculation_type = '" . $GLOBALS['db']->quote($type) . "'";
        }
        $query .= " ORDER BY date_entered DESC LIMIT " . intval($limit);
        
        $result = $GLOBALS['db']->query($query);
        $history = array();
        
        while ($row = $GLOBALS['db']->fetchByAssoc($result)) {
            $history[] = array(
                'id' => $row['id'],
                'name' => $row['name'],
                'type' => $row['calculation_type'],
                'date' => $row['date_entered'],
                'status' => $row['status'],
                'execution_time' => $row['execution_time'],
                'result' => json_decode($row['calculation_result'], true)
            );
        }
        
        return $history;
    }
    
    /**
     * Get calculation statistics
     */
    public function getCalculationStats()
    {
        $stats = array();
        
        // Total calculations
        $query = "SELECT COUNT(*) as total FROM {$this->table_name} WHERE deleted = 0";
        $result = $GLOBALS['db']->query($query);
        $row = $GLOBALS['db']->fetchByAssoc($result);
        $stats['total_calculations'] = $row['total'];
        
        // Calculations by type
        $query = "SELECT calculation_type, COUNT(*) as count FROM {$this->table_name} 
                  WHERE deleted = 0 GROUP BY calculation_type";
        $result = $GLOBALS['db']->query($query);
        $stats['by_type'] = array();
        while ($row = $GLOBALS['db']->fetchByAssoc($result)) {
            $stats['by_type'][$row['calculation_type']] = $row['count'];
        }
        
        // Average execution time
        $query = "SELECT AVG(execution_time) as avg_time FROM {$this->table_name} 
                  WHERE deleted = 0 AND status = 'completed'";
        $result = $GLOBALS['db']->query($query);
        $row = $GLOBALS['db']->fetchByAssoc($result);
        $stats['avg_execution_time'] = round($row['avg_time'], 3);
        
        // Success rate
        $query = "SELECT status, COUNT(*) as count FROM {$this->table_name} 
                  WHERE deleted = 0 GROUP BY status";
        $result = $GLOBALS['db']->query($query);
        $status_counts = array();
        while ($row = $GLOBALS['db']->fetchByAssoc($result)) {
            $status_counts[$row['status']] = $row['count'];
        }
        
        $total = array_sum($status_counts);
        $success = $status_counts['completed'] ?? 0;
        $stats['success_rate'] = $total > 0 ? round(($success / $total) * 100, 2) : 0;
        
        return $stats;
    }
}