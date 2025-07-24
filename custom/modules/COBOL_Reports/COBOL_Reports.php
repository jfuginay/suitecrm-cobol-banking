<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('data/SugarBean.php');
require_once('include/utils.php');

class COBOL_Reports extends SugarBean
{
    public $module_dir = 'COBOL_Reports';
    public $object_name = 'COBOL_Reports';
    public $table_name = 'cobol_reports';
    public $new_schema = true;
    public $module_name = 'COBOL_Reports';
    
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $description;
    public $deleted;
    public $report_type;
    public $report_module;
    public $report_format;
    public $cobol_program;
    public $parameters;
    public $schedule_type;
    public $schedule_time;
    public $last_run_date;
    public $next_run_date;
    public $status;
    public $output_file;
    public $execution_time;
    public $records_processed;
    public $chart_type;
    public $chart_config;
    public $dashboard_position;
    public $refresh_interval;
    
    public function COBOL_Reports()
    {
        parent::__construct();
    }
    
    /**
     * Generate report using COBOL
     */
    public function generateReport($parameters = array())
    {
        $start_time = microtime(true);
        
        // Update status
        $this->status = 'processing';
        $this->save();
        
        try {
            // Merge parameters
            $allParams = array_merge(
                json_decode($this->parameters, true) ?? array(),
                $parameters
            );
            
            // Prepare data based on report type
            $reportData = $this->prepareReportData($allParams);
            
            // Execute COBOL report generation
            $result = $this->executeCOBOLReport($reportData);
            
            // Process and save results
            $this->processReportResults($result);
            
            // Update status
            $this->status = 'completed';
            $this->last_run_date = date('Y-m-d H:i:s');
            $this->execution_time = microtime(true) - $start_time;
            $this->save();
            
            return array(
                'success' => true,
                'report_id' => $this->id,
                'output_file' => $this->output_file,
                'execution_time' => $this->execution_time,
                'records_processed' => $this->records_processed
            );
            
        } catch (Exception $e) {
            $this->status = 'error';
            $this->error_log = $e->getMessage();
            $this->save();
            
            return array(
                'success' => false,
                'error' => $e->getMessage()
            );
        }
    }
    
    /**
     * Prepare report data based on type
     */
    private function prepareReportData($parameters)
    {
        global $db;
        
        $data = array();
        
        switch ($this->report_type) {
            case 'FINANCIAL_SUMMARY':
                $data = $this->getFinancialSummaryData($parameters);
                break;
                
            case 'TRANSACTION_ANALYSIS':
                $data = $this->getTransactionAnalysisData($parameters);
                break;
                
            case 'COMPLIANCE_REPORT':
                $data = $this->getComplianceReportData($parameters);
                break;
                
            case 'CUSTOMER_ANALYTICS':
                $data = $this->getCustomerAnalyticsData($parameters);
                break;
                
            case 'RISK_ASSESSMENT':
                $data = $this->getRiskAssessmentData($parameters);
                break;
                
            case 'REGULATORY_FILING':
                $data = $this->getRegulatoryFilingData($parameters);
                break;
                
            case 'PERFORMANCE_METRICS':
                $data = $this->getPerformanceMetricsData($parameters);
                break;
                
            case 'CUSTOM_REPORT':
                $data = $this->getCustomReportData($parameters);
                break;
        }
        
        return $data;
    }
    
    /**
     * Execute COBOL report generation
     */
    private function executeCOBOLReport($data)
    {
        $cobol_api_url = $this->getCobolApiUrl();
        $endpoint = $cobol_api_url . '/reports/generate';
        
        $payload = array(
            'program' => $this->cobol_program,
            'report_type' => $this->report_type,
            'format' => $this->report_format,
            'data' => $data,
            'parameters' => json_decode($this->parameters, true)
        );
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Accept: application/json'
        ));
        curl_setopt($ch, CURLOPT_TIMEOUT, 600); // 10 minute timeout
        
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($http_code == 200 && $response) {
            return json_decode($response, true);
        } else {
            throw new Exception('COBOL report generation failed');
        }
    }
    
    /**
     * Process report results
     */
    private function processReportResults($result)
    {
        // Save report file
        $reportDir = 'upload/cobol_reports/' . date('Y/m');
        if (!is_dir($reportDir)) {
            mkdir($reportDir, 0755, true);
        }
        
        $filename = $this->id . '_' . date('YmdHis') . '.' . $this->report_format;
        $filepath = $reportDir . '/' . $filename;
        
        // Save based on format
        switch ($this->report_format) {
            case 'pdf':
                file_put_contents($filepath, base64_decode($result['content']));
                break;
                
            case 'excel':
                file_put_contents($filepath, base64_decode($result['content']));
                break;
                
            case 'csv':
                file_put_contents($filepath, $result['content']);
                break;
                
            case 'json':
                file_put_contents($filepath, json_encode($result['data'], JSON_PRETTY_PRINT));
                break;
                
            case 'html':
                file_put_contents($filepath, $result['content']);
                break;
        }
        
        $this->output_file = $filepath;
        $this->records_processed = $result['record_count'] ?? 0;
        
        // Store chart data if applicable
        if (isset($result['chart_data'])) {
            $this->chart_config = json_encode($result['chart_data']);
        }
    }
    
    /**
     * Get financial summary data
     */
    private function getFinancialSummaryData($parameters)
    {
        global $db;
        
        $startDate = $parameters['start_date'] ?? date('Y-m-01');
        $endDate = $parameters['end_date'] ?? date('Y-m-t');
        
        // Revenue data
        $revenueQuery = "SELECT 
            DATE_FORMAT(date_closed, '%Y-%m') as month,
            SUM(amount) as revenue,
            COUNT(*) as deals
            FROM opportunities
            WHERE deleted = 0 
            AND sales_stage = 'Closed Won'
            AND date_closed BETWEEN '$startDate' AND '$endDate'
            GROUP BY DATE_FORMAT(date_closed, '%Y-%m')";
        
        $revenueResult = $db->query($revenueQuery);
        $revenueData = array();
        while ($row = $db->fetchByAssoc($revenueResult)) {
            $revenueData[] = $row;
        }
        
        // Account balances
        $balanceQuery = "SELECT 
            account_type,
            COUNT(*) as count,
            SUM(annual_revenue) as total_revenue
            FROM accounts
            WHERE deleted = 0
            GROUP BY account_type";
        
        $balanceResult = $db->query($balanceQuery);
        $balanceData = array();
        while ($row = $db->fetchByAssoc($balanceResult)) {
            $balanceData[] = $row;
        }
        
        return array(
            'revenue' => $revenueData,
            'balances' => $balanceData,
            'period' => array(
                'start' => $startDate,
                'end' => $endDate
            )
        );
    }
    
    /**
     * Get transaction analysis data
     */
    private function getTransactionAnalysisData($parameters)
    {
        // Simulate transaction data
        return array(
            'transactions' => array(
                array('date' => '2024-01-15', 'type' => 'DEPOSIT', 'amount' => 5000),
                array('date' => '2024-01-16', 'type' => 'WITHDRAWAL', 'amount' => 1200),
                array('date' => '2024-01-17', 'type' => 'TRANSFER', 'amount' => 3000),
            ),
            'summary' => array(
                'total_deposits' => 150000,
                'total_withdrawals' => 45000,
                'net_flow' => 105000
            )
        );
    }
    
    /**
     * Get dashboard data for real-time display
     */
    public function getDashboardData()
    {
        if ($this->chart_config) {
            return json_decode($this->chart_config, true);
        }
        
        // Generate fresh data if needed
        if ($this->shouldRefresh()) {
            $this->generateReport();
            return json_decode($this->chart_config, true);
        }
        
        return array();
    }
    
    /**
     * Check if report should refresh
     */
    private function shouldRefresh()
    {
        if (!$this->last_run_date) return true;
        
        $lastRun = strtotime($this->last_run_date);
        $now = time();
        $interval = $this->refresh_interval ?? 3600; // Default 1 hour
        
        return ($now - $lastRun) > $interval;
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
     * Get available report types
     */
    public static function getReportTypes()
    {
        return array(
            'FINANCIAL_SUMMARY' => 'Financial Summary Report',
            'TRANSACTION_ANALYSIS' => 'Transaction Analysis',
            'COMPLIANCE_REPORT' => 'Compliance Report',
            'CUSTOMER_ANALYTICS' => 'Customer Analytics',
            'RISK_ASSESSMENT' => 'Risk Assessment Report',
            'REGULATORY_FILING' => 'Regulatory Filing',
            'PERFORMANCE_METRICS' => 'Performance Metrics',
            'CUSTOM_REPORT' => 'Custom Report'
        );
    }
    
    /**
     * Get available COBOL report programs
     */
    public static function getCOBOLPrograms()
    {
        return array(
            'FINANCIAL-REPORTER' => 'Financial Report Generator',
            'TRANSACTION-ANALYZER' => 'Transaction Analysis Engine',
            'COMPLIANCE-CHECKER' => 'Compliance Report Generator',
            'ANALYTICS-ENGINE' => 'Analytics Processing Engine',
            'RISK-CALCULATOR' => 'Risk Assessment Calculator',
            'REGULATORY-FORMATTER' => 'Regulatory Report Formatter'
        );
    }
    
    /**
     * Get chart types
     */
    public static function getChartTypes()
    {
        return array(
            'line' => 'Line Chart',
            'bar' => 'Bar Chart',
            'pie' => 'Pie Chart',
            'doughnut' => 'Doughnut Chart',
            'radar' => 'Radar Chart',
            'area' => 'Area Chart',
            'scatter' => 'Scatter Plot',
            'bubble' => 'Bubble Chart'
        );
    }
}