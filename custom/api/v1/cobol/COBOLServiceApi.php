<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/api/SugarApi.php');
require_once('include/SugarQuery/SugarQuery.php');

class COBOLServiceApi extends SugarApi
{
    /**
     * Register API endpoints
     */
    public function registerApiRest()
    {
        return array(
            // Financial Calculations
            'calculateEndpoint' => array(
                'reqType' => 'POST',
                'path' => array('cobol', 'calculate'),
                'pathVars' => array('', ''),
                'method' => 'calculate',
                'shortHelp' => 'Execute COBOL financial calculation',
                'longHelp' => 'custom/api/v1/cobol/help/calculate.html',
            ),
            
            // Validation
            'validateEndpoint' => array(
                'reqType' => 'POST',
                'path' => array('cobol', 'validate'),
                'pathVars' => array('', ''),
                'method' => 'validate',
                'shortHelp' => 'Validate field using COBOL business rules',
                'longHelp' => 'custom/api/v1/cobol/help/validate.html',
            ),
            
            // Legacy Data Import
            'importEndpoint' => array(
                'reqType' => 'POST',
                'path' => array('cobol', 'import'),
                'pathVars' => array('', ''),
                'method' => 'importLegacyData',
                'shortHelp' => 'Import legacy data through COBOL converter',
                'longHelp' => 'custom/api/v1/cobol/help/import.html',
            ),
            
            // Legacy Data Export
            'exportEndpoint' => array(
                'reqType' => 'POST',
                'path' => array('cobol', 'export'),
                'pathVars' => array('', ''),
                'method' => 'exportLegacyData',
                'shortHelp' => 'Export data to legacy format',
                'longHelp' => 'custom/api/v1/cobol/help/export.html',
            ),
            
            // Report Generation
            'reportEndpoint' => array(
                'reqType' => 'POST',
                'path' => array('cobol', 'report'),
                'pathVars' => array('', ''),
                'method' => 'generateReport',
                'shortHelp' => 'Generate report using COBOL engine',
                'longHelp' => 'custom/api/v1/cobol/help/report.html',
            ),
            
            // Batch Job Management
            'batchSubmitEndpoint' => array(
                'reqType' => 'POST',
                'path' => array('cobol', 'batch', 'submit'),
                'pathVars' => array('', '', ''),
                'method' => 'submitBatchJob',
                'shortHelp' => 'Submit batch job for processing',
                'longHelp' => 'custom/api/v1/cobol/help/batch_submit.html',
            ),
            
            'batchStatusEndpoint' => array(
                'reqType' => 'GET',
                'path' => array('cobol', 'batch', 'status', '?'),
                'pathVars' => array('', '', '', 'job_id'),
                'method' => 'getBatchStatus',
                'shortHelp' => 'Get batch job status',
                'longHelp' => 'custom/api/v1/cobol/help/batch_status.html',
            ),
            
            // Service Health Check
            'healthEndpoint' => array(
                'reqType' => 'GET',
                'path' => array('cobol', 'health'),
                'pathVars' => array('', ''),
                'method' => 'healthCheck',
                'shortHelp' => 'Check COBOL service health',
                'longHelp' => 'custom/api/v1/cobol/help/health.html',
            ),
            
            // Copybook Parser
            'parseCopybookEndpoint' => array(
                'reqType' => 'POST',
                'path' => array('cobol', 'copybook', 'parse'),
                'pathVars' => array('', '', ''),
                'method' => 'parseCopybook',
                'shortHelp' => 'Parse COBOL copybook definition',
                'longHelp' => 'custom/api/v1/cobol/help/copybook.html',
            ),
            
            // Transaction Stream
            'transactionStreamEndpoint' => array(
                'reqType' => 'GET',
                'path' => array('cobol', 'transactions', 'stream'),
                'pathVars' => array('', '', ''),
                'method' => 'getTransactionStream',
                'shortHelp' => 'Get real-time transaction stream',
                'longHelp' => 'custom/api/v1/cobol/help/stream.html',
            ),
        );
    }
    
    /**
     * Execute COBOL calculation
     */
    public function calculate($api, $args)
    {
        $this->requireArgs($args, array('type', 'parameters'));
        
        // Validate calculation type
        $validTypes = array(
            'LOAN-PAYMENT', 'COMPOUND-INTEREST', 'AMORTIZATION',
            'ROI-CALCULATOR', 'MORTGAGE-CALCULATOR', 'CURRENCY-CONVERSION',
            'RISK-ASSESSMENT', 'TAX-CALCULATION'
        );
        
        if (!in_array($args['type'], $validTypes)) {
            throw new SugarApiExceptionInvalidParameter('Invalid calculation type');
        }
        
        // Load calculator module
        require_once('custom/modules/COBOL_Calculator/COBOL_Calculator.php');
        $calculator = new COBOL_Calculator();
        
        // Execute calculation
        $result = $calculator->executeCalculation($args['type'], $args['parameters']);
        
        // Log API usage
        $this->logApiUsage('calculate', $args['type'], $result['success']);
        
        return array(
            'success' => $result['success'],
            'calculation_id' => $result['calculation_id'] ?? null,
            'result' => $result['result'] ?? null,
            'execution_time' => $result['execution_time'] ?? null,
            'error' => $result['error'] ?? null
        );
    }
    
    /**
     * Validate field value
     */
    public function validate($api, $args)
    {
        $this->requireArgs($args, array('module', 'field', 'value'));
        
        // Load business rules module
        require_once('custom/modules/Business_Rules/Business_Rules.php');
        $validator = new Business_Rules();
        
        // Perform validation
        $result = $validator->validateField(
            $args['module'],
            $args['field'],
            $args['value'],
            $args['record'] ?? array()
        );
        
        // Log API usage
        $this->logApiUsage('validate', $args['module'] . '.' . $args['field'], $result['valid']);
        
        return array(
            'valid' => $result['valid'],
            'errors' => $result['errors'] ?? array(),
            'warnings' => $args['include_warnings'] ? $this->getWarnings($args) : array()
        );
    }
    
    /**
     * Import legacy data
     */
    public function importLegacyData($api, $args)
    {
        $this->requireArgs($args, array('bridge_type', 'data'));
        
        // Validate user has import permission
        if (!$api->user->isAdmin() && !$api->acl->checkAccess('Import', 'access')) {
            throw new SugarApiExceptionNotAuthorized('No access to import data');
        }
        
        // Create temporary file from base64 data
        $tempFile = tempnam(sys_get_temp_dir(), 'cobol_import_');
        file_put_contents($tempFile, base64_decode($args['data']));
        
        try {
            // Load legacy bridge module
            require_once('custom/modules/Legacy_Bridge/Legacy_Bridge.php');
            $bridge = new Legacy_Bridge();
            
            // Set bridge configuration
            $bridge->bridge_type = $args['bridge_type'];
            $bridge->source_format = $args['source_format'] ?? 'FIXED_LENGTH';
            $bridge->copybook_definition = $args['copybook'] ?? '';
            $bridge->field_mappings = json_encode($args['field_mappings'] ?? array());
            $bridge->name = 'API Import - ' . date('Y-m-d H:i:s');
            $bridge->save();
            
            // Execute import
            $result = $bridge->importFromLegacy($tempFile, $args['options'] ?? array());
            
            // Log API usage
            $this->logApiUsage('import', $args['bridge_type'], $result['success']);
            
            return array(
                'success' => $result['success'],
                'bridge_id' => $bridge->id,
                'records_processed' => $result['records_processed'] ?? 0,
                'records_failed' => $result['records_failed'] ?? 0,
                'details' => $result['details'] ?? array(),
                'error' => $result['error'] ?? null
            );
            
        } finally {
            // Clean up temp file
            if (file_exists($tempFile)) {
                unlink($tempFile);
            }
        }
    }
    
    /**
     * Export data to legacy format
     */
    public function exportLegacyData($api, $args)
    {
        $this->requireArgs($args, array('module', 'bridge_type'));
        
        // Validate export permission
        if (!$api->acl->checkAccess($args['module'], 'export')) {
            throw new SugarApiExceptionNotAuthorized('No access to export module data');
        }
        
        // Load legacy bridge module
        require_once('custom/modules/Legacy_Bridge/Legacy_Bridge.php');
        $bridge = new Legacy_Bridge();
        
        // Configure export
        $bridge->bridge_type = $args['bridge_type'];
        $bridge->source_format = $args['target_format'] ?? 'FIXED_LENGTH';
        $bridge->field_mappings = json_encode($args['field_mappings'] ?? array());
        $bridge->name = 'API Export - ' . date('Y-m-d H:i:s');
        $bridge->save();
        
        // Execute export
        $result = $bridge->exportToLegacy($args['module'], $args['filters'] ?? array());
        
        // Log API usage
        $this->logApiUsage('export', $args['module'], $result['success']);
        
        if ($result['success']) {
            // Read exported file
            $content = file_get_contents($result['file_path']);
            
            return array(
                'success' => true,
                'data' => base64_encode($content),
                'format' => $bridge->source_format,
                'encoding' => 'EBCDIC',
                'records_exported' => $result['records_exported'],
                'file_size' => strlen($content)
            );
        } else {
            return array(
                'success' => false,
                'error' => $result['error'] ?? 'Export failed'
            );
        }
    }
    
    /**
     * Generate report
     */
    public function generateReport($api, $args)
    {
        $this->requireArgs($args, array('report_type'));
        
        // Load reports module
        require_once('custom/modules/COBOL_Reports/COBOL_Reports.php');
        $report = new COBOL_Reports();
        
        // Configure report
        $report->report_type = $args['report_type'];
        $report->report_format = $args['format'] ?? 'pdf';
        $report->parameters = json_encode($args['parameters'] ?? array());
        $report->name = $args['report_type'] . ' - ' . date('Y-m-d H:i:s');
        $report->save();
        
        // Generate report
        $result = $report->generateReport();
        
        // Log API usage
        $this->logApiUsage('report', $args['report_type'], $result['success']);
        
        if ($result['success']) {
            // For API, return report data
            if ($args['return_data'] ?? false) {
                $content = file_get_contents($result['output_file']);
                return array(
                    'success' => true,
                    'report_id' => $report->id,
                    'data' => base64_encode($content),
                    'format' => $report->report_format,
                    'execution_time' => $result['execution_time']
                );
            } else {
                return array(
                    'success' => true,
                    'report_id' => $report->id,
                    'download_url' => $this->getDownloadUrl($report->id),
                    'execution_time' => $result['execution_time']
                );
            }
        } else {
            return array(
                'success' => false,
                'error' => $result['error'] ?? 'Report generation failed'
            );
        }
    }
    
    /**
     * Submit batch job
     */
    public function submitBatchJob($api, $args)
    {
        $this->requireArgs($args, array('job_type'));
        
        // Load batch manager
        require_once('custom/modules/Batch_Manager/Batch_Manager.php');
        $job = new Batch_Manager();
        
        // Configure job
        $job->name = $args['name'] ?? $args['job_type'] . ' - API';
        $job->job_type = $args['job_type'];
        $job->parameters = json_encode($args['parameters'] ?? array());
        $job->priority = $args['priority'] ?? 5;
        $job->active = true;
        $job->schedule_type = 'ON_DEMAND';
        $job->save();
        
        // Queue job
        $queued = $job->queueJob();
        
        // Log API usage
        $this->logApiUsage('batch_submit', $args['job_type'], true);
        
        return array(
            'success' => true,
            'job_id' => $job->id,
            'queue_position' => $queued['position'] ?? null,
            'estimated_start' => $queued['estimated_start'] ?? null
        );
    }
    
    /**
     * Get batch job status
     */
    public function getBatchStatus($api, $args)
    {
        $this->requireArgs($args, array('job_id'));
        
        // Load job
        require_once('custom/modules/Batch_Manager/Batch_Manager.php');
        $job = new Batch_Manager();
        $job->retrieve($args['job_id']);
        
        if (!$job->id) {
            throw new SugarApiExceptionNotFound('Job not found');
        }
        
        return array(
            'job_id' => $job->id,
            'name' => $job->name,
            'type' => $job->job_type,
            'status' => $job->status,
            'progress' => $this->getJobProgress($job),
            'last_run' => $job->last_run_date,
            'run_count' => $job->run_count,
            'success_count' => $job->success_count,
            'failure_count' => $job->failure_count,
            'average_runtime' => $job->average_runtime
        );
    }
    
    /**
     * Health check
     */
    public function healthCheck($api, $args)
    {
        $health = array(
            'status' => 'healthy',
            'services' => array(),
            'timestamp' => date('c')
        );
        
        // Check COBOL API
        $cobolStatus = $this->checkCobolService();
        $health['services']['cobol_api'] = $cobolStatus;
        
        // Check database
        $dbStatus = $this->checkDatabase();
        $health['services']['database'] = $dbStatus;
        
        // Check file system
        $fsStatus = $this->checkFileSystem();
        $health['services']['file_system'] = $fsStatus;
        
        // Overall status
        foreach ($health['services'] as $service) {
            if ($service['status'] !== 'healthy') {
                $health['status'] = 'degraded';
                break;
            }
        }
        
        return $health;
    }
    
    /**
     * Parse COBOL copybook
     */
    public function parseCopybook($api, $args)
    {
        $this->requireArgs($args, array('copybook'));
        
        // Load legacy bridge for parsing
        require_once('custom/modules/Legacy_Bridge/Legacy_Bridge.php');
        $bridge = new Legacy_Bridge();
        
        $result = $bridge->parseCopybook($args['copybook']);
        
        if ($result) {
            return array(
                'success' => true,
                'fields' => $result['fields'],
                'record_length' => $result['record_length'],
                'field_count' => count($result['fields'])
            );
        } else {
            return array(
                'success' => false,
                'error' => 'Failed to parse copybook'
            );
        }
    }
    
    /**
     * Get transaction stream
     */
    public function getTransactionStream($api, $args)
    {
        // This would typically return SSE or WebSocket info
        // For REST API, return recent transactions
        
        $limit = $args['limit'] ?? 100;
        $since = $args['since'] ?? date('Y-m-d H:i:s', strtotime('-1 hour'));
        
        // Simulate transaction data
        $transactions = array();
        for ($i = 0; $i < min($limit, 20); $i++) {
            $transactions[] = array(
                'id' => uniqid('trans_'),
                'timestamp' => date('c', strtotime("-$i minutes")),
                'type' => array('DEPOSIT', 'WITHDRAWAL', 'TRANSFER')[rand(0, 2)],
                'amount' => rand(100, 10000),
                'account' => 'ACC' . rand(1000, 9999),
                'status' => 'completed'
            );
        }
        
        return array(
            'transactions' => $transactions,
            'count' => count($transactions),
            'since' => $since,
            'stream_url' => 'ws://localhost:8080/transactions'
        );
    }
    
    /**
     * Check COBOL service health
     */
    private function checkCobolService()
    {
        global $sugar_config;
        $cobol_url = $sugar_config['cobol_api_url'] ?? 'http://localhost:3000';
        
        try {
            $ch = curl_init($cobol_url . '/health');
            curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
            curl_setopt($ch, CURLOPT_TIMEOUT, 5);
            $response = curl_exec($ch);
            $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
            curl_close($ch);
            
            if ($http_code == 200) {
                return array('status' => 'healthy', 'response_time' => curl_getinfo($ch, CURLINFO_TOTAL_TIME));
            } else {
                return array('status' => 'unhealthy', 'error' => 'HTTP ' . $http_code);
            }
        } catch (Exception $e) {
            return array('status' => 'unhealthy', 'error' => $e->getMessage());
        }
    }
    
    /**
     * Check database health
     */
    private function checkDatabase()
    {
        global $db;
        
        try {
            $result = $db->query("SELECT 1");
            if ($result) {
                return array('status' => 'healthy');
            } else {
                return array('status' => 'unhealthy', 'error' => 'Query failed');
            }
        } catch (Exception $e) {
            return array('status' => 'unhealthy', 'error' => $e->getMessage());
        }
    }
    
    /**
     * Check file system health
     */
    private function checkFileSystem()
    {
        $testFile = 'upload/cobol_health_check.txt';
        
        try {
            file_put_contents($testFile, 'health check');
            if (file_exists($testFile)) {
                unlink($testFile);
                return array('status' => 'healthy');
            } else {
                return array('status' => 'unhealthy', 'error' => 'Write failed');
            }
        } catch (Exception $e) {
            return array('status' => 'unhealthy', 'error' => $e->getMessage());
        }
    }
    
    /**
     * Get job progress
     */
    private function getJobProgress($job)
    {
        // Calculate progress based on job type and status
        if ($job->status == 'completed') {
            return 100;
        } elseif ($job->status == 'running') {
            // Would check actual progress from COBOL service
            return rand(10, 90);
        } else {
            return 0;
        }
    }
    
    /**
     * Get download URL for report
     */
    private function getDownloadUrl($reportId)
    {
        global $sugar_config;
        $siteUrl = $sugar_config['site_url'] ?? '';
        return $siteUrl . '/index.php?module=COBOL_Reports&action=download&id=' . $reportId;
    }
    
    /**
     * Log API usage
     */
    private function logApiUsage($endpoint, $type, $success)
    {
        global $db, $current_user;
        
        $data = array(
            'id' => create_guid(),
            'user_id' => $current_user->id,
            'endpoint' => $endpoint,
            'type' => $type,
            'success' => $success ? 1 : 0,
            'date_created' => date('Y-m-d H:i:s')
        );
        
        $db->insertParams('cobol_api_usage', $data);
    }
}