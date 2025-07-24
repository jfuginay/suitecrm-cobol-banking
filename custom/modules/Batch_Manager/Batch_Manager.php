<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('data/SugarBean.php');
require_once('include/utils.php');

class Batch_Manager extends SugarBean
{
    public $module_dir = 'Batch_Manager';
    public $object_name = 'Batch_Manager';
    public $table_name = 'batch_manager';
    public $new_schema = true;
    public $module_name = 'Batch_Manager';
    
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $description;
    public $deleted;
    public $job_type;
    public $cobol_program;
    public $schedule_type;
    public $schedule_config;
    public $priority;
    public $max_runtime;
    public $retry_count;
    public $retry_delay;
    public $dependencies;
    public $parameters;
    public $status;
    public $last_run_date;
    public $next_run_date;
    public $run_count;
    public $success_count;
    public $failure_count;
    public $average_runtime;
    public $notification_config;
    public $active;
    
    // Job queue management
    private static $jobQueue = array();
    private static $runningJobs = array();
    private static $maxConcurrentJobs = 5;
    
    public function Batch_Manager()
    {
        parent::__construct();
    }
    
    /**
     * Schedule a batch job
     */
    public function scheduleJob()
    {
        if (!$this->active) {
            return array('success' => false, 'error' => 'Job is not active');
        }
        
        // Calculate next run date based on schedule
        $nextRun = $this->calculateNextRun();
        
        if ($nextRun) {
            $this->next_run_date = $nextRun;
            $this->save();
            
            // Add to scheduler if immediate
            if (strtotime($nextRun) <= time()) {
                $this->queueJob();
            }
            
            return array(
                'success' => true,
                'next_run' => $nextRun
            );
        }
        
        return array('success' => false, 'error' => 'Unable to schedule job');
    }
    
    /**
     * Queue job for execution
     */
    public function queueJob($params = array())
    {
        $job = array(
            'id' => $this->id,
            'name' => $this->name,
            'type' => $this->job_type,
            'program' => $this->cobol_program,
            'priority' => $this->priority,
            'parameters' => array_merge(
                json_decode($this->parameters, true) ?? array(),
                $params
            ),
            'queued_at' => time(),
            'status' => 'queued'
        );
        
        // Check dependencies
        if ($this->hasPendingDependencies()) {
            $job['status'] = 'waiting';
        }
        
        // Add to queue
        self::$jobQueue[] = $job;
        
        // Sort by priority
        usort(self::$jobQueue, function($a, $b) {
            return $a['priority'] - $b['priority'];
        });
        
        return $job;
    }
    
    /**
     * Execute batch job
     */
    public function executeJob($jobData = null)
    {
        $startTime = microtime(true);
        
        // Update status
        $this->status = 'running';
        $this->save();
        
        try {
            // Prepare execution context
            $context = $this->prepareExecutionContext($jobData);
            
            // Execute COBOL program
            $result = $this->executeCOBOLBatch($context);
            
            // Process results
            $this->processJobResults($result);
            
            // Update statistics
            $executionTime = microtime(true) - $startTime;
            $this->updateJobStatistics(true, $executionTime);
            
            // Handle post-execution
            $this->handlePostExecution($result);
            
            return array(
                'success' => true,
                'execution_time' => $executionTime,
                'records_processed' => $result['records_processed'] ?? 0,
                'output' => $result['output'] ?? null
            );
            
        } catch (Exception $e) {
            // Handle failure
            $this->status = 'failed';
            $this->save();
            
            $this->updateJobStatistics(false, microtime(true) - $startTime);
            
            // Retry logic
            if ($this->shouldRetry()) {
                $this->scheduleRetry();
            }
            
            // Send failure notification
            $this->sendNotification('failure', $e->getMessage());
            
            return array(
                'success' => false,
                'error' => $e->getMessage()
            );
        }
    }
    
    /**
     * Prepare execution context
     */
    private function prepareExecutionContext($jobData)
    {
        $context = array(
            'job_id' => $this->id,
            'job_type' => $this->job_type,
            'parameters' => json_decode($this->parameters, true) ?? array(),
            'runtime_limit' => $this->max_runtime,
            'execution_date' => date('Y-m-d H:i:s')
        );
        
        // Merge with runtime parameters
        if ($jobData && isset($jobData['parameters'])) {
            $context['parameters'] = array_merge(
                $context['parameters'],
                $jobData['parameters']
            );
        }
        
        // Load job-specific data
        switch ($this->job_type) {
            case 'DATA_RECONCILIATION':
                $context['data'] = $this->loadReconciliationData();
                break;
                
            case 'REPORT_GENERATION':
                $context['data'] = $this->loadReportData();
                break;
                
            case 'ACCOUNT_PROCESSING':
                $context['data'] = $this->loadAccountData();
                break;
                
            case 'TRANSACTION_BATCH':
                $context['data'] = $this->loadTransactionData();
                break;
                
            case 'MAINTENANCE_CLEANUP':
                $context['data'] = $this->loadMaintenanceTargets();
                break;
                
            case 'DATA_EXPORT':
                $context['data'] = $this->loadExportData();
                break;
        }
        
        return $context;
    }
    
    /**
     * Execute COBOL batch program
     */
    private function executeCOBOLBatch($context)
    {
        $cobol_api_url = $this->getCobolApiUrl();
        $endpoint = $cobol_api_url . '/batch/execute';
        
        $payload = array(
            'program' => $this->cobol_program,
            'job_type' => $this->job_type,
            'context' => $context,
            'timeout' => $this->max_runtime ?? 3600
        );
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Accept: application/json'
        ));
        curl_setopt($ch, CURLOPT_TIMEOUT, $this->max_runtime ?? 3600);
        
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($http_code == 200 && $response) {
            return json_decode($response, true);
        } else {
            throw new Exception('COBOL batch execution failed: HTTP ' . $http_code);
        }
    }
    
    /**
     * Process job results
     */
    private function processJobResults($result)
    {
        // Update status
        $this->status = 'completed';
        $this->last_run_date = date('Y-m-d H:i:s');
        
        // Store results
        $resultFile = 'upload/batch_results/' . $this->id . '_' . date('YmdHis') . '.json';
        if (!is_dir('upload/batch_results')) {
            mkdir('upload/batch_results', 0755, true);
        }
        file_put_contents($resultFile, json_encode($result, JSON_PRETTY_PRINT));
        
        // Log execution
        $this->logExecution(array(
            'status' => 'completed',
            'records_processed' => $result['records_processed'] ?? 0,
            'execution_time' => $result['execution_time'] ?? 0,
            'result_file' => $resultFile
        ));
        
        $this->save();
    }
    
    /**
     * Update job statistics
     */
    private function updateJobStatistics($success, $executionTime)
    {
        $this->run_count = ($this->run_count ?? 0) + 1;
        
        if ($success) {
            $this->success_count = ($this->success_count ?? 0) + 1;
        } else {
            $this->failure_count = ($this->failure_count ?? 0) + 1;
        }
        
        // Calculate average runtime
        if ($this->average_runtime) {
            $this->average_runtime = (($this->average_runtime * ($this->run_count - 1)) + $executionTime) / $this->run_count;
        } else {
            $this->average_runtime = $executionTime;
        }
        
        // Calculate next run
        $this->next_run_date = $this->calculateNextRun();
        
        $this->save();
    }
    
    /**
     * Calculate next run date
     */
    private function calculateNextRun()
    {
        $schedule = json_decode($this->schedule_config, true);
        if (!$schedule) return null;
        
        $now = time();
        $next = null;
        
        switch ($this->schedule_type) {
            case 'CRON':
                // Parse cron expression
                $cron = new CronExpression($schedule['expression']);
                $next = $cron->getNextRunDate()->format('Y-m-d H:i:s');
                break;
                
            case 'INTERVAL':
                $interval = $schedule['interval'] ?? 3600;
                $next = date('Y-m-d H:i:s', $now + $interval);
                break;
                
            case 'DAILY':
                $time = $schedule['time'] ?? '00:00';
                $next = date('Y-m-d ' . $time . ':00', strtotime('+1 day'));
                break;
                
            case 'WEEKLY':
                $day = $schedule['day'] ?? 1;
                $time = $schedule['time'] ?? '00:00';
                $next = date('Y-m-d ' . $time . ':00', strtotime('next ' . $this->getDayName($day)));
                break;
                
            case 'MONTHLY':
                $day = $schedule['day'] ?? 1;
                $time = $schedule['time'] ?? '00:00';
                $next = date('Y-m-' . sprintf('%02d', $day) . ' ' . $time . ':00', strtotime('+1 month'));
                break;
                
            case 'ON_DEMAND':
                // No automatic scheduling
                $next = null;
                break;
        }
        
        return $next;
    }
    
    /**
     * Check if job has pending dependencies
     */
    private function hasPendingDependencies()
    {
        if (!$this->dependencies) return false;
        
        $deps = json_decode($this->dependencies, true);
        if (!$deps || !is_array($deps)) return false;
        
        global $db;
        
        foreach ($deps as $depId) {
            $query = "SELECT status FROM {$this->table_name} 
                      WHERE id = '{$depId}' AND deleted = 0";
            $result = $db->query($query);
            $row = $db->fetchByAssoc($result);
            
            if (!$row || $row['status'] !== 'completed') {
                return true;
            }
        }
        
        return false;
    }
    
    /**
     * Handle post-execution tasks
     */
    private function handlePostExecution($result)
    {
        // Send success notification
        $this->sendNotification('success', $result);
        
        // Trigger dependent jobs
        $this->triggerDependentJobs();
        
        // Clean up old results
        $this->cleanupOldResults();
    }
    
    /**
     * Send notification
     */
    private function sendNotification($type, $data)
    {
        $config = json_decode($this->notification_config, true);
        if (!$config || !isset($config[$type])) return;
        
        $notification = $config[$type];
        
        // Email notification
        if (isset($notification['email'])) {
            $this->sendEmailNotification($notification['email'], $type, $data);
        }
        
        // Webhook notification
        if (isset($notification['webhook'])) {
            $this->sendWebhookNotification($notification['webhook'], $type, $data);
        }
    }
    
    /**
     * Get job status for monitoring
     */
    public static function getJobQueueStatus()
    {
        global $db;
        
        // Get running jobs
        $runningQuery = "SELECT COUNT(*) as count FROM batch_manager 
                        WHERE status = 'running' AND deleted = 0";
        $result = $db->query($runningQuery);
        $row = $db->fetchByAssoc($result);
        $running = $row['count'];
        
        // Get queued jobs
        $queuedQuery = "SELECT COUNT(*) as count FROM batch_manager 
                       WHERE status = 'queued' AND deleted = 0";
        $result = $db->query($queuedQuery);
        $row = $db->fetchByAssoc($result);
        $queued = $row['count'];
        
        // Get scheduled jobs
        $scheduledQuery = "SELECT COUNT(*) as count FROM batch_manager 
                          WHERE active = 1 AND next_run_date > NOW() AND deleted = 0";
        $result = $db->query($scheduledQuery);
        $row = $db->fetchByAssoc($result);
        $scheduled = $row['count'];
        
        return array(
            'running' => $running,
            'queued' => $queued,
            'scheduled' => $scheduled,
            'max_concurrent' => self::$maxConcurrentJobs,
            'queue_length' => count(self::$jobQueue)
        );
    }
    
    /**
     * Process job queue (called by scheduler)
     */
    public static function processJobQueue()
    {
        global $db;
        
        // Get jobs ready to run
        $query = "SELECT * FROM batch_manager 
                  WHERE active = 1 
                  AND (next_run_date <= NOW() OR status = 'queued')
                  AND status NOT IN ('running', 'completed')
                  AND deleted = 0
                  ORDER BY priority ASC, next_run_date ASC
                  LIMIT " . self::$maxConcurrentJobs;
        
        $result = $db->query($query);
        
        while ($row = $db->fetchByAssoc($result)) {
            $job = new Batch_Manager();
            $job->retrieve($row['id']);
            
            // Check if can run
            if (count(self::$runningJobs) < self::$maxConcurrentJobs) {
                self::$runningJobs[$job->id] = true;
                
                // Execute in background
                $job->executeJob();
                
                unset(self::$runningJobs[$job->id]);
            }
        }
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
     * Get available job types
     */
    public static function getJobTypes()
    {
        return array(
            'DATA_RECONCILIATION' => 'Data Reconciliation',
            'REPORT_GENERATION' => 'Report Generation',
            'ACCOUNT_PROCESSING' => 'Account Processing',
            'TRANSACTION_BATCH' => 'Transaction Batch Processing',
            'MAINTENANCE_CLEANUP' => 'Maintenance & Cleanup',
            'DATA_EXPORT' => 'Data Export',
            'COMPLIANCE_CHECK' => 'Compliance Check',
            'BACKUP_ARCHIVE' => 'Backup & Archive',
            'CUSTOM_BATCH' => 'Custom Batch Job'
        );
    }
    
    /**
     * Get COBOL batch programs
     */
    public static function getCOBOLPrograms()
    {
        return array(
            'BATCH-RECONCILE' => 'Batch Reconciliation Processor',
            'BATCH-REPORT' => 'Batch Report Generator',
            'BATCH-ACCOUNT' => 'Account Batch Processor',
            'BATCH-TRANSACTION' => 'Transaction Batch Processor',
            'BATCH-MAINTENANCE' => 'Maintenance Job Processor',
            'BATCH-EXPORT' => 'Data Export Processor',
            'BATCH-CUSTOM' => 'Custom Batch Processor'
        );
    }
}