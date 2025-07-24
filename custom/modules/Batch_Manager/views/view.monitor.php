<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/SugarView.php');

class Batch_ManagerViewMonitor extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }
    
    public function display()
    {
        global $mod_strings, $app_strings;
        
        // Add required resources
        echo '<link rel="stylesheet" type="text/css" href="custom/modules/Batch_Manager/css/monitor.css">';
        echo '<script src="custom/modules/Batch_Manager/js/job_monitor.js"></script>';
        echo '<script src="include/javascript/jquery/jquery-min.js"></script>';
        
        $this->displayJobMonitor();
    }
    
    private function displayJobMonitor()
    {
        global $mod_strings;
        
        echo '<div class="moduleTitle">
                <h2>' . $mod_strings['LBL_BATCH_JOB_MONITOR'] . '</h2>
                <div class="monitor-controls">
                    <button class="button" onclick="JobMonitor.refresh()">
                        <span class="icon-refresh">⟳</span> Refresh
                    </button>
                    <button class="button" onclick="JobMonitor.createJob()">
                        <span class="icon-plus">+</span> New Job
                    </button>
                    <label>
                        <input type="checkbox" id="auto-refresh" checked>
                        Auto-refresh (5s)
                    </label>
                </div>
                <div class="clear"></div>
              </div>';
        
        echo '<div id="batch-monitor-container">';
        
        // Queue Status Overview
        echo '<div class="queue-status-panel">
                <h3>Queue Status</h3>
                <div class="status-metrics">
                    <div class="status-metric">
                        <div class="metric-label">Running</div>
                        <div class="metric-value running" id="jobs-running">0</div>
                    </div>
                    <div class="status-metric">
                        <div class="metric-label">Queued</div>
                        <div class="metric-value queued" id="jobs-queued">0</div>
                    </div>
                    <div class="status-metric">
                        <div class="metric-label">Scheduled</div>
                        <div class="metric-value scheduled" id="jobs-scheduled">0</div>
                    </div>
                    <div class="status-metric">
                        <div class="metric-label">Failed</div>
                        <div class="metric-value failed" id="jobs-failed">0</div>
                    </div>
                </div>
                <div class="queue-capacity">
                    <div class="capacity-bar">
                        <div class="capacity-fill" id="capacity-fill" style="width: 0%"></div>
                    </div>
                    <div class="capacity-text">
                        Capacity: <span id="capacity-used">0</span> / <span id="capacity-max">5</span>
                    </div>
                </div>
              </div>';
        
        // Active Jobs
        echo '<div class="active-jobs-panel">
                <h3>Active Jobs</h3>
                <div id="active-jobs-list">
                    <table class="job-table">
                        <thead>
                            <tr>
                                <th>Job Name</th>
                                <th>Type</th>
                                <th>Status</th>
                                <th>Progress</th>
                                <th>Started</th>
                                <th>Runtime</th>
                                <th>Actions</th>
                            </tr>
                        </thead>
                        <tbody id="active-jobs-tbody">
                            <!-- Active jobs will be loaded here -->
                        </tbody>
                    </table>
                </div>
              </div>';
        
        // Job Queue
        echo '<div class="job-queue-panel">
                <h3>Job Queue</h3>
                <div class="queue-filters">
                    <select id="filter-type" onchange="JobMonitor.filterJobs()">
                        <option value="">All Types</option>
                        <option value="DATA_RECONCILIATION">Data Reconciliation</option>
                        <option value="REPORT_GENERATION">Report Generation</option>
                        <option value="ACCOUNT_PROCESSING">Account Processing</option>
                        <option value="TRANSACTION_BATCH">Transaction Batch</option>
                        <option value="MAINTENANCE_CLEANUP">Maintenance</option>
                    </select>
                    
                    <select id="filter-status" onchange="JobMonitor.filterJobs()">
                        <option value="">All Status</option>
                        <option value="queued">Queued</option>
                        <option value="running">Running</option>
                        <option value="completed">Completed</option>
                        <option value="failed">Failed</option>
                    </select>
                    
                    <input type="text" id="search-job" placeholder="Search jobs..." 
                           onkeyup="JobMonitor.searchJobs()">
                </div>
                
                <div id="job-queue-list">
                    <table class="job-table">
                        <thead>
                            <tr>
                                <th>Priority</th>
                                <th>Job Name</th>
                                <th>Type</th>
                                <th>Schedule</th>
                                <th>Next Run</th>
                                <th>Last Run</th>
                                <th>Status</th>
                                <th>Actions</th>
                            </tr>
                        </thead>
                        <tbody id="job-queue-tbody">
                            <!-- Job queue will be loaded here -->
                        </tbody>
                    </table>
                </div>
              </div>';
        
        // Job History
        echo '<div class="job-history-panel">
                <h3>Recent Job History</h3>
                <div class="history-timeline" id="job-history">
                    <!-- Job history timeline will be loaded here -->
                </div>
              </div>';
        
        // Performance Metrics
        echo '<div class="performance-panel">
                <h3>Performance Metrics</h3>
                <div class="performance-grid">
                    <div class="perf-metric">
                        <canvas id="job-success-chart"></canvas>
                        <div class="metric-label">Success Rate</div>
                    </div>
                    <div class="perf-metric">
                        <canvas id="job-runtime-chart"></canvas>
                        <div class="metric-label">Avg Runtime</div>
                    </div>
                    <div class="perf-metric">
                        <canvas id="job-throughput-chart"></canvas>
                        <div class="metric-label">Throughput</div>
                    </div>
                </div>
              </div>';
        
        // Job Details Modal
        echo '<div id="job-details-modal" class="modal" style="display:none;">
                <div class="modal-content">
                    <div class="modal-header">
                        <h3>Job Details</h3>
                        <span class="close" onclick="JobMonitor.closeModal()">&times;</span>
                    </div>
                    <div class="modal-body" id="job-details-content">
                        <!-- Job details will be loaded here -->
                    </div>
                    <div class="modal-footer">
                        <button class="button" onclick="JobMonitor.closeModal()">Close</button>
                    </div>
                </div>
              </div>';
        
        echo '</div>'; // End container
        
        // Initialize monitor
        echo '<script>
                $(document).ready(function() {
                    JobMonitor.init();
                });
              </script>';
    }
}