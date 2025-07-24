<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/SugarView.php');

class COBOL_ReportsViewDashboard extends SugarView
{
    public function __construct()
    {
        parent::__construct();
    }
    
    public function display()
    {
        global $mod_strings, $app_strings;
        
        // Add required libraries
        echo '<script src="include/javascript/jquery/jquery-min.js"></script>';
        echo '<script src="https://cdn.jsdelivr.net/npm/chart.js@3.9.1/dist/chart.min.js"></script>';
        echo '<link rel="stylesheet" type="text/css" href="custom/modules/COBOL_Reports/css/dashboard.css">';
        echo '<script src="custom/modules/COBOL_Reports/js/dashboard.js"></script>';
        
        $this->displayAnalyticsDashboard();
    }
    
    private function displayAnalyticsDashboard()
    {
        global $mod_strings;
        
        echo '<div class="moduleTitle">
                <h2>' . $mod_strings['LBL_ANALYTICS_DASHBOARD'] . '</h2>
                <div class="dashboard-controls">
                    <select id="date-range" onchange="Dashboard.updateDateRange()">
                        <option value="today">Today</option>
                        <option value="week">This Week</option>
                        <option value="month" selected>This Month</option>
                        <option value="quarter">This Quarter</option>
                        <option value="year">This Year</option>
                        <option value="custom">Custom Range</option>
                    </select>
                    <button class="button" onclick="Dashboard.refresh()">
                        <span class="refresh-icon">⟳</span> Refresh
                    </button>
                    <button class="button" onclick="Dashboard.exportDashboard()">
                        Export Dashboard
                    </button>
                </div>
                <div class="clear"></div>
              </div>';
        
        // Dashboard Container
        echo '<div id="analytics-dashboard">';
        
        // Key Metrics Row
        echo '<div class="metrics-row">
                <div class="metric-card">
                    <div class="metric-icon revenue-icon">💰</div>
                    <div class="metric-content">
                        <h4>Total Revenue</h4>
                        <div class="metric-value" id="total-revenue">$0</div>
                        <div class="metric-change positive">+12.5%</div>
                    </div>
                </div>
                
                <div class="metric-card">
                    <div class="metric-icon transaction-icon">📊</div>
                    <div class="metric-content">
                        <h4>Transactions</h4>
                        <div class="metric-value" id="total-transactions">0</div>
                        <div class="metric-change positive">+8.3%</div>
                    </div>
                </div>
                
                <div class="metric-card">
                    <div class="metric-icon customer-icon">👥</div>
                    <div class="metric-content">
                        <h4>Active Customers</h4>
                        <div class="metric-value" id="active-customers">0</div>
                        <div class="metric-change positive">+5.2%</div>
                    </div>
                </div>
                
                <div class="metric-card">
                    <div class="metric-icon compliance-icon">✓</div>
                    <div class="metric-content">
                        <h4>Compliance Score</h4>
                        <div class="metric-value" id="compliance-score">0%</div>
                        <div class="metric-change neutral">No change</div>
                    </div>
                </div>
              </div>';
        
        // Charts Row 1
        echo '<div class="charts-row">
                <div class="chart-container large">
                    <div class="chart-header">
                        <h3>Revenue Trend</h3>
                        <div class="chart-actions">
                            <select id="revenue-chart-type" onchange="Dashboard.updateChartType(\'revenue\', this.value)">
                                <option value="line">Line</option>
                                <option value="bar">Bar</option>
                                <option value="area">Area</option>
                            </select>
                        </div>
                    </div>
                    <div class="chart-body">
                        <canvas id="revenue-chart"></canvas>
                    </div>
                </div>
                
                <div class="chart-container medium">
                    <div class="chart-header">
                        <h3>Customer Segments</h3>
                    </div>
                    <div class="chart-body">
                        <canvas id="segments-chart"></canvas>
                    </div>
                </div>
              </div>';
        
        // Charts Row 2
        echo '<div class="charts-row">
                <div class="chart-container medium">
                    <div class="chart-header">
                        <h3>Transaction Types</h3>
                    </div>
                    <div class="chart-body">
                        <canvas id="transaction-types-chart"></canvas>
                    </div>
                </div>
                
                <div class="chart-container medium">
                    <div class="chart-header">
                        <h3>Risk Distribution</h3>
                    </div>
                    <div class="chart-body">
                        <canvas id="risk-chart"></canvas>
                    </div>
                </div>
                
                <div class="chart-container small">
                    <div class="chart-header">
                        <h3>Compliance Status</h3>
                    </div>
                    <div class="chart-body">
                        <canvas id="compliance-chart"></canvas>
                    </div>
                </div>
              </div>';
        
        // Real-time Feed
        echo '<div class="realtime-section">
                <h3>Real-time Activity Feed</h3>
                <div id="activity-feed">
                    <div class="feed-item">
                        <span class="feed-time">2 min ago</span>
                        <span class="feed-text">New transaction: $5,234.50 processed</span>
                    </div>
                    <div class="feed-item">
                        <span class="feed-time">5 min ago</span>
                        <span class="feed-text">Risk assessment completed for Account #1234</span>
                    </div>
                    <div class="feed-item">
                        <span class="feed-time">12 min ago</span>
                        <span class="feed-text">Compliance report generated for Q4</span>
                    </div>
                </div>
              </div>';
        
        // Reports Grid
        echo '<div class="reports-section">
                <h3>Recent Reports</h3>
                <div class="reports-grid">
                    <div class="report-card" onclick="Dashboard.viewReport(\'financial-summary\')">
                        <div class="report-icon">📈</div>
                        <h4>Financial Summary</h4>
                        <p>Monthly P&L Report</p>
                        <span class="report-date">Generated: Today</span>
                    </div>
                    
                    <div class="report-card" onclick="Dashboard.viewReport(\'compliance-audit\')">
                        <div class="report-icon">📋</div>
                        <h4>Compliance Audit</h4>
                        <p>Regulatory Compliance</p>
                        <span class="report-date">Generated: Yesterday</span>
                    </div>
                    
                    <div class="report-card" onclick="Dashboard.viewReport(\'risk-assessment\')">
                        <div class="report-icon">⚠️</div>
                        <h4>Risk Assessment</h4>
                        <p>Portfolio Risk Analysis</p>
                        <span class="report-date">Generated: 3 days ago</span>
                    </div>
                    
                    <div class="report-card" onclick="Dashboard.viewReport(\'customer-analytics\')">
                        <div class="report-icon">👥</div>
                        <h4>Customer Analytics</h4>
                        <p>Behavior Analysis</p>
                        <span class="report-date">Generated: This week</span>
                    </div>
                </div>
              </div>';
        
        // Scheduled Reports
        echo '<div class="scheduled-reports">
                <h3>Scheduled Reports</h3>
                <table class="scheduled-table">
                    <thead>
                        <tr>
                            <th>Report Name</th>
                            <th>Type</th>
                            <th>Schedule</th>
                            <th>Next Run</th>
                            <th>Actions</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td>Daily Transaction Summary</td>
                            <td>Transaction Analysis</td>
                            <td>Daily at 6:00 AM</td>
                            <td>Tomorrow 6:00 AM</td>
                            <td>
                                <a href="#" onclick="Dashboard.editSchedule(1)">Edit</a> |
                                <a href="#" onclick="Dashboard.runNow(1)">Run Now</a>
                            </td>
                        </tr>
                        <tr>
                            <td>Weekly Compliance Check</td>
                            <td>Compliance Report</td>
                            <td>Weekly on Monday</td>
                            <td>Next Monday</td>
                            <td>
                                <a href="#" onclick="Dashboard.editSchedule(2)">Edit</a> |
                                <a href="#" onclick="Dashboard.runNow(2)">Run Now</a>
                            </td>
                        </tr>
                        <tr>
                            <td>Monthly Financial Report</td>
                            <td>Financial Summary</td>
                            <td>Monthly on 1st</td>
                            <td>Next Month</td>
                            <td>
                                <a href="#" onclick="Dashboard.editSchedule(3)">Edit</a> |
                                <a href="#" onclick="Dashboard.runNow(3)">Run Now</a>
                            </td>
                        </tr>
                    </tbody>
                </table>
              </div>';
        
        echo '</div>'; // End dashboard container
        
        // Initialize Dashboard
        echo '<script>
                $(document).ready(function() {
                    Dashboard.init();
                });
              </script>';
    }
}