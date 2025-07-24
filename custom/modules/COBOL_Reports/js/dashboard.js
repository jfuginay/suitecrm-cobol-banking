/**
 * COBOL Reports Analytics Dashboard
 * Real-time data visualization and reporting
 */

var Dashboard = {
    
    charts: {},
    refreshInterval: 30000, // 30 seconds
    refreshTimer: null,
    currentDateRange: 'month',
    wsConnection: null,
    
    /**
     * Initialize dashboard
     */
    init: function() {
        console.log('Initializing COBOL Analytics Dashboard');
        
        // Load initial data
        this.loadMetrics();
        this.initializeCharts();
        
        // Setup WebSocket for real-time updates
        this.connectWebSocket();
        
        // Start auto-refresh
        this.startAutoRefresh();
        
        // Setup event handlers
        this.attachEventHandlers();
    },
    
    /**
     * Load key metrics
     */
    loadMetrics: function() {
        $.ajax({
            url: 'index.php?module=COBOL_Reports&action=getMetrics',
            type: 'GET',
            data: { date_range: this.currentDateRange },
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                Dashboard.updateMetrics(data);
            }
        });
    },
    
    /**
     * Update metric displays
     */
    updateMetrics: function(data) {
        // Animate metric updates
        this.animateValue('total-revenue', data.revenue || 0, '$', true);
        this.animateValue('total-transactions', data.transactions || 0);
        this.animateValue('active-customers', data.customers || 0);
        this.animateValue('compliance-score', data.compliance || 0, '%');
        
        // Update change indicators
        this.updateChangeIndicator('revenue', data.revenue_change);
        this.updateChangeIndicator('transactions', data.transactions_change);
        this.updateChangeIndicator('customers', data.customers_change);
    },
    
    /**
     * Initialize all charts
     */
    initializeCharts: function() {
        // Revenue Trend Chart
        this.charts.revenue = new Chart(document.getElementById('revenue-chart'), {
            type: 'line',
            data: {
                labels: [],
                datasets: [{
                    label: 'Revenue',
                    data: [],
                    borderColor: '#3498db',
                    backgroundColor: 'rgba(52, 152, 219, 0.1)',
                    tension: 0.4
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: { display: false },
                    tooltip: {
                        callbacks: {
                            label: function(context) {
                                return '$' + context.parsed.y.toLocaleString();
                            }
                        }
                    }
                },
                scales: {
                    y: {
                        beginAtZero: true,
                        ticks: {
                            callback: function(value) {
                                return '$' + value.toLocaleString();
                            }
                        }
                    }
                }
            }
        });
        
        // Customer Segments Chart
        this.charts.segments = new Chart(document.getElementById('segments-chart'), {
            type: 'doughnut',
            data: {
                labels: ['Enterprise', 'SMB', 'Individual', 'Government'],
                datasets: [{
                    data: [30, 45, 20, 5],
                    backgroundColor: ['#3498db', '#27ae60', '#f39c12', '#e74c3c']
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: {
                        position: 'bottom'
                    }
                }
            }
        });
        
        // Transaction Types Chart
        this.charts.transactionTypes = new Chart(document.getElementById('transaction-types-chart'), {
            type: 'bar',
            data: {
                labels: ['Deposits', 'Withdrawals', 'Transfers', 'Payments'],
                datasets: [{
                    label: 'Count',
                    data: [120, 80, 95, 150],
                    backgroundColor: '#27ae60'
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: { display: false }
                }
            }
        });
        
        // Risk Distribution Chart
        this.charts.risk = new Chart(document.getElementById('risk-chart'), {
            type: 'radar',
            data: {
                labels: ['Credit Risk', 'Market Risk', 'Operational Risk', 'Liquidity Risk', 'Compliance Risk'],
                datasets: [{
                    label: 'Current',
                    data: [65, 45, 30, 55, 20],
                    borderColor: '#e74c3c',
                    backgroundColor: 'rgba(231, 76, 60, 0.2)'
                }, {
                    label: 'Threshold',
                    data: [70, 70, 70, 70, 70],
                    borderColor: '#95a5a6',
                    backgroundColor: 'rgba(149, 165, 166, 0.1)',
                    borderDash: [5, 5]
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                scales: {
                    r: {
                        beginAtZero: true,
                        max: 100
                    }
                }
            }
        });
        
        // Compliance Status Chart
        this.charts.compliance = new Chart(document.getElementById('compliance-chart'), {
            type: 'polarArea',
            data: {
                labels: ['KYC', 'AML', 'Data Privacy', 'Reporting'],
                datasets: [{
                    data: [95, 88, 92, 98],
                    backgroundColor: [
                        'rgba(39, 174, 96, 0.7)',
                        'rgba(52, 152, 219, 0.7)',
                        'rgba(243, 156, 18, 0.7)',
                        'rgba(155, 89, 182, 0.7)'
                    ]
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                scales: {
                    r: {
                        beginAtZero: true,
                        max: 100
                    }
                }
            }
        });
        
        // Load initial chart data
        this.loadChartData();
    },
    
    /**
     * Load chart data from COBOL reports
     */
    loadChartData: function() {
        $.ajax({
            url: 'index.php?module=COBOL_Reports&action=getChartData',
            type: 'GET',
            data: { 
                date_range: this.currentDateRange,
                charts: ['revenue', 'segments', 'transactions', 'risk', 'compliance']
            },
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                Dashboard.updateAllCharts(data);
            }
        });
    },
    
    /**
     * Update all charts with new data
     */
    updateAllCharts: function(data) {
        // Update Revenue Chart
        if (data.revenue && this.charts.revenue) {
            this.charts.revenue.data.labels = data.revenue.labels;
            this.charts.revenue.data.datasets[0].data = data.revenue.data;
            this.charts.revenue.update();
        }
        
        // Update other charts similarly
        if (data.segments && this.charts.segments) {
            this.charts.segments.data.datasets[0].data = data.segments.data;
            this.charts.segments.update();
        }
        
        if (data.transactions && this.charts.transactionTypes) {
            this.charts.transactionTypes.data.datasets[0].data = data.transactions.data;
            this.charts.transactionTypes.update();
        }
        
        if (data.risk && this.charts.risk) {
            this.charts.risk.data.datasets[0].data = data.risk.data;
            this.charts.risk.update();
        }
        
        if (data.compliance && this.charts.compliance) {
            this.charts.compliance.data.datasets[0].data = data.compliance.data;
            this.charts.compliance.update();
        }
    },
    
    /**
     * Connect to WebSocket for real-time updates
     */
    connectWebSocket: function() {
        try {
            this.wsConnection = new WebSocket('ws://localhost:8080/reports');
            
            this.wsConnection.onopen = function() {
                console.log('WebSocket connected for real-time reports');
            };
            
            this.wsConnection.onmessage = function(event) {
                Dashboard.handleRealtimeUpdate(JSON.parse(event.data));
            };
            
            this.wsConnection.onerror = function(error) {
                console.error('WebSocket error:', error);
            };
            
            this.wsConnection.onclose = function() {
                console.log('WebSocket disconnected, reconnecting...');
                setTimeout(function() {
                    Dashboard.connectWebSocket();
                }, 5000);
            };
            
        } catch (e) {
            console.error('WebSocket connection failed:', e);
        }
    },
    
    /**
     * Handle real-time updates
     */
    handleRealtimeUpdate: function(data) {
        switch(data.type) {
            case 'metric_update':
                this.updateSingleMetric(data.metric, data.value);
                break;
                
            case 'chart_update':
                this.updateSingleChart(data.chart, data.data);
                break;
                
            case 'activity_feed':
                this.addActivityItem(data.activity);
                break;
                
            case 'alert':
                this.showAlert(data.message, data.severity);
                break;
        }
    },
    
    /**
     * Update date range
     */
    updateDateRange: function() {
        this.currentDateRange = $('#date-range').val();
        
        if (this.currentDateRange === 'custom') {
            this.showDateRangePicker();
        } else {
            this.refresh();
        }
    },
    
    /**
     * Refresh dashboard
     */
    refresh: function() {
        // Show loading indicator
        this.showLoading();
        
        // Reload all data
        this.loadMetrics();
        this.loadChartData();
        this.loadActivityFeed();
        
        // Hide loading after delay
        setTimeout(function() {
            Dashboard.hideLoading();
        }, 1000);
    },
    
    /**
     * Start auto-refresh
     */
    startAutoRefresh: function() {
        this.refreshTimer = setInterval(function() {
            Dashboard.refresh();
        }, this.refreshInterval);
    },
    
    /**
     * Stop auto-refresh
     */
    stopAutoRefresh: function() {
        if (this.refreshTimer) {
            clearInterval(this.refreshTimer);
            this.refreshTimer = null;
        }
    },
    
    /**
     * Animate numeric value
     */
    animateValue: function(elementId, value, prefix, format) {
        var element = document.getElementById(elementId);
        var current = parseInt(element.innerText.replace(/[^0-9]/g, '')) || 0;
        var increment = (value - current) / 20;
        var step = 0;
        
        var timer = setInterval(function() {
            current += increment;
            step++;
            
            if (step >= 20) {
                current = value;
                clearInterval(timer);
            }
            
            var display = format ? current.toLocaleString() : Math.round(current);
            element.innerText = (prefix || '') + display;
        }, 50);
    },
    
    /**
     * Update change indicator
     */
    updateChangeIndicator: function(metric, change) {
        var element = $('#' + metric + '-change');
        element.removeClass('positive negative neutral');
        
        if (change > 0) {
            element.addClass('positive').text('+' + change + '%');
        } else if (change < 0) {
            element.addClass('negative').text(change + '%');
        } else {
            element.addClass('neutral').text('No change');
        }
    },
    
    /**
     * Load activity feed
     */
    loadActivityFeed: function() {
        $.ajax({
            url: 'index.php?module=COBOL_Reports&action=getActivityFeed',
            type: 'GET',
            data: { limit: 10 },
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                Dashboard.updateActivityFeed(data.activities);
            }
        });
    },
    
    /**
     * Update activity feed
     */
    updateActivityFeed: function(activities) {
        var feedHtml = '';
        
        activities.forEach(function(activity) {
            feedHtml += '<div class="feed-item">';
            feedHtml += '<span class="feed-time">' + activity.time + '</span>';
            feedHtml += '<span class="feed-text">' + activity.text + '</span>';
            feedHtml += '</div>';
        });
        
        $('#activity-feed').html(feedHtml);
    },
    
    /**
     * Add activity item
     */
    addActivityItem: function(activity) {
        var item = $('<div class="feed-item new">');
        item.append('<span class="feed-time">' + activity.time + '</span>');
        item.append('<span class="feed-text">' + activity.text + '</span>');
        
        $('#activity-feed').prepend(item);
        
        // Remove old items if too many
        if ($('#activity-feed .feed-item').length > 10) {
            $('#activity-feed .feed-item:last').remove();
        }
        
        // Animate new item
        setTimeout(function() {
            item.removeClass('new');
        }, 100);
    },
    
    /**
     * View report
     */
    viewReport: function(reportType) {
        window.location.href = 'index.php?module=COBOL_Reports&action=DetailView&type=' + reportType;
    },
    
    /**
     * Export dashboard
     */
    exportDashboard: function() {
        window.location.href = 'index.php?module=COBOL_Reports&action=exportDashboard&format=pdf';
    },
    
    /**
     * Edit schedule
     */
    editSchedule: function(scheduleId) {
        // Open schedule editor
        window.location.href = 'index.php?module=COBOL_Reports&action=EditView&record=' + scheduleId;
    },
    
    /**
     * Run report now
     */
    runNow: function(reportId) {
        if (confirm('Run this report now?')) {
            $.ajax({
                url: 'index.php?module=COBOL_Reports&action=runReport',
                type: 'POST',
                data: { report_id: reportId },
                success: function(response) {
                    Dashboard.showAlert('Report generation started', 'success');
                    Dashboard.refresh();
                }
            });
        }
    },
    
    /**
     * Show loading indicator
     */
    showLoading: function() {
        $('#analytics-dashboard').addClass('loading');
    },
    
    /**
     * Hide loading indicator
     */
    hideLoading: function() {
        $('#analytics-dashboard').removeClass('loading');
    },
    
    /**
     * Show alert
     */
    showAlert: function(message, severity) {
        var alertClass = severity || 'info';
        var alert = $('<div class="dashboard-alert alert-' + alertClass + '">');
        alert.text(message);
        
        $('body').append(alert);
        
        setTimeout(function() {
            alert.fadeOut(function() {
                alert.remove();
            });
        }, 5000);
    },
    
    /**
     * Update chart type
     */
    updateChartType: function(chartName, newType) {
        if (this.charts[chartName]) {
            this.charts[chartName].config.type = newType;
            this.charts[chartName].update();
        }
    },
    
    /**
     * Attach event handlers
     */
    attachEventHandlers: function() {
        // Handle visibility change
        document.addEventListener('visibilitychange', function() {
            if (document.hidden) {
                Dashboard.stopAutoRefresh();
            } else {
                Dashboard.startAutoRefresh();
                Dashboard.refresh();
            }
        });
        
        // Handle window resize
        $(window).resize(function() {
            Dashboard.resizeCharts();
        });
    },
    
    /**
     * Resize charts
     */
    resizeCharts: function() {
        for (var chartName in this.charts) {
            if (this.charts[chartName]) {
                this.charts[chartName].resize();
            }
        }
    }
};