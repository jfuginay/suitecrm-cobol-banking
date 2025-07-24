/**
 * Batch Job Monitor
 * Real-time monitoring and management of COBOL batch jobs
 */

var JobMonitor = {
    
    refreshInterval: 5000, // 5 seconds
    refreshTimer: null,
    currentFilter: {
        type: '',
        status: '',
        search: ''
    },
    charts: {},
    
    /**
     * Initialize job monitor
     */
    init: function() {
        console.log('Initializing Batch Job Monitor');
        
        // Load initial data
        this.loadQueueStatus();
        this.loadActiveJobs();
        this.loadJobQueue();
        this.loadJobHistory();
        this.initializeCharts();
        
        // Setup auto-refresh
        this.setupAutoRefresh();
        
        // Event handlers
        this.attachEventHandlers();
    },
    
    /**
     * Load queue status
     */
    loadQueueStatus: function() {
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=getQueueStatus',
            type: 'GET',
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                JobMonitor.updateQueueStatus(data);
            }
        });
    },
    
    /**
     * Update queue status display
     */
    updateQueueStatus: function(data) {
        $('#jobs-running').text(data.running || 0);
        $('#jobs-queued').text(data.queued || 0);
        $('#jobs-scheduled').text(data.scheduled || 0);
        $('#jobs-failed').text(data.failed_today || 0);
        
        // Update capacity bar
        var capacityPercent = (data.running / data.max_concurrent) * 100;
        $('#capacity-fill').css('width', capacityPercent + '%');
        $('#capacity-used').text(data.running);
        $('#capacity-max').text(data.max_concurrent);
        
        // Update color based on capacity
        if (capacityPercent > 80) {
            $('#capacity-fill').addClass('high-load');
        } else if (capacityPercent > 60) {
            $('#capacity-fill').addClass('medium-load');
        } else {
            $('#capacity-fill').removeClass('high-load medium-load');
        }
    },
    
    /**
     * Load active jobs
     */
    loadActiveJobs: function() {
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=getActiveJobs',
            type: 'GET',
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                JobMonitor.displayActiveJobs(data.jobs);
            }
        });
    },
    
    /**
     * Display active jobs
     */
    displayActiveJobs: function(jobs) {
        var tbody = $('#active-jobs-tbody');
        tbody.empty();
        
        if (!jobs || jobs.length === 0) {
            tbody.append('<tr><td colspan="7" class="no-data">No active jobs</td></tr>');
            return;
        }
        
        jobs.forEach(function(job) {
            var progressBar = '<div class="progress-bar">' +
                '<div class="progress-fill" style="width:' + (job.progress || 0) + '%"></div>' +
                '<span class="progress-text">' + (job.progress || 0) + '%</span>' +
                '</div>';
            
            var runtime = JobMonitor.formatRuntime(job.runtime);
            
            var row = '<tr>' +
                '<td>' + job.name + '</td>' +
                '<td>' + job.type + '</td>' +
                '<td><span class="status-badge status-' + job.status + '">' + job.status + '</span></td>' +
                '<td>' + progressBar + '</td>' +
                '<td>' + job.started + '</td>' +
                '<td>' + runtime + '</td>' +
                '<td>' +
                    '<a href="#" onclick="JobMonitor.viewJob(\'' + job.id + '\')">View</a> | ' +
                    '<a href="#" onclick="JobMonitor.pauseJob(\'' + job.id + '\')">Pause</a> | ' +
                    '<a href="#" onclick="JobMonitor.stopJob(\'' + job.id + '\')">Stop</a>' +
                '</td>' +
                '</tr>';
            
            tbody.append(row);
        });
    },
    
    /**
     * Load job queue
     */
    loadJobQueue: function() {
        var params = {
            type: this.currentFilter.type,
            status: this.currentFilter.status,
            search: this.currentFilter.search
        };
        
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=getJobQueue',
            type: 'GET',
            data: params,
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                JobMonitor.displayJobQueue(data.jobs);
            }
        });
    },
    
    /**
     * Display job queue
     */
    displayJobQueue: function(jobs) {
        var tbody = $('#job-queue-tbody');
        tbody.empty();
        
        if (!jobs || jobs.length === 0) {
            tbody.append('<tr><td colspan="8" class="no-data">No jobs found</td></tr>');
            return;
        }
        
        jobs.forEach(function(job) {
            var priorityBadge = '<span class="priority-badge priority-' + job.priority + '">' + 
                                job.priority + '</span>';
            
            var statusBadge = '<span class="status-badge status-' + job.status + '">' + 
                              job.status + '</span>';
            
            var row = '<tr>' +
                '<td>' + priorityBadge + '</td>' +
                '<td>' + job.name + '</td>' +
                '<td>' + job.type + '</td>' +
                '<td>' + (job.schedule || 'On Demand') + '</td>' +
                '<td>' + (job.next_run || '-') + '</td>' +
                '<td>' + (job.last_run || 'Never') + '</td>' +
                '<td>' + statusBadge + '</td>' +
                '<td>' +
                    '<a href="#" onclick="JobMonitor.viewJob(\'' + job.id + '\')">View</a> | ' +
                    '<a href="#" onclick="JobMonitor.runJob(\'' + job.id + '\')">Run</a> | ' +
                    '<a href="#" onclick="JobMonitor.editJob(\'' + job.id + '\')">Edit</a>' +
                '</td>' +
                '</tr>';
            
            tbody.append(row);
        });
    },
    
    /**
     * Load job history
     */
    loadJobHistory: function() {
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=getJobHistory',
            type: 'GET',
            data: { limit: 20 },
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                JobMonitor.displayJobHistory(data.history);
            }
        });
    },
    
    /**
     * Display job history timeline
     */
    displayJobHistory: function(history) {
        var timeline = $('#job-history');
        timeline.empty();
        
        if (!history || history.length === 0) {
            timeline.append('<div class="no-data">No recent job history</div>');
            return;
        }
        
        history.forEach(function(item) {
            var statusClass = item.status === 'completed' ? 'success' : 
                             item.status === 'failed' ? 'error' : 'info';
            
            var timelineItem = '<div class="timeline-item ' + statusClass + '">' +
                '<div class="timeline-time">' + item.time + '</div>' +
                '<div class="timeline-content">' +
                    '<strong>' + item.job_name + '</strong><br>' +
                    'Status: ' + item.status + '<br>' +
                    'Runtime: ' + JobMonitor.formatRuntime(item.runtime) + '<br>' +
                    'Records: ' + (item.records || 0) +
                '</div>' +
                '</div>';
            
            timeline.append(timelineItem);
        });
    },
    
    /**
     * Initialize performance charts
     */
    initializeCharts: function() {
        // Success Rate Chart
        var ctx1 = document.getElementById('job-success-chart').getContext('2d');
        this.charts.success = new Chart(ctx1, {
            type: 'doughnut',
            data: {
                labels: ['Success', 'Failed'],
                datasets: [{
                    data: [95, 5],
                    backgroundColor: ['#27ae60', '#e74c3c']
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
        
        // Runtime Chart
        var ctx2 = document.getElementById('job-runtime-chart').getContext('2d');
        this.charts.runtime = new Chart(ctx2, {
            type: 'line',
            data: {
                labels: ['1h', '2h', '3h', '4h', '5h', '6h'],
                datasets: [{
                    label: 'Avg Runtime (min)',
                    data: [12, 15, 18, 14, 16, 13],
                    borderColor: '#3498db',
                    tension: 0.4
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: { display: false }
                },
                scales: {
                    y: { beginAtZero: true }
                }
            }
        });
        
        // Throughput Chart
        var ctx3 = document.getElementById('job-throughput-chart').getContext('2d');
        this.charts.throughput = new Chart(ctx3, {
            type: 'bar',
            data: {
                labels: ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'],
                datasets: [{
                    label: 'Jobs/Day',
                    data: [45, 52, 48, 55, 50],
                    backgroundColor: '#9b59b6'
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    legend: { display: false }
                },
                scales: {
                    y: { beginAtZero: true }
                }
            }
        });
    },
    
    /**
     * View job details
     */
    viewJob: function(jobId) {
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=getJobDetails',
            type: 'GET',
            data: { id: jobId },
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                JobMonitor.showJobDetails(data.job);
            }
        });
    },
    
    /**
     * Show job details modal
     */
    showJobDetails: function(job) {
        var content = '<div class="job-details">' +
            '<h4>' + job.name + '</h4>' +
            '<table class="details-table">' +
                '<tr><td>Type:</td><td>' + job.type + '</td></tr>' +
                '<tr><td>Status:</td><td>' + job.status + '</td></tr>' +
                '<tr><td>COBOL Program:</td><td>' + job.cobol_program + '</td></tr>' +
                '<tr><td>Schedule:</td><td>' + (job.schedule || 'On Demand') + '</td></tr>' +
                '<tr><td>Last Run:</td><td>' + (job.last_run || 'Never') + '</td></tr>' +
                '<tr><td>Next Run:</td><td>' + (job.next_run || '-') + '</td></tr>' +
                '<tr><td>Run Count:</td><td>' + job.run_count + '</td></tr>' +
                '<tr><td>Success Rate:</td><td>' + job.success_rate + '%</td></tr>' +
                '<tr><td>Avg Runtime:</td><td>' + this.formatRuntime(job.avg_runtime) + '</td></tr>' +
            '</table>';
        
        if (job.parameters) {
            content += '<h5>Parameters</h5>' +
                      '<pre>' + JSON.stringify(job.parameters, null, 2) + '</pre>';
        }
        
        if (job.last_error) {
            content += '<h5>Last Error</h5>' +
                      '<div class="error-message">' + job.last_error + '</div>';
        }
        
        content += '</div>';
        
        $('#job-details-content').html(content);
        $('#job-details-modal').show();
    },
    
    /**
     * Run job immediately
     */
    runJob: function(jobId) {
        if (!confirm('Run this job immediately?')) return;
        
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=runJob',
            type: 'POST',
            data: { id: jobId },
            success: function(response) {
                JobMonitor.showNotification('Job queued for execution', 'success');
                JobMonitor.refresh();
            },
            error: function() {
                JobMonitor.showNotification('Failed to queue job', 'error');
            }
        });
    },
    
    /**
     * Edit job
     */
    editJob: function(jobId) {
        window.location.href = 'index.php?module=Batch_Manager&action=EditView&record=' + jobId;
    },
    
    /**
     * Create new job
     */
    createJob: function() {
        window.location.href = 'index.php?module=Batch_Manager&action=EditView';
    },
    
    /**
     * Pause job
     */
    pauseJob: function(jobId) {
        if (!confirm('Pause this job?')) return;
        
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=pauseJob',
            type: 'POST',
            data: { id: jobId },
            success: function() {
                JobMonitor.showNotification('Job paused', 'info');
                JobMonitor.refresh();
            }
        });
    },
    
    /**
     * Stop job
     */
    stopJob: function(jobId) {
        if (!confirm('Stop this job? This action cannot be undone.')) return;
        
        $.ajax({
            url: 'index.php?module=Batch_Manager&action=stopJob',
            type: 'POST',
            data: { id: jobId },
            success: function() {
                JobMonitor.showNotification('Job stopped', 'warning');
                JobMonitor.refresh();
            }
        });
    },
    
    /**
     * Filter jobs
     */
    filterJobs: function() {
        this.currentFilter.type = $('#filter-type').val();
        this.currentFilter.status = $('#filter-status').val();
        this.loadJobQueue();
    },
    
    /**
     * Search jobs
     */
    searchJobs: function() {
        this.currentFilter.search = $('#search-job').val();
        this.loadJobQueue();
    },
    
    /**
     * Refresh all data
     */
    refresh: function() {
        this.loadQueueStatus();
        this.loadActiveJobs();
        this.loadJobQueue();
        this.loadJobHistory();
    },
    
    /**
     * Setup auto-refresh
     */
    setupAutoRefresh: function() {
        var self = this;
        
        $('#auto-refresh').change(function() {
            if ($(this).is(':checked')) {
                self.startAutoRefresh();
            } else {
                self.stopAutoRefresh();
            }
        });
        
        // Start if checked
        if ($('#auto-refresh').is(':checked')) {
            this.startAutoRefresh();
        }
    },
    
    /**
     * Start auto-refresh
     */
    startAutoRefresh: function() {
        this.refreshTimer = setInterval(function() {
            JobMonitor.refresh();
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
     * Format runtime
     */
    formatRuntime: function(seconds) {
        if (!seconds) return '0s';
        
        var hours = Math.floor(seconds / 3600);
        var minutes = Math.floor((seconds % 3600) / 60);
        var secs = seconds % 60;
        
        if (hours > 0) {
            return hours + 'h ' + minutes + 'm';
        } else if (minutes > 0) {
            return minutes + 'm ' + secs + 's';
        } else {
            return secs + 's';
        }
    },
    
    /**
     * Show notification
     */
    showNotification: function(message, type) {
        var notif = $('<div class="job-notification ' + type + '">' + message + '</div>');
        $('body').append(notif);
        
        setTimeout(function() {
            notif.fadeOut(function() {
                notif.remove();
            });
        }, 3000);
    },
    
    /**
     * Close modal
     */
    closeModal: function() {
        $('#job-details-modal').hide();
    },
    
    /**
     * Attach event handlers
     */
    attachEventHandlers: function() {
        // Close modal on click outside
        $(window).click(function(event) {
            if (event.target.id === 'job-details-modal') {
                JobMonitor.closeModal();
            }
        });
        
        // Handle visibility change
        document.addEventListener('visibilitychange', function() {
            if (document.hidden) {
                JobMonitor.stopAutoRefresh();
            } else if ($('#auto-refresh').is(':checked')) {
                JobMonitor.startAutoRefresh();
            }
        });
    }
};