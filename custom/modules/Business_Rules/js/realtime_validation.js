/**
 * Real-time COBOL Validation for SuiteCRM
 * Provides instant field validation using COBOL business rules
 */

var RealtimeValidation = {
    
    // WebSocket connection for real-time validation
    wsConnection: null,
    wsUrl: 'ws://localhost:8080',
    reconnectInterval: 5000,
    
    // Validation cache to prevent duplicate requests
    validationCache: {},
    cacheTimeout: 60000, // 1 minute
    
    // Current module and rules
    currentModule: null,
    moduleRules: {},
    
    // Validation queue for batch processing
    validationQueue: [],
    queueTimer: null,
    
    /**
     * Initialize real-time validation
     */
    init: function(module) {
        this.currentModule = module;
        console.log('Initializing real-time validation for module:', module);
        
        // Load module rules
        this.loadModuleRules(module);
        
        // Setup WebSocket connection
        this.connectWebSocket();
        
        // Attach validation handlers
        this.attachValidationHandlers();
        
        // Setup validation indicators
        this.setupValidationIndicators();
    },
    
    /**
     * Load validation rules for current module
     */
    loadModuleRules: function(module) {
        $.ajax({
            url: 'index.php?module=Business_Rules&action=getModuleRules',
            type: 'GET',
            data: { target_module: module },
            success: function(response) {
                var data = typeof response === 'string' ? JSON.parse(response) : response;
                RealtimeValidation.moduleRules = data.rules || {};
                RealtimeValidation.applyRulesToForm();
            },
            error: function() {
                console.error('Failed to load validation rules');
            }
        });
    },
    
    /**
     * Connect to WebSocket for real-time validation
     */
    connectWebSocket: function() {
        try {
            this.wsConnection = new WebSocket(this.wsUrl + '/validation');
            
            this.wsConnection.onopen = function() {
                console.log('WebSocket connected for validation');
                RealtimeValidation.updateConnectionStatus('connected');
            };
            
            this.wsConnection.onmessage = function(event) {
                RealtimeValidation.handleValidationResponse(event.data);
            };
            
            this.wsConnection.onerror = function(error) {
                console.error('WebSocket error:', error);
                RealtimeValidation.updateConnectionStatus('error');
            };
            
            this.wsConnection.onclose = function() {
                console.log('WebSocket disconnected');
                RealtimeValidation.updateConnectionStatus('disconnected');
                // Attempt reconnection
                setTimeout(function() {
                    RealtimeValidation.connectWebSocket();
                }, RealtimeValidation.reconnectInterval);
            };
            
        } catch (e) {
            console.error('WebSocket connection failed:', e);
            this.fallbackToAjax();
        }
    },
    
    /**
     * Attach validation handlers to form fields
     */
    attachValidationHandlers: function() {
        // Real-time validation on input
        $(document).on('blur', 'input[type="text"], input[type="number"], select, textarea', function() {
            var field = $(this).attr('name') || $(this).attr('id');
            if (field && RealtimeValidation.hasRulesForField(field)) {
                RealtimeValidation.validateField(this);
            }
        });
        
        // Instant validation for critical fields
        $(document).on('input', 'input[data-instant-validation="true"]', function() {
            var field = $(this).attr('name') || $(this).attr('id');
            if (field) {
                RealtimeValidation.queueValidation(this);
            }
        });
        
        // Form submission validation
        $('form').on('submit', function(e) {
            if (!RealtimeValidation.validateForm(this)) {
                e.preventDefault();
                RealtimeValidation.showValidationSummary();
                return false;
            }
        });
    },
    
    /**
     * Check if field has validation rules
     */
    hasRulesForField: function(fieldName) {
        return this.moduleRules[fieldName] || this.moduleRules['*'];
    },
    
    /**
     * Validate a single field
     */
    validateField: function(fieldElement) {
        var $field = $(fieldElement);
        var fieldName = $field.attr('name') || $field.attr('id');
        var fieldValue = $field.val();
        
        // Check cache first
        var cacheKey = fieldName + ':' + fieldValue;
        if (this.validationCache[cacheKey]) {
            var cachedResult = this.validationCache[cacheKey];
            if (Date.now() - cachedResult.timestamp < this.cacheTimeout) {
                this.applyValidationResult($field, cachedResult.result);
                return;
            }
        }
        
        // Show loading indicator
        this.showFieldLoading($field);
        
        // Get all field values for cross-field validation
        var formData = this.getFormData($field.closest('form'));
        
        // Send validation request
        if (this.wsConnection && this.wsConnection.readyState === WebSocket.OPEN) {
            this.wsConnection.send(JSON.stringify({
                action: 'validate',
                module: this.currentModule,
                field: fieldName,
                value: fieldValue,
                record: formData,
                requestId: this.generateRequestId()
            }));
        } else {
            this.validateViaAjax($field, fieldName, fieldValue, formData);
        }
    },
    
    /**
     * Queue validation for batch processing
     */
    queueValidation: function(fieldElement) {
        var $field = $(fieldElement);
        var fieldName = $field.attr('name') || $field.attr('id');
        
        // Add to queue
        this.validationQueue.push({
            field: $field,
            name: fieldName,
            value: $field.val()
        });
        
        // Clear existing timer
        if (this.queueTimer) {
            clearTimeout(this.queueTimer);
        }
        
        // Set new timer (debounce)
        this.queueTimer = setTimeout(function() {
            RealtimeValidation.processValidationQueue();
        }, 300);
    },
    
    /**
     * Process validation queue
     */
    processValidationQueue: function() {
        if (this.validationQueue.length === 0) return;
        
        // Get unique fields
        var uniqueFields = {};
        this.validationQueue.forEach(function(item) {
            uniqueFields[item.name] = item;
        });
        
        // Validate each field
        for (var fieldName in uniqueFields) {
            this.validateField(uniqueFields[fieldName].field);
        }
        
        // Clear queue
        this.validationQueue = [];
    },
    
    /**
     * Validate via AJAX fallback
     */
    validateViaAjax: function($field, fieldName, fieldValue, formData) {
        $.ajax({
            url: 'index.php?module=Business_Rules&action=validateField',
            type: 'POST',
            data: {
                module: this.currentModule,
                field: fieldName,
                value: fieldValue,
                record: JSON.stringify(formData)
            },
            success: function(response) {
                var result = typeof response === 'string' ? JSON.parse(response) : response;
                RealtimeValidation.applyValidationResult($field, result);
                
                // Cache result
                var cacheKey = fieldName + ':' + fieldValue;
                RealtimeValidation.validationCache[cacheKey] = {
                    result: result,
                    timestamp: Date.now()
                };
            },
            error: function() {
                RealtimeValidation.hideFieldLoading($field);
            }
        });
    },
    
    /**
     * Handle validation response from WebSocket
     */
    handleValidationResponse: function(data) {
        var response = JSON.parse(data);
        var $field = $('[name="' + response.field + '"], [id="' + response.field + '"]');
        
        if ($field.length) {
            this.applyValidationResult($field, response.result);
            
            // Cache result
            var cacheKey = response.field + ':' + response.value;
            this.validationCache[cacheKey] = {
                result: response.result,
                timestamp: Date.now()
            };
        }
    },
    
    /**
     * Apply validation result to field
     */
    applyValidationResult: function($field, result) {
        this.hideFieldLoading($field);
        
        // Remove existing validation classes
        $field.removeClass('validation-error validation-success validation-warning');
        $field.parent().find('.validation-message').remove();
        
        if (!result.valid) {
            // Show error
            $field.addClass('validation-error');
            
            var errorHtml = '<div class="validation-message validation-error-message">';
            result.errors.forEach(function(error) {
                errorHtml += '<span>' + error.message + '</span><br>';
            });
            errorHtml += '</div>';
            
            $field.after(errorHtml);
            
            // Add to form errors
            this.addFormError($field.attr('name') || $field.attr('id'), result.errors);
            
        } else if (result.warnings) {
            // Show warning
            $field.addClass('validation-warning');
            
            var warningHtml = '<div class="validation-message validation-warning-message">';
            result.warnings.forEach(function(warning) {
                warningHtml += '<span>' + warning.message + '</span><br>';
            });
            warningHtml += '</div>';
            
            $field.after(warningHtml);
            
        } else {
            // Show success
            $field.addClass('validation-success');
            this.removeFormError($field.attr('name') || $field.attr('id'));
        }
    },
    
    /**
     * Validate entire form
     */
    validateForm: function(formElement) {
        var $form = $(formElement);
        var isValid = true;
        var validationPromises = [];
        
        // Clear previous errors
        this.clearFormErrors();
        
        // Validate each field with rules
        $form.find('input, select, textarea').each(function() {
            var field = $(this).attr('name') || $(this).attr('id');
            if (field && RealtimeValidation.hasRulesForField(field)) {
                // Create promise for async validation
                var promise = new Promise(function(resolve) {
                    RealtimeValidation.validateField(this);
                    // Wait for validation to complete
                    setTimeout(function() {
                        resolve(!$(this).hasClass('validation-error'));
                    }.bind(this), 500);
                }.bind(this));
                
                validationPromises.push(promise);
            }
        });
        
        // Wait for all validations
        if (validationPromises.length > 0) {
            Promise.all(validationPromises).then(function(results) {
                isValid = results.every(function(result) { return result; });
            });
        }
        
        return isValid;
    },
    
    /**
     * Show field loading indicator
     */
    showFieldLoading: function($field) {
        $field.parent().find('.validation-loader').remove();
        $field.after('<span class="validation-loader"></span>');
    },
    
    /**
     * Hide field loading indicator
     */
    hideFieldLoading: function($field) {
        $field.parent().find('.validation-loader').remove();
    },
    
    /**
     * Setup validation indicators
     */
    setupValidationIndicators: function() {
        // Add validation status bar
        if ($('#validation-status-bar').length === 0) {
            $('body').append(
                '<div id="validation-status-bar">' +
                    '<span class="status-indicator"></span>' +
                    '<span class="status-text">Real-time validation active</span>' +
                '</div>'
            );
        }
    },
    
    /**
     * Update connection status
     */
    updateConnectionStatus: function(status) {
        var $statusBar = $('#validation-status-bar');
        var $indicator = $statusBar.find('.status-indicator');
        var $text = $statusBar.find('.status-text');
        
        $indicator.removeClass('connected disconnected error');
        
        switch(status) {
            case 'connected':
                $indicator.addClass('connected');
                $text.text('Real-time validation active');
                break;
            case 'disconnected':
                $indicator.addClass('disconnected');
                $text.text('Validation offline - using fallback');
                break;
            case 'error':
                $indicator.addClass('error');
                $text.text('Validation error - check connection');
                break;
        }
    },
    
    /**
     * Get form data as object
     */
    getFormData: function($form) {
        var data = {};
        $form.serializeArray().forEach(function(item) {
            data[item.name] = item.value;
        });
        return data;
    },
    
    /**
     * Generate unique request ID
     */
    generateRequestId: function() {
        return 'req_' + Date.now() + '_' + Math.random().toString(36).substr(2, 9);
    },
    
    /**
     * Form error tracking
     */
    formErrors: {},
    
    addFormError: function(field, errors) {
        this.formErrors[field] = errors;
    },
    
    removeFormError: function(field) {
        delete this.formErrors[field];
    },
    
    clearFormErrors: function() {
        this.formErrors = {};
    },
    
    /**
     * Show validation summary
     */
    showValidationSummary: function() {
        var errorCount = Object.keys(this.formErrors).length;
        if (errorCount === 0) return;
        
        var summaryHtml = '<div id="validation-summary" class="alert alert-danger">';
        summaryHtml += '<h4>Please correct the following errors:</h4><ul>';
        
        for (var field in this.formErrors) {
            this.formErrors[field].forEach(function(error) {
                summaryHtml += '<li><strong>' + field + ':</strong> ' + error.message + '</li>';
            });
        }
        
        summaryHtml += '</ul></div>';
        
        // Remove existing summary
        $('#validation-summary').remove();
        
        // Add to form
        $('form').first().prepend(summaryHtml);
        
        // Scroll to top
        $('html, body').animate({ scrollTop: 0 }, 300);
    },
    
    /**
     * Apply rules to form
     */
    applyRulesToForm: function() {
        // Mark fields with instant validation
        for (var field in this.moduleRules) {
            var rules = this.moduleRules[field];
            rules.forEach(function(rule) {
                if (rule.real_time == 1) {
                    $('[name="' + field + '"], [id="' + field + '"]')
                        .attr('data-instant-validation', 'true')
                        .addClass('has-validation');
                }
            });
        }
    },
    
    /**
     * Fallback to AJAX if WebSocket fails
     */
    fallbackToAjax: function() {
        console.log('Using AJAX fallback for validation');
        this.updateConnectionStatus('disconnected');
    }
};

// Auto-initialize based on current module
$(document).ready(function() {
    var currentModule = $('input[name="module"]').val() || 
                       window.module_sugar_grp1 || 
                       SUGAR.ajaxUI.currentModule;
    
    if (currentModule && currentModule !== 'Home') {
        RealtimeValidation.init(currentModule);
    }
});