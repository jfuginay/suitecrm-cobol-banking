/**
 * Legacy Bridge JavaScript Module
 * Handles UI interactions for legacy COBOL data import/export
 */

var LegacyBridge = {
    currentStep: 1,
    maxSteps: 4,
    importData: {
        bridgeType: '',
        sourceSystem: '',
        sourceFormat: '',
        encoding: '',
        fileName: '',
        copybook: '',
        targetModule: '',
        fieldMappings: {},
        transformationRules: [],
        fileContent: null
    },
    
    init: function() {
        console.log('Legacy Bridge initialized');
        this.attachEventHandlers();
    },
    
    attachEventHandlers: function() {
        $('#copybook_upload').change(function() {
            $('#copybook_file').show();
            $('#copybook_editor').hide();
        });
        
        $('#copybook_manual').change(function() {
            $('#copybook_file').hide();
            $('#copybook_editor').show();
        });
        
        $('#copybook_template').change(function() {
            $('#copybook_file').hide();
            $('#copybook_editor').show();
            LegacyBridge.loadCopybookTemplate();
        });
        
        $('#legacy_file').change(function(e) {
            LegacyBridge.handleFileUpload(e);
        });
    },
    
    nextStep: function() {
        if (!this.validateStep(this.currentStep)) {
            return;
        }
        
        this.saveStepData(this.currentStep);
        
        if (this.currentStep < this.maxSteps) {
            this.currentStep++;
            this.showStep(this.currentStep);
            
            // Special handling for step transitions
            if (this.currentStep === 3) {
                this.initializeFieldMapping();
            } else if (this.currentStep === 4) {
                this.prepareImportSummary();
            }
        }
    },
    
    previousStep: function() {
        if (this.currentStep > 1) {
            this.currentStep--;
            this.showStep(this.currentStep);
        }
    },
    
    showStep: function(step) {
        // Hide all steps
        $('.wizard-content').hide();
        $('#step-' + step).show();
        
        // Update step indicators
        $('.wizard-steps .step').removeClass('active completed');
        for (var i = 1; i < step; i++) {
            $('.wizard-steps .step[data-step="' + i + '"]').addClass('completed');
        }
        $('.wizard-steps .step[data-step="' + step + '"]').addClass('active');
        
        // Update navigation buttons
        if (step === 1) {
            $('#btn-previous').hide();
        } else {
            $('#btn-previous').show();
        }
        
        if (step === this.maxSteps) {
            $('#btn-next').hide();
            $('#btn-import').show();
        } else {
            $('#btn-next').show();
            $('#btn-import').hide();
        }
    },
    
    validateStep: function(step) {
        switch(step) {
            case 1:
                if (!$('#bridge_type').val()) {
                    alert('Please select a bridge type');
                    return false;
                }
                if (!$('#legacy_file')[0].files[0]) {
                    alert('Please upload a legacy file');
                    return false;
                }
                break;
                
            case 2:
                if (!this.importData.copybook) {
                    alert('Please define the copybook');
                    return false;
                }
                break;
                
            case 3:
                if (!$('#target_module').val()) {
                    alert('Please select a target module');
                    return false;
                }
                if (Object.keys(this.importData.fieldMappings).length === 0) {
                    alert('Please map at least one field');
                    return false;
                }
                break;
        }
        
        return true;
    },
    
    saveStepData: function(step) {
        switch(step) {
            case 1:
                this.importData.bridgeType = $('#bridge_type').val();
                this.importData.sourceSystem = $('#source_system').val();
                this.importData.sourceFormat = $('#source_format').val();
                this.importData.encoding = $('#encoding').val();
                break;
                
            case 2:
                this.importData.copybook = $('#copybook_content').val();
                break;
                
            case 3:
                this.importData.targetModule = $('#target_module').val();
                this.saveFieldMappings();
                break;
        }
    },
    
    handleFileUpload: function(e) {
        var file = e.target.files[0];
        if (file) {
            this.importData.fileName = file.name;
            
            var reader = new FileReader();
            reader.onload = function(e) {
                LegacyBridge.importData.fileContent = e.target.result;
            };
            reader.readAsArrayBuffer(file);
        }
    },
    
    parseCopybook: function() {
        var copybook = $('#copybook_content').val();
        if (!copybook) {
            alert('Please enter copybook content');
            return;
        }
        
        this.importData.copybook = copybook;
        
        // Simulate parsing - in real implementation, this would call COBOL service
        var fields = this.extractFieldsFromCopybook(copybook);
        this.displayCopybookPreview(fields);
    },
    
    extractFieldsFromCopybook: function(copybook) {
        var fields = [];
        var lines = copybook.split('\n');
        
        lines.forEach(function(line) {
            var match = line.match(/\s+05\s+(\S+)\s+PIC\s+(.+)\./);
            if (match) {
                fields.push({
                    name: match[1],
                    picture: match[2],
                    type: LegacyBridge.getFieldType(match[2]),
                    length: LegacyBridge.getFieldLength(match[2])
                });
            }
        });
        
        return fields;
    },
    
    getFieldType: function(picture) {
        if (picture.includes('X')) return 'String';
        if (picture.includes('9')) return 'Number';
        if (picture.includes('S')) return 'Signed Number';
        if (picture.includes('V')) return 'Decimal';
        return 'Unknown';
    },
    
    getFieldLength: function(picture) {
        var match = picture.match(/[X9]\((\d+)\)/);
        if (match) return parseInt(match[1]);
        
        var length = (picture.match(/[X9]/g) || []).length;
        return length;
    },
    
    displayCopybookPreview: function(fields) {
        var html = '<h4>Parsed Fields</h4>';
        html += '<table class="preview-table">';
        html += '<tr><th>Field Name</th><th>Type</th><th>Length</th><th>Picture</th></tr>';
        
        fields.forEach(function(field) {
            html += '<tr>';
            html += '<td>' + field.name + '</td>';
            html += '<td>' + field.type + '</td>';
            html += '<td>' + field.length + '</td>';
            html += '<td>' + field.picture + '</td>';
            html += '</tr>';
        });
        
        html += '</table>';
        $('#copybook_preview').html(html);
        
        // Store parsed fields for mapping
        this.importData.parsedFields = fields;
    },
    
    loadCopybookTemplate: function() {
        var template = '';
        
        switch(this.importData.bridgeType) {
            case 'CUSTOMER-MASTER':
                template = `       01  CUSTOMER-RECORD.
           05  CUST-ID         PIC X(10).
           05  CUST-NAME       PIC X(30).
           05  CUST-ADDRESS-1  PIC X(30).
           05  CUST-ADDRESS-2  PIC X(30).
           05  CUST-CITY       PIC X(20).
           05  CUST-STATE      PIC X(2).
           05  CUST-ZIP        PIC X(10).
           05  CUST-PHONE      PIC X(15).
           05  CUST-EMAIL      PIC X(50).
           05  CUST-BALANCE    PIC S9(9)V99 COMP-3.`;
                break;
                
            case 'ACCOUNT-LEDGER':
                template = `       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER     PIC X(15).
           05  ACCT-TYPE       PIC X(2).
           05  OPEN-DATE       PIC 9(8).
           05  CURR-BALANCE    PIC S9(11)V99 COMP-3.
           05  AVAIL-BALANCE   PIC S9(11)V99 COMP-3.`;
                break;
                
            default:
                template = '       01  RECORD.\n           05  FIELD-1     PIC X(10).';
        }
        
        $('#copybook_content').val(template);
    },
    
    initializeFieldMapping: function() {
        if (!this.importData.parsedFields) {
            alert('Please parse copybook first');
            return;
        }
        
        var html = '';
        this.importData.parsedFields.forEach(function(field) {
            html += '<tr>';
            html += '<td>' + field.name + '</td>';
            html += '<td><select class="field-map" data-legacy-field="' + field.name + '">';
            html += '<option value="">-- Skip --</option>';
            html += '</select></td>';
            html += '<td><select class="field-transform" data-legacy-field="' + field.name + '">';
            html += '<option value="">None</option>';
            html += '<option value="uppercase">Uppercase</option>';
            html += '<option value="lowercase">Lowercase</option>';
            html += '<option value="trim">Trim</option>';
            html += '<option value="date">Format Date</option>';
            html += '<option value="currency">Format Currency</option>';
            html += '</select></td>';
            html += '</tr>';
        });
        
        $('#field_mapping_tbody').html(html);
    },
    
    loadModuleFields: function() {
        var module = $('#target_module').val();
        if (!module) return;
        
        // Simulate loading fields - in real implementation, this would be AJAX
        var fields = this.getModuleFields(module);
        
        $('.field-map').each(function() {
            var options = '<option value="">-- Skip --</option>';
            fields.forEach(function(field) {
                options += '<option value="' + field.name + '">' + field.label + '</option>';
            });
            $(this).html(options);
        });
    },
    
    getModuleFields: function(module) {
        // Simulated field list
        var fieldMap = {
            'Accounts': [
                {name: 'name', label: 'Account Name'},
                {name: 'billing_address_street', label: 'Billing Street'},
                {name: 'billing_address_city', label: 'Billing City'},
                {name: 'billing_address_state', label: 'Billing State'},
                {name: 'billing_address_postalcode', label: 'Billing Postal Code'},
                {name: 'phone_office', label: 'Office Phone'},
                {name: 'email1', label: 'Email'},
                {name: 'annual_revenue', label: 'Annual Revenue'},
                {name: 'account_type', label: 'Type'}
            ],
            'Contacts': [
                {name: 'first_name', label: 'First Name'},
                {name: 'last_name', label: 'Last Name'},
                {name: 'email1', label: 'Email'},
                {name: 'phone_work', label: 'Work Phone'},
                {name: 'primary_address_street', label: 'Primary Street'},
                {name: 'primary_address_city', label: 'Primary City'},
                {name: 'primary_address_state', label: 'Primary State'},
                {name: 'primary_address_postalcode', label: 'Primary Postal Code'}
            ]
        };
        
        return fieldMap[module] || [];
    },
    
    saveFieldMappings: function() {
        this.importData.fieldMappings = {};
        
        $('.field-map').each(function() {
            var legacyField = $(this).data('legacy-field');
            var suiteField = $(this).val();
            var transform = $('.field-transform[data-legacy-field="' + legacyField + '"]').val();
            
            if (suiteField) {
                LegacyBridge.importData.fieldMappings[legacyField] = {
                    target: suiteField,
                    transform: transform
                };
            }
        });
    },
    
    addTransformationRule: function() {
        var ruleId = 'rule_' + Date.now();
        var html = `
            <div class="transformation-rule" id="${ruleId}">
                <select class="rule-type">
                    <option value="default">Set Default Value</option>
                    <option value="replace">Find & Replace</option>
                    <option value="concat">Concatenate Fields</option>
                    <option value="split">Split Field</option>
                </select>
                <div class="rule-config"></div>
                <button type="button" class="button small" onclick="LegacyBridge.removeRule('${ruleId}')">Remove</button>
            </div>
        `;
        
        $('#transformation_rules').append(html);
    },
    
    removeRule: function(ruleId) {
        $('#' + ruleId).remove();
    },
    
    prepareImportSummary: function() {
        $('#summary_bridge_type').text(this.importData.bridgeType);
        $('#summary_file_name').text(this.importData.fileName);
        $('#summary_target_module').text(this.importData.targetModule);
        $('#summary_field_count').text(Object.keys(this.importData.fieldMappings).length + ' fields mapped');
        
        // Preview data
        this.loadDataPreview();
    },
    
    loadDataPreview: function() {
        // Simulate preview - in real implementation, this would parse actual file
        var html = '<table class="preview-table">';
        html += '<thead><tr>';
        
        for (var field in this.importData.fieldMappings) {
            html += '<th>' + this.importData.fieldMappings[field].target + '</th>';
        }
        html += '</tr></thead><tbody>';
        
        // Sample data
        for (var i = 0; i < 5; i++) {
            html += '<tr>';
            for (var field in this.importData.fieldMappings) {
                html += '<td>Sample Data ' + (i + 1) + '</td>';
            }
            html += '</tr>';
        }
        
        html += '</tbody></table>';
        $('#preview-table-container').html(html);
    },
    
    startImport: function() {
        if (!confirm('Are you sure you want to start the import?')) {
            return;
        }
        
        $('#legacy-import-wizard').hide();
        $('#import-progress').show();
        
        // Prepare form data
        var formData = new FormData();
        formData.append('bridge_type', this.importData.bridgeType);
        formData.append('source_system', this.importData.sourceSystem);
        formData.append('source_format', this.importData.sourceFormat);
        formData.append('encoding', this.importData.encoding);
        formData.append('copybook', this.importData.copybook);
        formData.append('target_module', this.importData.targetModule);
        formData.append('field_mappings', JSON.stringify(this.importData.fieldMappings));
        formData.append('transformation_rules', JSON.stringify(this.importData.transformationRules));
        formData.append('legacy_file', $('#legacy_file')[0].files[0]);
        
        // Start import
        $.ajax({
            url: 'index.php?module=Legacy_Bridge&action=processImport',
            type: 'POST',
            data: formData,
            processData: false,
            contentType: false,
            xhr: function() {
                var xhr = new window.XMLHttpRequest();
                xhr.upload.addEventListener("progress", function(evt) {
                    if (evt.lengthComputable) {
                        var percentComplete = evt.loaded / evt.total * 100;
                        $('#progress-fill').css('width', percentComplete + '%');
                    }
                }, false);
                return xhr;
            },
            success: function(response) {
                LegacyBridge.handleImportResponse(response);
            },
            error: function(xhr, status, error) {
                LegacyBridge.handleImportError(error);
            }
        });
        
        // Start progress monitoring
        this.monitorProgress();
    },
    
    monitorProgress: function() {
        var checkProgress = setInterval(function() {
            $.ajax({
                url: 'index.php?module=Legacy_Bridge&action=getImportProgress',
                type: 'GET',
                success: function(response) {
                    var data = typeof response === 'string' ? JSON.parse(response) : response;
                    
                    $('#records-processed').text(data.processed);
                    $('#records-errors').text(data.errors);
                    
                    if (data.status === 'completed' || data.status === 'error') {
                        clearInterval(checkProgress);
                    }
                    
                    if (data.log) {
                        $('#import-log').append('<div>' + data.log + '</div>');
                    }
                }
            });
        }, 2000);
    },
    
    handleImportResponse: function(response) {
        var data = typeof response === 'string' ? JSON.parse(response) : response;
        
        if (data.success) {
            alert('Import completed successfully!\n' +
                  'Records processed: ' + data.records_processed + '\n' +
                  'Records failed: ' + data.records_failed);
            
            window.location.href = 'index.php?module=Legacy_Bridge&action=DetailView&record=' + data.bridge_id;
        } else {
            this.handleImportError(data.error);
        }
    },
    
    handleImportError: function(error) {
        alert('Import failed: ' + error);
        $('#import-log').append('<div class="error">ERROR: ' + error + '</div>');
    }
};

// Initialize on document ready
$(document).ready(function() {
    LegacyBridge.init();
});