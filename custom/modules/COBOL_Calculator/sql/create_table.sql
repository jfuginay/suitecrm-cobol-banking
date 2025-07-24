-- COBOL Calculator Module Table
CREATE TABLE IF NOT EXISTS cobol_calculator (
    id char(36) NOT NULL,
    name varchar(255) DEFAULT NULL,
    date_entered datetime DEFAULT NULL,
    date_modified datetime DEFAULT NULL,
    modified_user_id char(36) DEFAULT NULL,
    created_by char(36) DEFAULT NULL,
    description text,
    deleted tinyint(1) DEFAULT '0',
    assigned_user_id char(36) DEFAULT NULL,
    calculation_type varchar(50) DEFAULT NULL,
    input_parameters text,
    calculation_result text,
    cobol_response text,
    execution_time decimal(10,6) DEFAULT NULL,
    status varchar(25) DEFAULT 'pending',
    account_id char(36) DEFAULT NULL,
    contact_id char(36) DEFAULT NULL,
    PRIMARY KEY (id),
    KEY idx_calc_type (calculation_type),
    KEY idx_calc_status (status),
    KEY idx_calc_account (account_id),
    KEY idx_calc_contact (contact_id),
    KEY idx_date_entered (date_entered),
    KEY idx_assigned_user (assigned_user_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- Add module to modules table if not exists
INSERT IGNORE INTO modules (id, name, date_entered, date_modified, modified_user_id, created_by, deleted, assigned_user_id, module_name, description)
VALUES (UUID(), 'COBOL Calculator', NOW(), NOW(), '1', '1', 0, '1', 'COBOL_Calculator', 'COBOL-powered financial calculations module');

-- Grant permissions
INSERT IGNORE INTO acl_actions (id, date_entered, date_modified, modified_user_id, created_by, name, category, acltype, aclaccess, deleted)
VALUES 
(UUID(), NOW(), NOW(), '1', '1', 'list', 'COBOL_Calculator', 'module', 89, 0),
(UUID(), NOW(), NOW(), '1', '1', 'edit', 'COBOL_Calculator', 'module', 89, 0),
(UUID(), NOW(), NOW(), '1', '1', 'delete', 'COBOL_Calculator', 'module', 89, 0),
(UUID(), NOW(), NOW(), '1', '1', 'view', 'COBOL_Calculator', 'module', 89, 0),
(UUID(), NOW(), NOW(), '1', '1', 'import', 'COBOL_Calculator', 'module', 89, 0),
(UUID(), NOW(), NOW(), '1', '1', 'export', 'COBOL_Calculator', 'module', 89, 0);