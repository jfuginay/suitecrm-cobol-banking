<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$viewdefs['COBOL_Calculator']['DetailView'] = array(
    'templateMeta' => array(
        'form' => array(
            'buttons' => array(
                'EDIT',
                'DELETE',
                'DUPLICATE',
                array(
                    'customCode' => '<input type="button" class="button" onClick="window.location=\'index.php?module=COBOL_Calculator&action=calculator\';" value="{$MOD.LBL_RUN_NEW_CALCULATION}">',
                ),
            ),
        ),
        'maxColumns' => '2',
        'widths' => array(
            array('label' => '10', 'field' => '30'),
            array('label' => '10', 'field' => '30'),
        ),
    ),
    'panels' => array(
        'default' => array(
            array(
                'name',
                'status',
            ),
            array(
                'calculation_type',
                'execution_time',
            ),
            array(
                array(
                    'name' => 'input_parameters',
                    'label' => 'LBL_INPUT_PARAMETERS',
                    'customCode' => '<div id="input_params_display"></div>
                        <script>
                        var params = {$fields.input_parameters.value|@json_encode};
                        if(params) {
                            var parsed = JSON.parse(params);
                            var html = "<table class=\"detail-table\">";
                            for(var key in parsed) {
                                html += "<tr><td><b>" + key + ":</b></td><td>" + parsed[key] + "</td></tr>";
                            }
                            html += "</table>";
                            document.getElementById("input_params_display").innerHTML = html;
                        }
                        </script>',
                ),
                array(
                    'name' => 'calculation_result',
                    'label' => 'LBL_CALCULATION_RESULT',
                    'customCode' => '<div id="calc_result_display"></div>
                        <script>
                        var result = {$fields.calculation_result.value|@json_encode};
                        if(result) {
                            var parsed = JSON.parse(result);
                            var html = "<table class=\"detail-table\">";
                            for(var key in parsed) {
                                html += "<tr><td><b>" + key + ":</b></td><td>" + parsed[key] + "</td></tr>";
                            }
                            html += "</table>";
                            document.getElementById("calc_result_display").innerHTML = html;
                        }
                        </script>',
                ),
            ),
            array(
                'account_name',
                'contact_name',
            ),
            array(
                'assigned_user_name',
                array(
                    'name' => 'date_entered',
                    'customCode' => '{$fields.date_entered.value} {$APP.LBL_BY} {$fields.created_by_name.value}',
                    'label' => 'LBL_DATE_ENTERED',
                ),
            ),
            array(
                'description',
            ),
        ),
    ),
);