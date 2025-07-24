<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/Controller/SugarController.php');

class COBOL_CalculatorController extends SugarController
{
    public function action_calculate()
    {
        $this->view = 'json';
        
        $type = $_POST['type'] ?? '';
        $parameters = $_POST['parameters'] ?? '';
        
        if (empty($type) || empty($parameters)) {
            echo json_encode(array(
                'success' => false,
                'error' => 'Missing required parameters'
            ));
            return;
        }
        
        $params = json_decode($parameters, true);
        if (!$params) {
            echo json_encode(array(
                'success' => false,
                'error' => 'Invalid parameters format'
            ));
            return;
        }
        
        // Create calculator bean and execute
        require_once('custom/modules/COBOL_Calculator/COBOL_Calculator.php');
        $calculator = new COBOL_Calculator();
        $result = $calculator->executeCalculation($type, $params);
        
        echo json_encode($result);
    }
    
    public function action_history()
    {
        $this->view = 'json';
        
        require_once('custom/modules/COBOL_Calculator/COBOL_Calculator.php');
        $calculator = new COBOL_Calculator();
        $history = $calculator->getCalculationHistory(null, 20);
        
        echo json_encode(array(
            'success' => true,
            'history' => $history
        ));
    }
    
    public function action_save()
    {
        $this->view = 'json';
        
        $name = $_POST['name'] ?? '';
        $type = $_POST['type'] ?? '';
        $result = $_POST['result'] ?? '';
        
        if (empty($name)) {
            echo json_encode(array(
                'success' => false,
                'error' => 'Name is required'
            ));
            return;
        }
        
        require_once('custom/modules/COBOL_Calculator/COBOL_Calculator.php');
        $calculator = new COBOL_Calculator();
        $calculator->name = $name;
        $calculator->calculation_type = $type;
        $calculator->calculation_result = $result;
        $calculator->status = 'completed';
        $calculator->save();
        
        echo json_encode(array(
            'success' => true,
            'id' => $calculator->id
        ));
    }
    
    public function action_stats()
    {
        $this->view = 'json';
        
        require_once('custom/modules/COBOL_Calculator/COBOL_Calculator.php');
        $calculator = new COBOL_Calculator();
        $stats = $calculator->getCalculationStats();
        
        echo json_encode(array(
            'success' => true,
            'stats' => $stats
        ));
    }
    
    public function action_calculator()
    {
        $this->view = 'calculator';
    }
}