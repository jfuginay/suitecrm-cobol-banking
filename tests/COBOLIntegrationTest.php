<?php
/**
 * Automated tests for COBOL Banking Integration
 * Run with: phpunit tests/COBOLIntegrationTest.php
 */

use PHPUnit\Framework\TestCase;

class COBOLIntegrationTest extends TestCase {
    
    private $api_url = 'http://localhost:3001';
    private $cobol_bridge;
    
    public function setUp(): void {
        // Include SuiteCRM bootstrap if available
        if (file_exists('include/entryPoint.php')) {
            require_once('include/entryPoint.php');
        }
        
        // Include COBOL Bridge class
        require_once('custom/modules/COBOL_Bridge/COBOL_Bridge.php');
        $this->cobol_bridge = new COBOL_Bridge();
    }
    
    /**
     * Test COBOL service health check
     */
    public function testServiceHealth() {
        $ch = curl_init($this->api_url . '/health');
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_TIMEOUT, 5);
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        $this->assertEquals(200, $http_code, 'COBOL service should return 200 OK');
        
        $health = json_decode($response, true);
        $this->assertArrayHasKey('status', $health);
        $this->assertEquals('healthy', $health['status']);
    }
    
    /**
     * Test simple interest calculation
     */
    public function testSimpleInterestCalculation() {
        $result = $this->cobol_bridge->calculate([
            'type' => 'SIMPLE-INTEREST',
            'principal' => 10000,
            'rate' => 0.05,
            'term' => 365
        ]);
        
        $this->assertNotEmpty($result);
        $this->assertArrayHasKey('interest', $result);
        $this->assertEquals(500.00, $result['interest'], 'Simple interest should be 500.00');
    }
    
    /**
     * Test loan payment calculation
     */
    public function testLoanPaymentCalculation() {
        $result = $this->cobol_bridge->calculate([
            'type' => 'LOAN-PAYMENT',
            'principal' => 100000,
            'rate' => 0.05,
            'term' => 360  // 30 years
        ]);
        
        $this->assertNotEmpty($result);
        $this->assertArrayHasKey('payment', $result);
        
        // Monthly payment should be around $536.82
        $this->assertGreaterThan(530, $result['payment']);
        $this->assertLessThan(540, $result['payment']);
    }
    
    /**
     * Test compound interest calculation
     */
    public function testCompoundInterestCalculation() {
        $result = $this->cobol_bridge->calculate([
            'type' => 'COMPOUND-INTEREST',
            'principal' => 10000,
            'rate' => 0.05,
            'term' => 5,
            'frequency' => 12  // Monthly compounding
        ]);
        
        $this->assertNotEmpty($result);
        $this->assertArrayHasKey('final_amount', $result);
        
        // Final amount should be around $12,833.59
        $this->assertGreaterThan(12800, $result['final_amount']);
        $this->assertLessThan(12900, $result['final_amount']);
    }
    
    /**
     * Test error handling for invalid calculation type
     */
    public function testInvalidCalculationType() {
        $result = $this->cobol_bridge->calculate([
            'type' => 'INVALID-TYPE',
            'principal' => 10000
        ]);
        
        $this->assertArrayHasKey('error', $result);
        $this->assertStringContainsString('Invalid calculation type', $result['error']);
    }
    
    /**
     * Test caching functionality
     */
    public function testCalculationCaching() {
        // First calculation
        $start_time = microtime(true);
        $result1 = $this->cobol_bridge->calculate([
            'type' => 'SIMPLE-INTEREST',
            'principal' => 50000,
            'rate' => 0.04,
            'term' => 180
        ]);
        $first_time = microtime(true) - $start_time;
        
        // Second calculation (should be cached)
        $start_time = microtime(true);
        $result2 = $this->cobol_bridge->calculate([
            'type' => 'SIMPLE-INTEREST',
            'principal' => 50000,
            'rate' => 0.04,
            'term' => 180
        ]);
        $second_time = microtime(true) - $start_time;
        
        // Results should be identical
        $this->assertEquals($result1, $result2);
        
        // Cached result should be significantly faster
        $this->assertLessThan($first_time * 0.5, $second_time, 'Cached result should be at least 50% faster');
    }
    
    /**
     * Test batch calculation
     */
    public function testBatchCalculation() {
        $calculations = [
            [
                'type' => 'SIMPLE-INTEREST',
                'principal' => 10000,
                'rate' => 0.05,
                'term' => 365
            ],
            [
                'type' => 'LOAN-PAYMENT',
                'principal' => 50000,
                'rate' => 0.04,
                'term' => 180
            ]
        ];
        
        $ch = curl_init($this->api_url . '/batch');
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, ['Content-Type: application/json']);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode(['calculations' => $calculations]));
        
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        $this->assertEquals(200, $http_code);
        
        $results = json_decode($response, true);
        $this->assertArrayHasKey('results', $results);
        $this->assertCount(2, $results['results']);
    }
    
    /**
     * Test decimal precision
     */
    public function testDecimalPrecision() {
        // Test that COBOL maintains exact decimal precision
        $result = $this->cobol_bridge->calculate([
            'type' => 'SIMPLE-INTEREST',
            'principal' => 123.45,
            'rate' => 0.0789,
            'term' => 33
        ]);
        
        $this->assertNotEmpty($result);
        
        // Verify no floating point errors
        $expected = 123.45 * 0.0789 * (33/365);
        $cobol_result = $result['interest'];
        
        // COBOL should give exact result
        $this->assertEquals(round($expected, 2), $cobol_result);
    }
    
    /**
     * Test WebSocket connection
     */
    public function testWebSocketConnection() {
        // Simple test to check if WebSocket server is responding
        $headers = get_headers('http://localhost:8081', 1);
        
        $this->assertNotFalse($headers, 'WebSocket server should be accessible');
        
        // WebSocket upgrade should return 426 Upgrade Required for HTTP request
        $status = substr($headers[0], 9, 3);
        $this->assertContains($status, ['426', '101'], 'WebSocket server should return upgrade status');
    }
    
    /**
     * Test authentication module
     */
    public function testAuthentication() {
        $ch = curl_init($this->api_url . '/auth/login');
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, ['Content-Type: application/json']);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode([
            'username' => 'testuser',
            'password' => 'testpass'
        ]));
        
        $response = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        // Should return 200 or 401 (not 500)
        $this->assertContains($http_code, [200, 401], 'Auth endpoint should work without errors');
    }
}

// Run tests if called directly
if (php_sapi_name() === 'cli' && basename($argv[0]) === basename(__FILE__)) {
    echo "Running COBOL Integration Tests...\n\n";
    
    // Check if services are running
    $ch = curl_init('http://localhost:3001/health');
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_TIMEOUT, 2);
    $response = curl_exec($ch);
    $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);
    
    if ($http_code !== 200) {
        echo "ERROR: COBOL services are not running!\n";
        echo "Please start services with: docker-compose -f docker-compose-cobol.yml up -d\n";
        exit(1);
    }
    
    // Run PHPUnit if available
    if (class_exists('PHPUnit\TextUI\Command')) {
        PHPUnit\TextUI\Command::main();
    } else {
        echo "PHPUnit not found. Install with: composer require --dev phpunit/phpunit\n";
        
        // Run basic tests manually
        $test = new COBOLIntegrationTest();
        $test->setUp();
        
        try {
            $test->testServiceHealth();
            echo "✓ Service health check passed\n";
            
            $test->testSimpleInterestCalculation();
            echo "✓ Simple interest calculation passed\n";
            
            $test->testLoanPaymentCalculation();
            echo "✓ Loan payment calculation passed\n";
            
            echo "\nBasic tests completed successfully!\n";
        } catch (Exception $e) {
            echo "✗ Test failed: " . $e->getMessage() . "\n";
        }
    }
}