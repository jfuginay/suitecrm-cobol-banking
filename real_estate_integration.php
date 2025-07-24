<?php
/**
 * Real Estate Integration with Credit Card API
 * This module provides property listing and customer payment management
 */

class RealEstateIntegration 
{
    private $credit_api_base = 'http://host.docker.internal:3000';
    
    /**
     * Add a new customer with card on file for real estate services
     */
    public function addCustomerWithCard($customerData) 
    {
        // Prepare data for credit card API
        $apiData = [
            'fullName' => $customerData['full_name'],
            'email' => $customerData['email'],
            'phone' => $customerData['phone'] ?? '',
            'address' => [
                'street' => $customerData['street'] ?? '',
                'city' => $customerData['city'] ?? '',
                'state' => $customerData['state'] ?? '',
                'zip' => $customerData['zip'] ?? ''
            ],
            'cardNumber' => $customerData['card_number'],
            'expiryDate' => $customerData['expiry_date'],
            'cvv' => $customerData['cvv'],
            'propertyDetails' => [
                'propertyType' => $customerData['property_type'] ?? 'residential',
                'estimatedValue' => floatval($customerData['estimated_value'] ?? 0),
                'listingIntent' => $customerData['listing_intent'] ?? 'sell'
            ]
        ];
        
        // Call credit card API
        $response = $this->makeApiCall('/api/signup', 'POST', $apiData);
        
        if ($response && $response['success']) {
            return [
                'success' => true,
                'customer_id' => $response['customerId'],
                'card_info' => $response['cardOnFile'],
                'message' => 'Customer successfully added with card on file'
            ];
        }
        
        return [
            'success' => false,
            'message' => $response['error'] ?? 'Failed to add customer'
        ];
    }
    
    /**
     * Validate a credit card for a customer
     */
    public function validateCard($cardNumber) 
    {
        $response = $this->makeApiCall('/api/validate', 'POST', [
            'cardNumber' => $cardNumber
        ]);
        
        return $response;
    }
    
    /**
     * Process a real estate transaction charge
     */
    public function processRealEstateCharge($customerId, $amount, $description) 
    {
        // In production, this would charge the card on file
        return [
            'success' => true,
            'transaction_id' => 'TXN_' . time(),
            'amount' => $amount,
            'description' => $description,
            'status' => 'completed'
        ];
    }
    
    /**
     * Get customer cards on file
     */
    public function getCustomerCards() 
    {
        $response = $this->makeApiCall('/api/cards', 'GET');
        return $response;
    }
    
    /**
     * Make API call to credit card service
     */
    private function makeApiCall($endpoint, $method = 'GET', $data = null) 
    {
        $url = $this->credit_api_base . $endpoint;
        
        $curl = curl_init();
        curl_setopt_array($curl, [
            CURLOPT_URL => $url,
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_TIMEOUT => 30,
            CURLOPT_CUSTOMREQUEST => $method,
            CURLOPT_HTTPHEADER => [
                'Content-Type: application/json',
                'Accept: application/json'
            ]
        ]);
        
        if ($data && in_array($method, ['POST', 'PUT', 'PATCH'])) {
            curl_setopt($curl, CURLOPT_POSTFIELDS, json_encode($data));
        }
        
        $response = curl_exec($curl);
        $httpCode = curl_getinfo($curl, CURLINFO_HTTP_CODE);
        curl_close($curl);
        
        if ($httpCode >= 200 && $httpCode < 300) {
            return json_decode($response, true);
        }
        
        return ['error' => 'API call failed', 'http_code' => $httpCode];
    }
}

/**
 * Demo function to add John Fuginay as a real estate customer
 */
function addJohnAsCustomer() 
{
    $realEstate = new RealEstateIntegration();
    
    $customerData = [
        'full_name' => 'John Fuginay',
        'email' => 'john.fuginay@gmail.com',
        'phone' => '+1-555-987-6543',
        'street' => '456 Oak Avenue',
        'city' => 'San Francisco',
        'state' => 'CA',
        'zip' => '94103',
        'card_number' => '4539578763621486', // This card is already validated
        'expiry_date' => '12/27',
        'cvv' => '123',
        'property_type' => 'residential',
        'estimated_value' => 850000,
        'listing_intent' => 'sell'
    ];
    
    $result = $realEstate->addCustomerWithCard($customerData);
    return $result;
}

// If called directly, run the demo
if (php_sapi_name() === 'cli' && basename(__FILE__) == basename($argv[0])) {
    echo "Adding John Fuginay as Real Estate Customer...\n";
    $result = addJohnAsCustomer();
    echo json_encode($result, JSON_PRETTY_PRINT) . "\n";
}
?>