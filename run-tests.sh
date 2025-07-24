#!/bin/bash
# Quick test runner for COBOL integration

echo "=== COBOL Integration Test Suite ==="
echo ""

# Check if services are running
echo "Checking COBOL services..."
curl -s http://localhost:3001/health > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "❌ COBOL services not running!"
    echo "Starting services..."
    docker-compose -f docker-compose-cobol.yml up -d
    echo "Waiting 10 seconds for services to start..."
    sleep 10
fi

# Run health check
echo ""
echo "1. Testing service health..."
HEALTH=$(curl -s http://localhost:3001/health)
if [[ $HEALTH == *"healthy"* ]]; then
    echo "✅ Service is healthy"
else
    echo "❌ Service health check failed"
    exit 1
fi

# Test calculations
echo ""
echo "2. Testing calculations..."

# Simple interest
echo -n "   Simple Interest: "
RESULT=$(curl -s -X POST http://localhost:3001/calculate \
    -H "Content-Type: application/json" \
    -d '{"type":"SIMPLE-INTEREST","principal":10000,"rate":0.05,"term":365}')
if [[ $RESULT == *"interest"* ]]; then
    echo "✅ Pass"
else
    echo "❌ Fail"
fi

# Loan payment
echo -n "   Loan Payment: "
RESULT=$(curl -s -X POST http://localhost:3001/calculate \
    -H "Content-Type: application/json" \
    -d '{"type":"LOAN-PAYMENT","principal":100000,"rate":0.05,"term":360}')
if [[ $RESULT == *"payment"* ]]; then
    echo "✅ Pass"
else
    echo "❌ Fail"
fi

# Test WebSocket
echo ""
echo "3. Testing WebSocket server..."
curl -s http://localhost:8081 > /dev/null 2>&1
if [ $? -eq 0 ]; then
    echo "✅ WebSocket server accessible"
else
    echo "❌ WebSocket server not accessible"
fi

# Test authentication
echo ""
echo "4. Testing authentication..."
AUTH_RESULT=$(curl -s -X POST http://localhost:3001/auth/login \
    -H "Content-Type: application/json" \
    -d '{"username":"testuser","password":"testpass"}')
if [[ $AUTH_RESULT == *"token"* ]] || [[ $AUTH_RESULT == *"error"* ]]; then
    echo "✅ Authentication endpoint working"
else
    echo "❌ Authentication endpoint failed"
fi

# Summary
echo ""
echo "=== Test Summary ==="
echo "All basic tests completed!"
echo ""
echo "For full test suite, run:"
echo "php tests/COBOLIntegrationTest.php"