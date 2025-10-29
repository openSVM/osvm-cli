#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}OVSM Iterative Research Strategy Test${NC}"
echo -e "${BLUE}========================================${NC}\n"

# Test 1: Simple query (should work without iteration)
echo -e "${YELLOW}Test 1: Simple Query${NC}"
echo "Query: 'Get current block statistics'"
echo -e "\nExpected behavior:"
echo "  - Should complete in first attempt"
echo "  - No strategy iteration needed"
echo -e "\nRunning test..."
echo "----------------------------------------"

cargo run --bin osvm -- -v "Get current block statistics"

echo -e "\n${GREEN}Test 1 Complete${NC}"
echo "----------------------------------------"

# Test 2: Query requiring data exploration
echo -e "\n${YELLOW}Test 2: Data Exploration Query${NC}"
echo "Query: 'Analyze recent DeFi activity and identify top protocols'"
echo -e "\nExpected behavior:"
echo "  - Iteration 1: Get DeFi overview"
echo "  - Iteration 2: Deep dive into specific protocols"
echo "  - Should refine strategy based on findings"
echo -e "\nRunning test..."
echo "----------------------------------------"

cargo run --bin osvm -- -v "Analyze recent DeFi activity and identify top protocols"

echo -e "\n${GREEN}Test 2 Complete${NC}"
echo "----------------------------------------"

# Test 3: Complex investigation with multiple tools
echo -e "\n${YELLOW}Test 3: Complex Investigation${NC}"
echo "Query: 'Find the most active NFT collection today and analyze its trading patterns'"
echo -e "\nExpected behavior:"
echo "  - Iteration 1: Get trending NFT collections"
echo "  - Iteration 2: Analyze specific collection details"
echo "  - Iteration 3: Examine trading patterns"
echo -e "\nRunning test..."
echo "----------------------------------------"

cargo run --bin osvm -- -v "Find the most active NFT collection today and analyze its trading patterns"

echo -e "\n${GREEN}Test 3 Complete${NC}"
echo "----------------------------------------"

echo -e "\n${BLUE}========================================${NC}"
echo -e "${BLUE}All Tests Complete!${NC}"
echo -e "${BLUE}========================================${NC}"

echo -e "\n${YELLOW}Summary:${NC}"
echo "✓ Test 1: Simple queries should complete quickly"
echo "✓ Test 2: Exploration queries should show strategy refinement"
echo "✓ Test 3: Complex queries should demonstrate multi-iteration research"
