#!/bin/bash
echo ""
echo "Running test case 1..."
node main.js tests/integration_tests/1/1_dist.json tests/integration_tests/1/1_pkg.json tests/integration_tests/1/1_constraints.json
echo ""
echo "Running test case 2..."
node main.js ./tests/integration_tests/2/2_dist.json ./tests/integration_tests/2/2_pkg.json ./tests/integration_tests/2/2_constraints.json
echo ""
echo "Running test case 3..."
node main.js ./tests/integration_tests/3/3_dist.json ./tests/integration_tests/3/3_pkg.json ./tests/integration_tests/3/3_constraints.json
echo ""
echo "Running test case 4..."
node main.js ./tests/integration_tests/4/4_dist.json ./tests/integration_tests/4/4_pkg.json ./tests/integration_tests/4/4_constraints.json
echo ""
echo "Running test case 5..."
node main.js ./tests/integration_tests/5/5_dist.json ./tests/integration_tests/5/5_pkg.json ./tests/integration_tests/5/5_constraints.json
echo ""
echo "Running test case 6..."
node main.js ./tests/integration_tests/6/6_dist.json ./tests/integration_tests/6/6_pkg.json ./tests/integration_tests/6/6_constraints.json
echo ""
echo "Running test case 7..."
node main.js ./tests/integration_tests/7/7_dist.json ./tests/integration_tests/7/7_pkg.json ./tests/integration_tests/7/7_constraints.json
echo ""
echo "Running test case 8..."
node main.js ./tests/integration_tests/8/8_dist.json ./tests/integration_tests/8/8_pkg.json ./tests/integration_tests/8/8_constraints.json
echo ""
echo "Running test case 9..."
node main.js ./tests/integration_tests/9/9_dist.json ./tests/integration_tests/9/9_pkg.json ./tests/integration_tests/9/9_constraints.json
echo ""
echo "Running test case 10..."
node main.js ./tests/integration_tests/10/10_dist.json ./tests/integration_tests/10/10_pkg.json ./tests/integration_tests/10/10_constraints.json
echo ""
echo "Running test case 11..."
node main.js ./tests/integration_tests/11/11_dist.json ./tests/integration_tests/11/11_pkg.json ./tests/integration_tests/11/11_constraints.json
echo ""
echo "Running test case 12..."
node main.js ./tests/integration_tests/12/12_dist.json ./tests/integration_tests/12/12_pkg.json ./tests/integration_tests/12/12_constraints.json
echo ""
echo "Running test case 13..."
node main.js ./tests/integration_tests/13/13_dist.json ./tests/integration_tests/13/13_pkg.json ./tests/integration_tests/13/13_constraints.json
echo ""
echo "Running test case 14..."
node main.js ./tests/integration_tests/14/14_dist.json ./tests/integration_tests/14/14_pkg.json ./tests/integration_tests/14/14_constraints.json
echo ""
echo "Running test case 15..."
node main.js ./tests/integration_tests/15/15_dist.json ./tests/integration_tests/15/15_pkg.json ./tests/integration_tests/15/15_constraints.json
echo ""
echo "Running test case 16..."
node main.js ./tests/integration_tests/16/16_dist.json ./tests/integration_tests/16/16_pkg.json ./tests/integration_tests/16/16_constraints.json
echo ""
echo "All tests completed!"
$SHELL