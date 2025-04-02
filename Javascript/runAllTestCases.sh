#!/bin/bash

echo ""
echo "Running test case 1..."
node main.js ../testFiles/tests/1/1_dist.json ../testFiles/tests/1/1_pkg.json ../testFiles/tests/1/1_constraints.json
echo ""
echo "Running test case 2..."
node main.js ../testFiles/2/2_dist.json ../testFiles/2/2_pkg.json ../testFiles/2/2_constraints.json
echo ""
echo "Running test case 3..."
node main.js ../testFiles/3/3_dist.json ../testFiles/3/3_pkg.json ../testFiles/3/3_constraints.json
echo ""
echo "Running test case 4..."
node main.js ../testFiles/4/4_dist.json ../testFiles/4/4_pkg.json ../testFiles/4/4_constraints.json
echo ""
echo "Running test case 5..."
node main.js ../testFiles/5/5_dist.json ../testFiles/5/5_pkg.json ../testFiles/5/5_constraints.json
echo ""
echo "Running test case 6..."
node main.js ../testFiles/6/6_dist.json ../testFiles/6/6_pkg.json ../testFiles/6/6_constraints.json
echo ""
echo "Running test case 7..."
node main.js ../testFiles/7/7_dist.json ../testFiles/7/7_pkg.json ../testFiles/7/7_constraints.json
echo ""
echo "Running test case 8..."
node main.js ../testFiles/8/8_dist.json ../testFiles/8/8_pkg.json ../testFiles/8/8_constraints.json
echo ""
echo "Running test case 9..."
node main.js ../testFiles/9/9_dist.json ../testFiles/9/9_pkg.json ../testFiles/9/9_constraints.json
echo ""
echo "Running test case 10..."
node main.js ../testFiles/10/10_dist.json ../testFiles/10/10_pkg.json ../testFiles/10/10_constraints.json
echo ""
echo "Running test case 11..."
node main.js ../testFiles/11/11_dist.json ../testFiles/11/11_pkg.json ../testFiles/11/11_constraints.json
echo ""
echo "Running test case 12..."
node main.js ../testFiles/12/12_dist.json ../testFiles/12/12_pkg.json ../testFiles/12/12_constraints.json
echo ""
echo "Running test case 13..."
node main.js ../testFiles/13/13_dist.json ../testFiles/13/13_pkg.json ../testFiles/13/13_constraints.json
echo ""
echo "Running test case 14..."
node main.js ../testFiles/14/14_dist.json ../testFiles/14/14_pkg.json ../testFiles/14/14_constraints.json
echo ""
echo "Running test case 15..."
node main.js ../testFiles/15/15_dist.json ../testFiles/15/15_pkg.json ../testFiles/15/15_constraints.json
echo ""
echo "Running test case 16..."
echo "This one might take a while..."
node main.js ../testFiles/16/16_dist.json ../testFiles/16/16_pkg.json ../testFiles/16/16_constraints.json
echo ""
echo "All tests completed!"
$SHELL