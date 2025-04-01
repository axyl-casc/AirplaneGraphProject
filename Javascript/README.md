# TEMOO Cargo – JavaScript Version

Welcome to the TEMOO Cargo expedited shipping service simulator, implemented in JavaScript (Node.js). This tool computes efficient routing and scheduling for cargo deliveries from a central hub to various Canadian airports, constrained by aircraft capacity, deadlines, and travel times.


## How to Run

1. Install Node.js  
   Download and install Node.js from nodejs.org if it is not already installed.

2. Navigate to the JavaScript directory:

   ```bash
   cd Javascript
   ```

3. Run the simulator with a set of input files:

   ```bash
   node main.js testcases/distance_matrix_01.json testcases/package_data_01.json testcases/constraints_01.json
   ```

4. Run all test cases:

   ```bash
   sh runTest.sh
   ```

   Note: Some test cases, such as test case 16, may take longer to complete depending on hardware performance.

## Input Format

- **Distance Matrix (distance_matrix.json)**: 2D array representing distances between airports. A value of -1 indicates no direct route.
- **Package Data (package_data.json)**: Array of packages, each with id, weight, arrival time, deadline time, and destination.
- **Constraints (constraints.json)**: Includes number of planes, their weight capacity, and speed.

## Output

- Displays the optimal delivery schedule in the console.
- If no feasible solution exists, the program clearly states this.

## Notes

- The scheduling algorithm uses a backtracking or branch-and-cut strategy.
- Graph traversal relies on algorithms such as Floyd–Warshall or Dijkstra's.
- Scheduling ensures packages meet deadlines and planes do not exceed capacity.

Refer to the general project README for a broader overview and the Haskell implementation.
