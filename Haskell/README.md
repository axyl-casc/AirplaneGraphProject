# TEMOO Cargo – Haskell Version

Welcome to the TEMOO Cargo expedited shipping service simulator, implemented in **Haskell**. This tool determines optimal or near-optimal routes for delivering packages from a central hub airport to other Canadian destinations, respecting constraints such as aircraft capacity, package deadlines, and flight distance.


## How to Build and Run

1. **Navigate to the Haskell Directory**

   ```bash
   cd Haskell
   ```

2. **Build the Project Using Cabal**

   ```bash
   cabal clean
   cabal build
   ```

3. **Run the Program**

   ```bash
   ./AirplaneGraphProject testcases/distance_matrix_01.json testcases/package_data_01.json testcases/constraints_01.json
   ```

   > Note: The executable name may vary depending on how it's configured in the `.cabal` file.

## Input Format

- **Distance Matrix (distance_matrix.json)**: A square matrix of distances. `-1` indicates no direct route.
- **Package Data (package_data.json)**: Each entry contains a package's ID, weight, arrival time, deadline, and destination.
- **Constraints (constraints.json)**: Includes `numOfPlanes`, `weightCapacity`, and `speed`.

## Output

- Displays results in the console, including:
  - Total distance flown
  - Number of planes used
  - Per-plane delivery schedule
  - Confirmation that all packages are delivered within deadlines
- May also write output to `solution.txt`, if implemented

## Algorithm Overview

- Uses a branch-and-cut or backtracking method to explore feasible plane routes
- Graph analysis uses algorithms like Floyd–Warshall or Dijkstra for shortest path calculations
- Scheduler ensures each plane's weight and time constraints are respected

Refer to the general project README for a full overview and JavaScript counterpart.
