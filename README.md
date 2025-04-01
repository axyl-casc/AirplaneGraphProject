# Programming Paradigms Project (Haskell Version)

Welcome to the **TEMOO Cargo** expedited shipping service simulator! This project demonstrates how to schedule and route cargo plane deliveries to various airports across Canada, subject to constraints such as weight capacity, flight distances, and package deadlines. Although this README references a Haskell implementation, the core concepts and input/output formats described here align closely with the approach used in the reference JavaScript/Node.js version.

---

## Table of Contents
1. [Project Overview](#project-overview)  
2. [Core Features](#core-features)  
3. [Input Files & Data Format](#input-files--data-format)  
4. [How to Compile and Run](#how-to-compile-and-run)  
5. [Algorithm & Approach](#algorithm--approach)  
6. [Output & Results](#output--results)  
7. [Project Structure](#project-structure)  
8. [Contributing](#contributing)  
9. [License](#license)

---

## Project Overview
TEMOO Cargo is an expedited cargo delivery system that schedules cargo planes from a central hub airport to various destinations. Packages have arrival times, deadlines (must be delivered within 24 hours), and weights. A limited number of planes operate round-trip flights from the hub to distribute packages while complying with constraints on:

- Plane weight capacity  
- Fuel/speed limitations  
- Delivery deadlines  

**Key Goal**: Determine an optimal or near-optimal routing strategy to deliver all packages on time, minimizing total distance flown.

---

## Core Features
1. **Graph-Based Representation**  
   - Airports represented as **nodes** (vertices).  
   - Distances between airports represented as **edges** (flight paths). A distance of `-1` indicates no direct route.

2. **Constraint-Based Scheduling**  
   - Limited number of planes.  
   - Weight capacity constraints per plane.  
   - 24-hour delivery deadlines for all packages.  

3. **Scheduling Algorithm**  
   - Branch-and-cut or backtracking approach to explore different plane assignments and route permutations.  
   - Shortest path calculations (e.g., Floyd–Warshall or Dijkstra’s) to account for multi-hop flights.  

4. **Feasibility Checks**  
   - Verifies that no plane exceeds its weight limit.  
   - Ensures packages arrive before their deadlines.  
   - Returns to the hub for the next pick-up, if multiple flights are needed.

---

## Input Files & Data Format

1. **Distance Matrix (e.g., `distance_matrix.json`)**  
   - A 2D array where the entry at `[i][j]` is the distance from airport `i` to airport `j`.  
   - `-1` indicates no direct route between `i` and `j`.  
   - Must be a valid square matrix of non-negative integers (aside from `-1`).  

   **Example of JSON input:**  
   ```json
   [
       [0, 100],
       [100, 0]
   ]
 

2. **Package Data (e.g., `package_data.json`)**  
   - Array of JSON objects, each describing one package:  
     - `id`: Integer, unique package ID  
     - `weight`: Integer, package weight  
     - `arrivalTime`: String in "HH:MM" (24-hour) format, time package arrives at the hub  
     - `deadlineTime`: String in "HH:MM" format, must be delivered by this time (within 24 hours)  
     - `destination`: Integer, ID of the destination airport  

   **Example of JSON input:**  
   ```json
   [
       { "id": 1, "weight": 500, "arrivalTime": "08:00", "deadlineTime": "12:00", "destination": 1 },
       { "id": 2, "weight": 500, "arrivalTime": "10:00", "deadlineTime": "12:00", "destination": 1 }
   ]


3. **Constraints (e.g., constraints.json)**  
   - An object with plane and flight-related settings:  
     - numOfPlanes: Integer, how many planes are available  
     - weightCapacity: Integer, max cargo weight each plane can hold (kg)  
     - speed: Integer, plane speed (km/h)
   Example of JSON input:
```json
      {
          "numOfPlanes": 2,
          "weightCapacity": 500,
          "speed": 700
      }

---

## How to Compile and Run

This project supports both **JavaScript (Node.js)** and **Haskell** implementations of the TEMOO Cargo scheduler. Below are instructions for compiling and running both versions.

---

### JavaScript (Node.js) Version

1. **Install Node.js**  
   Make sure you have Node.js installed. You can download it from [nodejs.org](https://nodejs.org/).

2. **Run the Program**  (Example command line argument)
   From the project root directory, run:
   ```bash
   node main.js distance_matrix.json package_data.json constraints.json
   ```
All
3. **Output**  
   - Outputs the optimal delivery schedule to the console.  
   - If no valid schedule exists under the given constraints, it reports the failure clearly.

---

### Haskell Version

1. **Compile the Haskell Code**  
   If using `cabal`, run:
   ```bash
   cabal clean
   cabal build
   ```
   If using `compile.sh`, ensure it contains:
   ```bash
   ghc Main.hs -o temoo_cargo
   ```

2. **Run the Program**  
   Once compiled, execute the program with the same input files:
   ```bash
   ./temoo_cargo distance_matrix.json package_data.json constraints.json
   ```

3. **Output**  
   - Displays scheduling information similar to the Node.js version.  
   - May print to the console or write to `solution.txt` depending on implementation.
---

Feel free to use either implementation depending on your workflow or platform. The input/output formats are identical to ensure consistency across both.
---

## Algorithm & Approach

1. **Parsing the Input**  
   - Load the distance matrix, package data, and constraints.  
   - Validate matrix dimensions, package fields, and constraint values.

2. **Graph Preparation**  
   - Compute shortest paths between airports (e.g., using Floyd–Warshall).  
   - Store results in a structure that easily retrieves distance/path for any pair of airports.

3. **Scheduling & Assignment**  
   - Sort or select packages by earliest deadline.  
   - Attempt to assign each package to one of the available planes (checking weight capacity, travel time, etc.).  
   - **Branch-and-Cut**: if adding a package becomes infeasible, backtrack and try alternate routes/plane assignments.

4. **Solution Recording**  
   - Track the total distance traveled for each partial/complete solution.  
   - Update a global “best solution” if a new route yields a lower total distance and satisfies all deadlines.

---

## Output & Results
A successful run displays (or writes to `solution.txt` or similar):

- **Total Distance** of the best solution.  
- **Number of Airplanes Used**.  
- **Schedules**: For each plane:  
  - The exact route (airport → airport)  
  - Takeoff time, arrival times, and total travel time  
  - Package IDs carried, their weights, and deadlines  
- **Feasibility Indicators**: Confirms that each package was delivered before its 24-hour deadline.

---

## Project Structure

While your **Haskell** implementation may differ in file layout, below is how the Node.js reference solution is organized. You can mirror a similar pattern for Haskell modules:

```
project-root/
├─ compile.sh              # Your Haskell compile script or Stack commands
├─ main.js                 # Main entry point (Node.js reference)
├─ parser.js               # Input file parsing & validation
├─ airport_graph_builder.js # Constructs airport network with shortest paths
├─ package.js              # Data structure/class for packages
├─ airplane.js             # Data structure/class for airplane & route feasibility
├─ pathFinder.js           # Core scheduling algorithm
├─ constraints.json        # Example constraints file
├─ distance_matrix.json    # Example distance matrix file
├─ package_data.json       # Example package data file
└─ README.md               # This file (project documentation)
```

---

## Contributing
1. **Fork** this repository.  
2. **Create** a feature branch.  
3. **Commit** changes with meaningful messages.  
4. **Open** a Pull Request describing your modifications or additions.  

We welcome improvements to:
- Algorithmic efficiency  
- Additional constraint support (e.g., multi-hub)  
- Testing and coverage  
- Documentation clarity  

---

## License
This project is distributed under the MIT License. Feel free to use, modify, and distribute it in your own projects, keeping the above license intact.

---

**Thank you for using TEMOO Cargo’s Haskell-based shipping simulator!** If you have any questions or feature requests, please open an issue. Enjoy building and optimizing your cargo delivery schedules!


