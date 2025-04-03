# Teemo Cargo

Welcome to the **TEMOO Cargo** shipping service simulator! This project demonstrates how to schedule and route cargo plane deliveries to various airports, subject to constraints such as weight capacity, flight distances, and package deadlines. The project includes both Haskell and JavaScript implementations with the same core functionality.

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Core Features](#core-features)
3. [Input Files & Data Format](#input-files--data-format)
4. [Implementation Options](#implementation-options)
5. [Algorithm & Approach](#algorithm--approach)
6. [Output & Results](#output--results)
7. [Project Structure](#project-structure)
8. [Testing](#testing)
9. [Performance Benchmarking](#performance-benchmarking)

---

## Project Overview

TEMOO Cargo is an expedited cargo delivery system that schedules cargo planes from a central hub airport to various destinations. Packages have arrival times, deadlines, and weights. A limited number of planes operate flights from the hub to distribute packages while complying with constraints on:

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
   - Packages have a deadline time.
   - Packages have an arrival time so they cannot be delivered before it.
   - Planes can only have a single route (They cannot have multiple routes).

3. **Scheduling Algorithm**

   - Branch-and-bound and backtracking approach to explore different plane assignments, route permutations, and reduce search space.
   - Shortest path calculations (e.g., Floyd–Warshall or Dijkstra's) to pre-calculate the shortest distance between each node.

4. **Feasibility Checks**
   - Verifies that no plane exceeds its weight limit.
   - Ensures packages arrive before their deadlines.

---

## Input Files & Data Format

1. **Distance Matrix (e.g., `distance_matrix.json`)**

   - A 2D array where the entry at `[i][j]` is the distance from airport `i` to airport `j`.
   - `-1` indicates no direct route between `i` and `j`.
   - Must be a valid square matrix of non-negative integers (aside from `-1`).
   - The matrix must represent an undirected graph.

   **Example of JSON input:**

   ```json
   [
     [0, 100],
     [100, 0]
   ]
   ```

2. **Package Data (e.g., `package_data.json`)**

   - Array of JSON objects, each describing one package:
     - `id`: Integer, unique package ID
     - `weight`: Integer, package weight
     - `arrivalTime`: String in "HH:MM" (24-hour) format, time package arrives at the hub
     - `deadlineTime`: String in "HH:MM" format, must be delivered by this time
     - `destination`: Integer, ID of the destination airport

   **Example of JSON input:**

   ```json
   [
     {
       "id": 1,
       "weight": 500,
       "arrivalTime": "08:00",
       "deadlineTime": "12:00",
       "destination": 1
     },
     {
       "id": 2,
       "weight": 500,
       "arrivalTime": "10:00",
       "deadlineTime": "12:00",
       "destination": 1
     }
   ]
   ```

3. **Constraints (e.g., constraints.json)**

   - An object with plane and flight-related settings:
     - numOfPlanes: Integer, how many planes are available
     - weightCapacity: Integer, max cargo weight each plane can hold (kg)
     - speed: Integer, plane speed (km/h)

   **Example of JSON input:**

   ```json
   {
     "numOfPlanes": 2,
     "weightCapacity": 500,
     "speed": 700
   }
   ```

---

## Implementation Options

This project provides two separate implementations of the TEMOO Cargo scheduler:

### [JavaScript (Node.js) Implementation](./Javascript/README.md)

- **Details**: [JavaScript Implementation README](./Javascript/README.md)
- **Testing**: [JavaScript Testing Documentation](./Javascript/tests.md)

### [Haskell Implementation](./Haskell/README.md)

- **Details**: [Haskell Implementation README](./Haskell/README.md)
- **Testing**: [Haskell Testing Documentation](./Haskell/tests.md)

Both implementations share the same algorithm approach and produce the same results. However, the haskell solution is significantly faster as the search space increase.

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
   - **Branch-and-Bound**: if adding a package becomes infeasible, backtrack and try alternate routes/plane assignments. This allows the search space to be reduced.

4. **Solution Recording**
   - Track the total distance traveled for each partial/complete solution.
   - Update a global "best solution" if a new route yields a lower total distance and satisfies all deadlines.

---

## Output & Results

A successful run displays:

- **Number of Valid Solutions**
- **Number of nodes explored** - Total number of recursive calls in the main algorithm
- **Total Distance** of the best solution
- **Schedules**: For each plane:
  - The exact route (airport → airport)
  - Takeoff time, arrival times, and total travel time
  - Package IDs, their weights, and deadlines

---

## Project Structure

The project is organized into several main directories:

- `./Javascript/` - Contains the Node.js implementation

  - `./Javascript/tests/` - Unit tests for JavaScript implementation
  - `./Javascript/runAllTestCases.sh` - Bash script to run all integration tests

- `./Haskell/` - Contains the Haskell implementation

  - `./Haskell/tests/unit_tests/` - Unit tests for Haskell implementation
  - `./Haskell/compileAndRunALLTests.sh` - Bash script to run all integration tests

- `./testFiles/` - Contains common test cases used by both the Javascript and Haskell implementation

  - `./Other/` - Performance benchmarking tool in Python

For specific details about each implementation's internal structure, please refer to their respective README files linked above.

---

## Testing

### Integration Testing

The project includes 16 standard test cases that exercise different aspects of the routing algorithm:

1. Basic single package delivery
2. Multiple packages to the same destination
3. Weight capacity constraints
4. Multiple destinations with deadlines
5. Complex multi-package, multi-plane scenarios

### Test Files Location

- Located in `./testFiles/`
- **JavaScript tests**: Script in `./Javascript/runAllTestCases.sh` to run all tests
- **Haskell tests**: Script in `./Haskell/compileAndRunALLTests.sh` to run all tests

### Test Documentation

- [JavaScript Testing Documentation](./Javascript/tests.md)
- [Haskell Testing Documentation](./Haskell/tests.md)

---

## Performance Benchmarking

The project includes Python-based benchmarking tool to compare the performance of the JavaScript and Haskell implementations in the `root/Other`:

### [Python Benchmarking Tools](./Other/Python/README.md)

---
