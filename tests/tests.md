# Test Documentation

This document outlines the testing strategy for the project, focusing on the integration testing.

## Prerequisites

- Ensure that the necessary dependencies installed (refer to the main `README.md` if needed)

## External Library used

- Jest for test automation (Only for unit tests)

## Types of Tests

### Unit Tests

- **Purpose:** To test individual units or components of the code (e.g., functions, methods, classes)
- **Examples:**
  - Testing the `addPackage` method of the `Airplane` class in `airplane.js`
  - Testing the `parseConstraints` function in `parser.js`
- **To Run, Type the following in a terminal while in the projects's root directory**:
  ```bash
  npm test
  ```

### Integration Tests

Integration tests are organized in the `tests/integration_tests/` directory. The directory contains a collection of test cases, each in its own subdirectory (e.g., `1`, `2`, `3`).

Each test case consists of three JSON files:

- `constraints.json`: Defines the number of airplanes, their weight capacity, and speed
- `distance.json`: Represents the distances between airports in a matrix format
- `package.json`: Contains an array of package objects with their properties (id, weight, arrival time, deadline time, destination)

## Running Integration Tests

For each test case, type the following in the terminal while in the main directory:

```bash
node main.js tests/integration_tests/[test_case_number]/[test_case_number]_dist.json tests/integration_tests/[test_case_number]/[test_case_number]_pkg.json tests/integration_tests/[test_case_number]/[test_case_number]_constraints.json
```

## Test Cases Which Should Generate Solutions

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 1: Single Package, Single Airplane

**Purpose:** Verifies basic functionality with minimal complexity.

```json
// 1_constraints.json
{
  "numOfPlanes": 1,
  "weightCapacity": 500,
  "speed": 700
}

// 1_dist.json
[
  [0, 100],
  [100, 0]
]

// 1_pkg.json
[
  { "id": 1, "weight": 150, "arrivalTime": "08:00", "deadlineTime": "12:00", "destination": 1 }
]
```

**To Run, Type the following in a terminal while in the projects's root directory**:

```bash
node main.js tests/integration_tests/1/1_dist.json tests/integration_tests/1/1_pkg.json tests/integration_tests/1/1_constraints.json
```

**Expected outcome:** The singular plane delivers the only package with the total distance of 200.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 2: Two Packages, Single Airplane (Same Destination)

**Purpose:** Tests handling multiple packages going to the same location.

```json
// 2_constraints.json
{
  "numOfPlanes": 1,
  "weightCapacity": 500,
  "speed": 700
}

// 2_dist.json
[
  [0, 100],
  [100, 0]
]

// 2_pkg.json
[
  { "id": 1, "weight": 150, "arrivalTime": "08:00", "deadlineTime": "12:00", "destination": 1 },
  { "id": 2, "weight": 200, "arrivalTime": "08:30", "deadlineTime": "14:00", "destination": 1 }
]
```

**To Run, Type the following in a terminal while in the projects's root directory**:

```bash
node main.js tests/integration_tests/2/2_dist.json tests/integration_tests/2/2_pkg.json tests/integration_tests/2/2_constraints.json
```

**Expected outcome:** The singular plane takes both packages and the total distance should be 200.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 3: 2 Packages, 2 Airplanes, Same Destination (Weight Capacity Test)

**Purpose:** Tests handling multiple packages that requires 2 airplanes going to the same location (due to weight capacity).

```bash
node main.js tests/integration_tests/3/3_dist.json tests/integration_tests/3/3_pkg.json tests/integration_tests/3/3_constraints.json
```

**Expected outcome:** 2 Planes should both be flown to the same destination each carrying a package.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 4: 2 Packages, 2 Airplanes, 2 Destinations (Weight Capacity Test)

**Purpose:** Tests handling multiple packages that requires 2 airplanes going to different locations (due to weight capacity).

```bash
node main.js tests/integration_tests/4/4_dist.json tests/integration_tests/4/4_pkg.json tests/integration_tests/4/4_constraints.json
```

**Expected outcome:** 2 Planes should both be flown to the corresponding destination each carrying a package.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 5: 2 Packages, 2 Airplanes, 2 Destinations (Deadline Test)

**Purpose:** Tests handling multiple packages that requires 2 airplanes going to different locations (due to an expiring deadline).

```bash
node main.js tests/integration_tests/5/5_dist.json tests/integration_tests/5/5_pkg.json tests/integration_tests/5/5_constraints.json
```

**Expected outcome:** 2 Planes should both be flown to the corresponding destination each carrying a package.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 6: 2 Packages, 2 Airplanes, 2 Destinations (Deadline Test)

**Purpose:** Tests handling multiple packages that requires only 1 airplanes going to different locations (deadlines don't expire)

```bash
node main.js tests/integration_tests/6/6_dist.json tests/integration_tests/6/6_pkg.json tests/integration_tests/6/6_constraints.json
```

**Expected outcome:** 1 Plane should be flown to the corresponding destination dropping off 2 packages.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 7: 2 Packages, 2 Airplanes, Multiple Destinations (Cost Test)

**Purpose:** Seeing if we are able to find the minimum cost route

```bash
node main.js tests/integration_tests/7/7_dist.json tests/integration_tests/7/7_pkg.json tests/integration_tests/7/7_constraints.json
```

```json
//the graph
[
	[  0, 500,  600,   -1],
	[500,   0,   -1,  500],
  [600,  -1,    0, 2000],
	[ -1, 500, 2000,    0]
]
```

**Expected outcome:** Cost/distance should be 3200(km) total

**Notes:** Through experimentation trying to find a situation where 1 plane would be more expensive than taking 2, with the 
current way that we are calculating costs (where we are including the return to origin airport), cost will NEVER be a deciding
factor for taking additional aircrafts. It will always be done either because we failed to reach a deadline successfully or 
because we have reached the weight capacity for an aircraft.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 8: 4 Packages, 3 Airplanes, Multiple Destinations (Deadline Test)

**Purpose:** We have multiple planes and multiple packages, despite the fact that the weight of each package could easily fit
onto a single aircraft due to the deadlines we should see specific planes carry specific packages.

```bash
node main.js tests/integration_tests/8/8_dist.json tests/integration_tests/8/8_pkg.json tests/integration_tests/8/8_constraints.json
```

```json
//the graph used
[
	[  0, 500,  600,   -1],
	[500,   0,   -1,   -1],
  [600,  -1,    0, 2000],
	[ -1,  -1, 2000,    0]
]

//package data used
[
	  { "id": 1, "weight": 50, "arrivalTime": "20:00", "deadlineTime": "24:00", "destination": 4 },
    { "id": 2, "weight": 50, "arrivalTime": "22:00", "deadlineTime": "24:00", "destination": 1 },
    { "id": 3, "weight": 50, "arrivalTime": "23:00", "deadlineTime": "24:00", "destination": 1 },
    { "id": 4, "weight": 50, "arrivalTime": "23:00", "deadlineTime": "24:00", "destination": 2 }

]
```

**Expected outcome:** Plane 0 should be sent to Airport 3 with package 1,
                      Plane 1 should be sent to Airport 1 with package 2,3,
                      Plane 2 should be sent to Airport 2 with package 4.

-------------------------------------------------------------------------------------------------------------------------------------
## Test Cases Which Should Not Generate Solutions
-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 9: 4 Packages, 3 Airplanes, Multiple Destinations (Deadline Test)

**Purpose:** Exact same scenario as Test Case 8, with tight deadlines but one fewer plane which makes it impossible

```bash
node main.js tests/integration_tests/9/9_dist.json tests/integration_tests/9/9_pkg.json tests/integration_tests/9/9_constraints.json
```

**Expected outcome:** No solution.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 10: 1 Package, 1 Plane (Weight Test)

**Purpose:** A package that is too large to fit on a plane cannot be delivered

```bash
node main.js tests/integration_tests/10/10_dist.json tests/integration_tests/10/10_pkg.json tests/integration_tests/10/10_constraints.json
```

```json
//constraints
{ 
    "numOfPlanes": 1, 
    "weightCapacity": 500, 
    "speed": 700 
}

//package data
[
	{ "id": 1, "weight": 600, "arrivalTime": "08:00", "deadlineTime": "24:00", "destination": 1 }
]

```

**Expected outcome:** No solution.

-------------------------------------------------------------------------------------------------------------------------------------

### Test Case 11: 1 Package, 1 Plane (Deadline Test)

**Purpose:** A package that arrives too late for it's destination cannot ever be delivered.

```bash
node main.js tests/integration_tests/11/11_dist.json tests/integration_tests/11/11_pkg.json tests/integration_tests/11/11_constraints.json
```

**Expected outcome:** No solution.

## Test Cases Where Input Files Are Not in the Correct Format
