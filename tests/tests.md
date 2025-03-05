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

## Test Cases Which Should Not Generate Solutions

## Test Cases Where Input Files Are Not in the Correct Format
