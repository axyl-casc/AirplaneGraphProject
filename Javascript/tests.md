# Test Documentation

This document outlines the testing strategy for the project, focusing on the Javascript implementation with integration testing.
All the test cases outlined passed/match expected outcomes. The integration test cases are the same as the Haskell version.

## Prerequisites

- Ensure that the necessary dependencies are installed (refer to the main [README.md](./README.md) if needed)

## External Library used

- Jest for test automation (Only for unit tests)

## Types of Tests

### Unit Tests

- **Purpose:** To test individual units or components of the code (e.g., functions, methods, classes)
- **Examples:**
  - Testing the `addPackage` method of the `Airplane` class in `airplane.js`
  - Testing the `parseConstraints` function in `parser.js`
- **To Run, Type the following in a terminal while in the project's root directory**:
  ```bash
  npm test
  ```

### Integration Tests

Integration tests are organized in the `project_root/testFiles/` directory. The directory contains a collection of test cases, each in its own subdirectory (e.g., `1`, `2`, `3`).

Each test case consists of three JSON files:

- `[test_case_number]_constraints.json`: Defines the number of airplanes, their weight capacity, and speed
- `[test_case_number]_dist.json`: Represents the distances between airports in a matrix format
- `[test_case_number]_pkg.json`: Contains an array of package objects with their properties (id, weight, arrival time, deadline time, destination)

## Running Integration Tests

First cd into the JavaScript solution directory.
For each test case, type the following in the terminal:

```bash
node main.js ../testFiles/[test_case_number]/[test_case_number]_dist.json ../testFiles/[test_case_number]/[test_case_number]_pkg.json ../testFiles/[test_case_number]/[test_case_number]_constraints.json
```

### Example:

```bash
node main.js ../testFiles/9/9_dist.json ../testFiles/9/9_pkg.json ../testFiles/9/9_constraints.json
```

A convenience script is provided to run all test cases in sequence:

```bash
bash runAllTestCases.sh
```

This will execute all test cases and report their results.

## Test Cases Which Should Generate Solutions

---

### Test Case 1: Single Package, Single Airplane

**Purpose:** Verifies basic functionality with minimal complexity.

**To Run:**

```bash
node main.js ../testFiles/1/1_dist.json ../testFiles/1/1_pkg.json ../testFiles/1/1_constraints.json
```

**Expected outcome:** The singular plane delivers the only package with the total distance of 200.

---

### Test Case 2: Two Packages, Single Airplane (Same Destination)

**Purpose:** Tests handling multiple packages going to the same location.

**To Run:**

```bash
node main.js ../testFiles/2/2_dist.json ../testFiles/2/2_pkg.json ../testFiles/2/2_constraints.json
```

**Expected outcome:** The singular plane takes both packages and the total distance should be 200.

---

### Test Case 3: 2 Packages, 2 Airplanes, Same Destination (Weight Capacity Test)

**Purpose:** Tests handling multiple packages that requires 2 airplanes going to the same location (due to weight capacity).

**To Run:**

```bash
node main.js ../testFiles/3/3_dist.json ../testFiles/3/3_pkg.json ../testFiles/3/3_constraints.json
```

**Expected outcome:** 2 Planes should both be flown to the same destination each carrying a package. Total distance should be 400

---

### Test Case 4: 2 Packages, 2 Airplanes, 2 Destinations (Weight Capacity Test)

**Purpose:** Tests handling multiple packages that requires 2 airplanes going to different locations (due to weight capacity).

**To Run:**

```bash
node main.js ../testFiles/4/4_dist.json ../testFiles/4/4_pkg.json ../testFiles/4/4_constraints.json
```

**Expected outcome:** 2 Planes should both be flown to the corresponding destination each carrying a package. Total Distance should be 2200.

---

### Test Case 5: 2 Packages, 2 Airplanes, 2 Destinations (Deadline Test)

**Purpose:** Tests handling multiple packages that requires 2 airplanes going to different locations (due to an expiring deadline).

**To Run:**

```bash
node main.js ../testFiles/5/5_dist.json ../testFiles/5/5_pkg.json ../testFiles/5/5_constraints.json
```

**Expected outcome:** 2 Planes should both be flown to the corresponding destination each carrying a package. Total Distance should be 2200.

---

### Test Case 6: 2 Packages, 2 Airplanes, 2 Destinations (Deadline Test)

**Purpose:** Tests handling multiple packages that requires only 1 airplanes going to different locations (deadlines don't expire)

**To Run:**

```bash
node main.js ../testFiles/6/6_dist.json ../testFiles/6/6_pkg.json ../testFiles/6/6_constraints.json
```

**Expected outcome:** 1 Plane should be flown to the corresponding destination dropping off 2 packages. Total Distance should be 2200.

---

### Test Case 7: 2 Packages, 2 Airplanes, Multiple Destinations (Cost Test)

**Purpose:** Seeing if we are able to find the minimum cost route

**To Run:**

```bash
node main.js ../testFiles/7/7_dist.json ../testFiles/7/7_pkg.json ../testFiles/7/7_constraints.json
```

**Expected outcome:** Cost/distance should be 3200(km) total

**Notes:** Through experimentation trying to find a situation where 1 plane would be more expensive than taking 2, with the
current way that we are calculating costs (where we are including the return to origin airport), cost will NEVER be a deciding
factor for taking additional aircrafts. It will always be done either because we failed to reach a deadline successfully or
because we have reached the weight capacity for an aircraft.

---

### Test Case 8: 4 Packages, 3 Airplanes, Multiple Destinations (Deadline Test)

**Purpose:** We have multiple planes and multiple packages, despite the fact that the weight of each package could easily fit
onto a single aircraft due to the deadlines we should see specific planes carry specific packages.

**To Run:**

```bash
node main.js ../testFiles/8/8_dist.json ../testFiles/8/8_pkg.json ../testFiles/8/8_constraints.json
```

**Expected outcome:** Plane 0 should be sent to Airport 3 with package 1,
Plane 1 should be sent to Airport 1 with package 2,3,
Plane 2 should be sent to Airport 2 with package 4.

---

## Test Cases Which Should Not Generate Solutions

---

### Test Case 9: 4 Packages, 3 Airplanes, Multiple Destinations (Deadline Test)

**Purpose:** Exact same scenario as Test Case 8, with tight deadlines but one fewer plane which makes it impossible

**To Run:**

```bash
node main.js ../testFiles/9/9_dist.json ../testFiles/9/9_pkg.json ../testFiles/9/9_constraints.json
```

**Expected outcome:** No solution.

---

### Test Case 10: 1 Package, 1 Plane (Weight Test)

**Purpose:** A package that is too large to fit on a plane cannot be delivered

**To Run:**

```bash
node main.js ../testFiles/10/10_dist.json ../testFiles/10/10_pkg.json ../testFiles/10/10_constraints.json
```

**Expected outcome:** No solution.

---

### Test Case 11: 1 Package, 1 Plane (Deadline Test)

**Purpose:** A package that arrives too late for it's destination cannot ever be delivered.

**To Run:**

```bash
node main.js ../testFiles/11/11_dist.json ../testFiles/11/11_pkg.json ../testFiles/11/11_constraints.json
```

**Expected outcome:** No solution.

## Test Cases Where Input Files Are Not in the Correct Format

---

### Test Case 12: (Format Test)

**Purpose:** Constraint data is not in the correct format.

**To Run:**

```bash
node main.js ../testFiles/12/12_dist.json ../testFiles/12/12_pkg.json ../testFiles/12/12_constraints.json
```

**Expected outcome:** Error message is printed.

---

### Test Case 13: (Format Test)

**Purpose:** Package data is not in the correct format.

**To Run:**

```bash
node main.js ../testFiles/13/13_dist.json ../testFiles/13/13_pkg.json ../testFiles/13/13_constraints.json
```

**Expected outcome:** Error message is printed.

---

### Test Case 14: (Format Test)

**Purpose:** If incorrect amount of parameters are passed.

**To Run:**

```bash
node main.js fakefile.json fakefile2.json
```

**Expected outcome:** Error message is printed.

---

### Test Case 15: (Format Test)

**Purpose:** If file cannot be found.

**To Run:**

```bash
node main.js fakefile.json fakefile2.json fakefile3.json
```

**Expected outcome:** Error message is printed.

---

### Test Case 16: Complex Test

**Purpose:** Tests handling of larger, more complex scenarios.

**To Run:**

```bash
node main.js ../testFiles/16/16_dist.json ../testFiles/16/16_pkg.json ../testFiles/16/16_constraints.json
```

**Expected outcome:** A valid solution that efficiently routes all packages.

**Note:** This test case may take longer to complete depending on hardware performance.
