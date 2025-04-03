# TEEMO Cargo - JavaScript Version

This is the JavaScript implementation of the **TEMOO Cargo** shipping service simulator.

## External Libraries

- **Jest** - Testing framework for Unit Testing

## Prerequisites

- **Node.js**
- **npm**

## How to run

1. Clone this repository
2. Cd into this directory
3. Install the dependencies:
   ```bash
   npm install
   ```
4. Run the application:
   ```bash
   node main <pathToDistanceMatrix> <pathToPkgData> <pathToConstraintsFile>
   ```

## Example Usage

```bash
node main.js ../testFiles/4/4_dist.json ../testFiles/4/4_pkg.json ../testFiles/4/4_constraints.json

```

## More Documentation

For detailed information about:

- Input/output formats
- Algorithm implementation details

Refer to the [general project README](../README.md).

## Testing

Refer to the [Testing Document](./tests.md)
