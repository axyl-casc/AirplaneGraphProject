# TEEMO Cargo - JavaScript Version

> Modern cargo optimization for efficient logistics

This is the JavaScript implementation of the TEEMO Cargo optimization system, providing efficient route planning and package distribution.

## External Libraries

- **Jest** - Testing framework for unit Testing

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
node main ./sample_input_files/sample_distance_matrix.json ./sample_input_files/data/sample_packages.json ./sample_input_files/data/sample_constraints.json
```

## More Documentation

For detailed information about:

- Input/output formats
- Algorithm implementation details
- Performance benchmarks
- Comparison with Haskell implementation

Please refer to the [general project README](../README.md).

## Testing

Please refer to the [Testing Document](./tests.md)
