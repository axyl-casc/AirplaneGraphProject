# TEMOO Cargo – Haskell Version

This is the Haskell implementation of the TEEMO Cargo optimization system, providing efficient route planning and package distribution.

## External Libraries

- **Aeson** - For Parsing Json input files

## Prerequisites

- **Cabal** ([Install guide](https://www.haskell.org/cabal/))
- **GHCup** ([Install guide](https://www.haskell.org/ghcup/))
- **Ghc**([Install guide](https://www.haskell.org/ghc/))

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

### Executable Location

After building, the compiled executable is stored at:

```
./dist-newstyle/build/<arch>-<os>/ghc-<ghc_version>/AirplaneGraphProject-0.1.0.0/x/AirplaneGraphProject/build/AirplaneGraphProject/AirplaneGraphProject
```

To find the exact location dynamically, run:

```bash
cabal list-bin exe:AirplaneGraphProject
```

Alternatively, you can move the executable to a custom location using:

```bash
mkdir -p build
mv "$(cabal list-bin exe:AirplaneGraphProject)" ./build/AirplaneGraphProject
```

3. **Run the Program**

### Option 1: Using the Cabal Runner

```bash
cabal run AirplaneGraphProject -- <pathToDistanceMatrix> <pathToPkgData> <pathToConstraintsFile>
```

### Option 2: Using the Compiled Executable

If moved to `./build/`, run:

```bash
./build/AirplaneGraphProject <pathToDistanceMatrix> <pathToPkgData> <pathToConstraintsFile>
```

Or, if using the default location:

```bash
./dist-newstyle/build/<arch>-<os>/ghc-<ghc_version>/AirplaneGraphProject-0.1.0.0/x/AirplaneGraphProject/build/AirplaneGraphProject/AirplaneGraphProject <pathToDistanceMatrix> <pathToPkgData> <pathToConstraintsFile>
```

## Example Usage

```bash
cabal run AirplaneGraphProject -- ../testFiles/3/3_dist.json ../testFiles/3/3_pkg.json ../testFiles/3/3_constraints.json
```

Or with the executable:

```bash
./build/AirplaneGraphProject ../testFiles/3/3_dist.json ../testFiles/3/3_pkg.json ../testFiles/3/3_constraints.json
```

## More Documentation

For detailed information about:

- Input/output formats
- Algorithm implementation details

  Refer to the [general project README](../README.md).

## Testing

Refer to the [Testing Document](./tests.md).
