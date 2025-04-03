# TEMOO Cargo – Haskell Version

This is the Haskell implementation of the TEEMO Cargo optimization system, providing efficient route planning and package distribution.

## External Libraries

- **Aeson** - For Parsing Json input files

## Prerequisites

- **Cabal** ([Install guide](https://www.haskell.org/cabal/))
- **GHCup** ([Install guide](https://www.haskell.org/ghcup/))

### Installing Prerequisites

1. **Install GHCup** (Haskell toolchain installer)

   ```bash
   # macOS/Linux
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

   # Windows
   # Download the installer from https://www.haskell.org/ghcup/
   ```

2. **Install GHC** (Glasgow Haskell Compiler)

   ```bash
   ghcup install ghc
   ghcup set ghc
   ```

3. **Install Cabal**

   ```bash
   ghcup install cabal
   ghcup set cabal
   ```

4. **Install Project Dependencies**
   ```bash
   cabal update
   cabal install --only-dependencies
   ```

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

If moved to `./build/` as suggested, run:

```bash
./build/AirplaneGraphProject <pathToDistanceMatrix> <pathToPkgData> <pathToConstraintsFile>
```

Or, if using the default location:

```bash
./dist-newstyle/build/<arch>-<os>/ghc-<ghc_version>/AirplaneGraphProject-0.1.0.0/x/AirplaneGraphProject/build/AirplaneGraphProject/AirplaneGraphProject <pathToDistanceMatrix> <pathToPkgData> <pathToConstraintsFile>
```

## Example Usage

```bash
cabal run AirplaneGraphProject -- ./sample_input_files/sample_distance_matrix.json ./sample_input_files/data/sample_packages.json ./sample_input_files/data/sample_constraints.json
```

Or with the executable:

```bash
./build/AirplaneGraphProject ./sample_input_files/sample_distance_matrix.json ./sample_input_files/data/sample_packages.json ./sample_input_files/data/sample_constraints.json
```

## More Documentation

For detailed information about:

- Input/output formats
- Algorithm implementation details
- Performance benchmarks
- Comparison with Javascript implementation
  Please refer to the [general project README](../README.md).

## Testing

Please refer to the [Testing Document](./tests.md).
