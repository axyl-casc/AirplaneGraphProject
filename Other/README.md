# TEMOO Cargo – Python Benchmarking Script

This README describes `generate_graphs.py`, a standalone Python script used to benchmark and compare the performance of the JavaScript and Haskell implementations of the TEMOO Cargo delivery scheduler.

## Purpose

The script automates the following tasks:
- Generates random test inputs (distance matrix, package data, and constraints)
- Runs the Haskell and JavaScript versions of the scheduler
- Times their execution
- Plots the performance results using matplotlib

## File Structure

```
Python/
├── generate_graphs.py         # Main Python script for test generation, execution, timing, and graphing
└── README.md                  # This file
```

## Requirements

Install the necessary Python package:

```bash
pip install matplotlib
```

Also ensure:
- Node.js is installed and the JavaScript implementation is runnable
- GHC and Cabal are installed for Haskell, and the Haskell implementation is compiled

## How to Run

1. Navigate to the Python directory:

```bash
cd Python
```

2. Run the script:

```bash
python generate_graphs.py
```

3. The script will:
   - Generate random test cases
   - Run both JavaScript and Haskell versions on each test
   - Measure and log execution times
   - Display a matplotlib graph comparing runtimes

## Output

- Console output logs execution times and progress
- A matplotlib graph is displayed comparing performance between the two implementations

## Notes

- Ensure both implementations are functional and accessible from the script (e.g., correct file paths)
- All generated JSON files are saved in the `testcases/` directory and can be reused
- The graph helps visualize how each implementation scales with problem size

---

This script provides a fast and visual way to compare the performance and scalability of the JavaScript and Haskell versions of TEMOO Cargo.
