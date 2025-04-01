#!/usr/bin/env python3
import json
import random
import time
import subprocess
import matplotlib.pyplot as plt
def generate_constraints(filename="constraints.json", numOfPlanes=5,weightCapacity=500, speed=700):
    """Generates a constraints JSON file similar to your sample."""
    data = {
        "numOfPlanes": numOfPlanes,
        "weightCapacity": weightCapacity,
        "speed": speed
    }
    with open(filename, "w") as f:
        json.dump(data, f, indent=2)

def generate_distance_matrix(filename="distance_matrix.json", size=5):
    """Generates a distance matrix JSON file.
    
    The matrix is of given size; the diagonal entries are 0 and off-diagonals are random distances.
    There is a small chance to mark a value as unreachable (-1).
    """
    matrix = []
    for i in range(size):
        row = []
        for j in range(size):
            if i == j:
                row.append(0)
            else:
                # 10% chance to be unreachable (-1)
                if random.random() < 0.1:
                    row.append(-1)
                else:
                    row.append(random.randint(5, 50))
        matrix.append(row)
    with open(filename, "w") as f:
        json.dump(matrix, f, indent=2)

def generate_package_data(filename="package_data.json", num_packages=5, num_nodes=5):
    """Generates a package data JSON file.
    
    Each package includes:
      - id: The package identifier.
      - weight: A fixed weight.
      - arrivalTime: Computed as HH:MM.
      - destination: An integer computed from num_nodes.
      - deadline: Arrival time plus 10 hours (with wrap-around on a 24-hour clock).
    """
    packages = []
    for i in range(1, num_packages + 1):
        weight = 50
        hour = i % 12  # arrival time hour (0-11)
        minute = i % 60
        arrival_time = f"{hour:02d}:{minute:02d}"
        # Compute deadline: add 10 hours to the arrival hour, mod 24 for proper formatting.
        deadline_hour = (hour + 24)
        deadline = f"{deadline_hour:02d}:{minute:02d}"
        destination = (i % num_nodes)
        if destination <= 0:
            destination = num_nodes - 2
        packages.append({
            "id": i,
            "weight": weight,
            "arrivalTime": arrival_time,
            "destination": destination,
            "deadlineTime": deadline
        })
    with open(filename, "w") as f:
        json.dump(packages, f, indent=2)

def run_command(command):
    """Runs a given command and returns the elapsed time along with standard output and error."""
    start = time.time()
    result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    end = time.time()
    elapsed = end - start
    return elapsed, result.stdout.decode(), result.stderr.decode()

def experiment_vary_packages():
    """Experiment: Vary the number of packages while keeping the distance matrix size fixed.
    
    This function runs both commands for each package count, measures the execution times,
    prints the output and errors from each program, and plots a graph comparing the performance.
    """
    package_counts = [5, 10, 15, 20,21,22,23,24,25]
    times_exe = []
    times_js = []
    
    for count in package_counts:
        # Generate the files using fixed matrix size and constraints.
        generate_constraints()
        # keep old distance matrix the same
        #generate_distance_matrix(size=5)
        generate_package_data(num_packages=count, num_nodes=17)
        
        # Define the commands to test.
        cmd_exe = ["hs/main.exe", "distance_matrix.json", "package_data.json", "constraints.json"]
        cmd_js = ["node", "js/main.js", "distance_matrix.json", "package_data.json", "constraints.json"]
        
        elapsed_exe, out_exe, err_exe = run_command(cmd_exe)
        elapsed_js, out_js, err_js = run_command(cmd_js)
        
        print(f"Packages: {count} -> hs/main.exe took {elapsed_exe:.4f} sec")
        print("Output from hs/main.exe:")
        print(out_exe)
        if err_exe:
            print("Errors from hs/main.exe:")
            print(err_exe)
        print("-" * 60)
        
        print(f"Packages: {count} -> node js/main.js took {elapsed_js:.4f} sec")
        print("Output from node js/main.js:")
        print(out_js)
        if err_js:
            print("Errors from node js/main.js:")
            print(err_js)
        print("=" * 60)
        
        times_exe.append(elapsed_exe)
        times_js.append(elapsed_js)
    
    # Plot the results
    plt.figure()
    plt.plot(package_counts, times_exe, marker='o', label="Haskell")
    plt.plot(package_counts, times_js, marker='o', label="Nodejs")
    plt.xlabel("Number of Packages")
    plt.ylabel("Execution Time (seconds)")
    plt.title("Execution Time vs. Number of Packages")
    plt.legend()
    plt.grid(True)
    plt.savefig("runtime_vs_packages.png")
    plt.show()


def main():
    print("Running experiment: Vary Number of Packages")
    experiment_vary_packages()
    

if __name__ == "__main__":
    main()
