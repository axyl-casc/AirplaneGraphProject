const TIME_LIMIT = 24 * 60; // 24 hours in minutes

/**
 * Schedules deliveries for a single plane using the precomputed airport graph.
 * @param {Airport[]} graph - The airport network with shortest paths.
 * @param {Package[]} packages - Array of package objects.
 * @param {number} current_time - The current time in minutes.
 * @param {Airplane} plane - The single airplane object.
 * @returns {Object} - Flight schedule for the plane.
 */
function scheduleDeliveries(graph, packages, current_time, plane) {
    const hub = 0; // Central hub airport ID
    const planeSchedule = [];

    while (packages.length > 0) {
        // Calculate deadlineTime dynamically
        let availablePackages = packages.filter(pkg => 
            pkg.arrivalTime <= current_time && (pkg.arrivalTime + TIME_LIMIT) >= current_time
        );

        if (availablePackages.length === 0) {
            // If no packages are available, fast forward to the next package arrival
            current_time = Math.min(...packages.map(pkg => pkg.arrivalTime));
            continue;
        }

        const routePlan = findBestRoute(graph, availablePackages, plane, hub, current_time);

        if (!routePlan) break; // No valid route found

        planeSchedule.push(routePlan);
        current_time += routePlan.duration; // Advance time based on flight duration

        // Remove delivered packages
        packages = packages.filter(pkg => !routePlan.packages.includes(pkg.id));
    }

    return { plane: plane.id, flights: planeSchedule };
}

/**
 * Finds the best route for a plane to deliver packages from the hub.
 * Uses the **precomputed shortest paths** from the airport graph.
 * @param {Airport[]} graph - The airport network.
 * @param {Object[]} packages - Packages to deliver.
 * @param {Object} plane - Plane object with weight_capacity.
 * @param {number} hub - The central hub airport ID.
 * @returns {Object|null} - Best route with package details or null if no route found.
 */
function findBestRoute(graph, packages, plane, hub) {
    const selectedPackages = selectPackages(packages, plane.weight_capacity);
    if (selectedPackages.length === 0) return null;

    // Get shortest paths using precomputed graph data
    const destinations = selectedPackages.map(pkg => pkg.destination);
    const route = [hub, ...destinations, hub]; // Hub -> Destinations -> Hub

    let totalDistance = 0;
    for (let i = 0; i < route.length - 1; i++) {
        const from = route[i];
        const to = route[i + 1];
        totalDistance += graph[from].connections.get(to).distance;
    }

    return {
        route,
        packages: selectedPackages.map(pkg => pkg.id),
        distance: totalDistance
    };
}

/**
 * Selects packages based on plane weight capacity.
 * @param {Object[]} packages - Packages to deliver.
 * @param {number} capacity - Plane weight capacity.
 * @returns {Object[]} - Selected packages within capacity.
 */
function selectPackages(packages, capacity) {
    let totalWeight = 0;
    const selected = [];

    for (const pkg of packages) {
        if (totalWeight + pkg.weight <= capacity) {
            selected.push(pkg);
            totalWeight += pkg.weight;
        }
    }

    return selected;
}

module.exports = { scheduleDeliveries };

