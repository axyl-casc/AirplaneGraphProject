
/**
 * Schedules deliveries for planes using the precomputed airport graph.
 * @param {Airport[]} graph - The airport network with shortest paths.
 * @param {Object[]} packages - Array of package objects {id, weight, destination}.
 * @param {Object[]} planes - Array of plane objects {id, weight_capacity}.
 * @returns {Object[]} - Array of flight schedules for each plane.
 */
function scheduleDeliveries(graph, packages, current_time, plane) {
    // plane is for only one plane
    // current_time so only packages with an arrival time lower than current_time are allowed
    const hub = 0; // Assuming the hub is airport 0
    const scheduledFlights = [];

    for (const plane of planes) {
        let remainingPackages = [...packages];
        const planeSchedule = [];

        while (remainingPackages.length > 0) {
            const routePlan = findBestRoute(graph, remainingPackages, plane, hub);
            if (!routePlan) break; // No feasible route found

            planeSchedule.push(routePlan);
            remainingPackages = remainingPackages.filter(pkg => !routePlan.packages.includes(pkg.id));
        }

        scheduledFlights.push({ plane: plane.id, flights: planeSchedule });
    }

    return scheduledFlights;
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

