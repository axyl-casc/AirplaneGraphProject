const TIME_LIMIT = 24 * 60; // 24 hours in minutes
const TURNAROUND_TIME = 0; // minutes

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

	console.log('Starting delivery scheduling...');

	while (packages.length > 0) {
		let availablePackages = packages.filter(
			(pkg) => pkg.arrivalTime <= current_time && current_time <= pkg.arrivalTime + TIME_LIMIT,
		);

		if (availablePackages.length === 0) {
			current_time = Math.min(...packages.map((pkg) => pkg.arrivalTime));
			continue;
		}
		console.log('kevin' + current_time);

		const routePlan = findBestRoute(graph, availablePackages, plane, hub, current_time);
		if (!routePlan) break;

		planeSchedule.push(routePlan);
		current_time = routePlan.time; // Correctly update to flight's end time

		packages = packages.filter((pkg) => !routePlan.packages.includes(pkg.id));
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
 * @param {number} current_time - The current time in minutes.
 * @returns {Object|null} - Best route with package details or null if no route found.
 */
function findBestRoute(graph, packages, plane, hub, current_time) {
	const selectedPackages = selectPackages(packages, plane.weightCapacity);
	if (selectedPackages.length === 0) return null;

	const destinations = selectedPackages.map((pkg) => pkg.destination);
	const route = [hub, ...destinations, hub];

	let totalDistance = 0;
	for (let i = 0; i < route.length - 1; i++) {
		const from = route[i];
		const to = route[i + 1];
		totalDistance += graph[from].connections.get(to).distance;
	}

	const flightDuration = (totalDistance / plane.planeSpeed) * 60; // Convert hours to minutes
	const flightEndTime = flightDuration;

	// Ensure delivery is within the package's deadline
	const allDeadlinesMet = selectedPackages.every(
		(pkg) => flightEndTime <= pkg.arrivalTime + TIME_LIMIT,
	);

	if (!allDeadlinesMet) return null; // Skip if any package would be late

	return {
		route,
		packages: selectedPackages.map((pkg) => pkg.id),
		time: flightEndTime,
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

	console.log('Selecting packages within weight capacity:', capacity);
	for (const pkg of packages) {
		if (totalWeight + pkg.weight <= capacity) {
			selected.push(pkg);
			totalWeight += pkg.weight;
			console.log(`Package ${pkg.id} added. Total weight:`, totalWeight);
		}
	}

	return selected;
}

module.exports = { scheduleDeliveries };
