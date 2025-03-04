// For testing graphs:https://graphonline.top

const { parseAndLoadInputFiles } = require('./parser.js');
const { buildAirportNetwork } = require('./airport_graph_builder.js');
const { TemooPackage } = require('./package.js');
const { Airplane } = require('./airplane.js');
const { Route, scheduleDeliveries } = require('./pathFinder.js');

const [distanceMatrix, packageData, constraints] = parseAndLoadInputFiles();

const packages = packageData.map((pkg) => new TemooPackage(pkg));

const planes = [];
for (let i = 0; i < constraints.numOfPlanes; i++) {
	planes.push(new Airplane(constraints.speed, constraints.weightCapacity));
}

const airportNetwork = buildAirportNetwork(distanceMatrix);
var bestSolutionInfo = {
	bestTotal: Infinity,
	bestSolution: null,
	totalNumberOfSolutions: 0,
};
const deliveryPlan = scheduleDeliveries(packages, planes, 0, bestSolutionInfo, airportNetwork);

printSolution(deliveryPlan, airportNetwork);

/**
 * Formats and prints the solution in a readable format
 * @param {Object} bestSolutionInfo - Object containing the best solution found
 * @param {Object} airportNetwork - Network with travel distances between locations
 *  */
function printSolution(bestSolutionInfo, airportNetwork) {
	if (!bestSolutionInfo || !bestSolutionInfo.bestSolution) {
		return 'No solution found.';
	}

	const solution = bestSolutionInfo.bestSolution;
	let output = [];

	// Print header and overall statistics
	output.push('=== DELIVERY SOLUTION ===');
	output.push(`Total Distance: ${bestSolutionInfo.bestTotal.toFixed(2)} km`);
	output.push(`Total Solutions Evaluated: ${bestSolutionInfo.totalNumberOfSolutions}`);
	output.push(`Number of Airplanes Used: ${solution.length}`);
	output.push('');

	// Count total packages
	let totalPackages = 0;
	solution.forEach((airplane) => {
		totalPackages += airplane.route.packages.length;
	});
	output.push(`Total Packages Delivered: ${totalPackages}`);
	output.push('');

	// Print details for each airplane
	solution.forEach((airplane, index) => {
		output.push(`--- AIRPLANE ${index} ---`);
		output.push(`Speed: ${airplane.route.planeSpeed} km/h`);
		output.push(`Capacity: ${airplane.route.totalCapacity} kg`);
		output.push(
			`Load: ${airplane.route.totalWeight} kg (${((airplane.route.totalWeight / airplane.route.totalCapacity) * 100).toFixed(1)}%)`,
		);
		output.push(`Distance: ${airplane.route.totalDistance}`);
		output.push(`Departure Time: ${formatTime(airplane.route.departureTimeOrigin)} (HH:MM)`);
		output.push(`Return Time: ${formatTime(airplane.route.returnTimeOrigin)} (HH:MM)`);

		// Print detailed route with distances
		output.push('');
		output.push('DETAILED ROUTE:');

		// Initialize current location and time
		let currentLocation = 0; // Origin
		let currentTime = airplane.route.departureTimeOrigin;

		// Construct path array including origin, all destinations, and return to origin from the packages delivered
		const path = [0]; // Start at origin
		airplane.route.packages.forEach((pkg) => {
			path.push(pkg.destination);
		});
		path.push(0); // Return to origin

		// Display each leg of the journey
		for (let i = 0; i < path.length - 1; i++) {
			const from = path[i];
			const to = path[i + 1];

			// Get connection info
			const connection = airportNetwork[from].connections.get(to);
			const distance = connection.distance;
			const travelTime = (distance / airplane.route.planeSpeed) * 60; // minutes
			const path2 = connection.path;

			// Update current time
			currentTime += travelTime;

			// Create leg description
			let legDescription = `  Leg ${i + 1}: Location ${from} → Location ${to}`;
			legDescription += `\n`;
			path2.forEach((p) => {
				legDescription += `→ ${p} `;
			});

			legDescription += `\n    Distance: ${distance} km`;
			legDescription += `\n    Travel Time: ${formatTime(travelTime)} (hh:mm)`;
			legDescription += `\n    Arrival Time: ${formatTime(currentTime)} (hh:mm)`;

			// Add package information if this is a delivery (not return to origin)
			if (i < airplane.route.packages.length) {
				const pkg = airplane.route.packages[i];
				legDescription += `\n    Delivering: Package ${pkg.id} (${pkg.weight} kg)`;
				legDescription += `\n    Deadline: ${formatTime(pkg.deadlineTime)}`;

				// Calculate time margin
				const timeMargin = pkg.deadlineTime - currentTime;
				const marginStatus =
					timeMargin >= 0
						? `On time (${formatTime(timeMargin)} before deadline)`
						: `LATE by ${formatTime(-timeMargin)}`;
				legDescription += `\n    Status: ${marginStatus}`;
			} else {
				legDescription += '\n    Returning to Origin';
			}

			output.push(legDescription);
		}

		output.push('');
		const graphPath = [];
		for (let i = 0; i < path.length - 1; i++) {
			graphPath.push(`${path[i]}->${path[i + 1]}`);
		}
		output.push(`  ${graphPath.join(', ')}`);
		output.push('');
	});

	console.log(output.join('\n'));
}

/**
 * Formats minutes into HH:MM format
 * @param {Number} minutes - Time in minutes
 * @returns {String} - Formatted time string
 */
function formatTime(minutes) {
	const hours = Math.floor(minutes / 60);
	const mins = Math.floor(minutes % 60);
	return `${hours.toString().padStart(2, '0')}:${mins.toString().padStart(2, '0')}`;
}
