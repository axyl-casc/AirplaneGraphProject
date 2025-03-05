// For testing graphs:https://graphonline.top

const { parseAndLoadInputFiles } = require('./parser.js');
const { buildAirportNetwork, Airport } = require('./airport_graph_builder.js');
const { TemooPackage } = require('./package.js');
const { Airplane } = require('./airplane.js');
const { scheduleDeliveries } = require('./pathFinder.js');

//Load the input Files
const [distanceMatrix, packageData, constraints] = parseAndLoadInputFiles();
const numOfPackages = packageData.length;
// Create the package objects
const packages = packageData.map((pkg) => new TemooPackage(pkg));

// Create the planes objects
const planes = [];
for (let i = 0; i < constraints.numOfPlanes; i++) {
	planes.push(new Airplane(constraints.speed, constraints.weightCapacity));
}

// build and initialize the airport network to be used for finding the path from one airport to another
const airportNetwork = buildAirportNetwork(distanceMatrix);
Airplane.setAirportNetwork(airportNetwork);

// Initial Solution
let optimalSolution = {
	optimalPlaneConfig: null,
	totalNodesExplored: 0,
	validSolutionsCount: 0,
	optimalMinDistance: Infinity,
};

// Find the solution
const optimalDeliverySchedule = scheduleDeliveries(packages, planes, 0, optimalSolution);
printSolution(optimalDeliverySchedule, airportNetwork, numOfPackages);

/**
 * Prints the solution
 *
 * @param {object} optimalDeliverySchedule - An object containing the optimal solution
 * @param {object} airportNetwork - An object representing the airport network with connections.
 * @param {Number} numOfPackages  - Number of packages that were delivered
 */
function printSolution(optimalDeliverySchedule, airportNetwork, numOfPackages) {
	const deliveryPlan = optimalDeliverySchedule.optimalPlaneConfig;
	const report = [];

	report.push('=== DELIVERY SOLUTION ===');
	report.push(`Number of  Valid Solutions: ${optimalDeliverySchedule.validSolutionsCount}`);
	report.push(`Number of Nodes Explored: ${optimalDeliverySchedule.totalNodesExplored}`);

	// If a solution was not found
	if (!optimalDeliverySchedule.optimalPlaneConfig) {
		report.push('No solution found.');
		console.log(report.join('\n'));
		return;
	}
	report.push(
		`Total Distance for optimal solution: ${optimalDeliverySchedule.optimalMinDistance} km`,
	);

	report.push(`Number of Airplanes Used: ${deliveryPlan.length}`);
	report.push(`Airplane Capacity: ${deliveryPlan[0].totalWeightCapacity} kg`);
	report.push(`Airplane Speed: ${deliveryPlan[0].speed} km/h`);
	report.push('');

	report.push(`Total Packages Delivered: ${numOfPackages}`);
	report.push('');

	// Each Airplane's Details
	deliveryPlan.forEach((airplane, index) => {
		report.push(`--- AIRPLANE ${index} ---`);
		report.push(`Total Load: ${airplane.currentLoad} kg `);
		report.push(`Total Distance Traveled: ${airplane.totalDistance} km`);
		report.push(`Departure Time: ${formatTime(airplane.departureTimeOrigin)} (HH:MM)`);
		report.push(`Return Time: ${formatTime(airplane.returnTimeOrigin)} (HH:MM)`);
		report.push('');

		report.push('ROUTE DETAILS:');
		let currentAirport = 0; // Origin
		let currentTime = airplane.departureTimeOrigin;

		// Print the path taken to deliver the packages
		airplane.packages.forEach((packageInfo, packageIndex) => {
			const connection = airportNetwork[currentAirport].connections.get(packageInfo.destination);
			const legDistance = connection.distance;
			const legTravelTimeMinutes = (legDistance / airplane.speed) * 60;
			currentTime += legTravelTimeMinutes;

			let legDescription = `  Leg ${packageIndex + 1}: ${airportNetwork[currentAirport].formatPathToAirport(packageInfo.destination)}`;
			legDescription += `\n    Distance: ${legDistance} km`;
			legDescription += `\n    Travel Time: ${formatTime(legTravelTimeMinutes)} (HH:MM)`;
			legDescription += `\n    Arrival Time: ${formatTime(currentTime)} (HH:MM)`;
			legDescription += `\n    Delivering: Package ${packageInfo.id} (${packageInfo.weight} kg)`;
			legDescription += `\n    Deadline: ${formatTime(packageInfo.deadlineTime)} (HH:MM)`;
			report.push(legDescription);

			currentAirport = packageInfo.destination;
		});

		// Return to Origin
		const returnDistance = airportNetwork[currentAirport].connections.get(0).distance;
		const returnTravelTimeMinutes = (returnDistance / airplane.speed) * 60;
		currentTime += returnTravelTimeMinutes;

		let returnLegDescription = `  Return to Origin: ${airportNetwork[currentAirport].formatPathToAirport(0)}`;
		returnLegDescription += `\n    Distance: ${returnDistance} km`;
		returnLegDescription += `\n    Travel Time: ${formatTime(returnTravelTimeMinutes)} (HH:MM)`;
		returnLegDescription += `\n    Arrival Time: ${formatTime(currentTime)} (HH:MM)`;
		report.push(returnLegDescription);
	});

	console.log(report.join('\n'));
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
