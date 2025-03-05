// For testing graphs:https://graphonline.top

const { parseAndLoadInputFiles } = require('./parser.js');
const { buildAirportNetwork } = require('./airport_graph_builder.js');
const { TemooPackage } = require('./package.js');
const { Airplane } = require('./airplane.js');
const { scheduleDeliveries } = require('./pathFinder.js');

//Load the input Files
const [distanceMatrix, packageData, constraints] = parseAndLoadInputFiles();

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
let bestSolutionInfo = {
	bestTotal: Infinity,
	bestSolution: null,
	totalNumberOfSolutions: 0,
};

// Find the solution
const deliveryPlan = scheduleDeliveries(packages, planes, 0, bestSolutionInfo);
printSolution(deliveryPlan, airportNetwork);

/**
 * Prints a detailed report of the optimal delivery solution.
 *
 * @param {object} bestSolutionInfo - An object containing the best solution and related statistics.
 * @param {object} airportNetwork - An object representing the airport network with connections.
 */
function printSolution(solution, airportNetwork) {
	// If a solution was not found
	if (!bestSolutionInfo.bestSolution) {
		console.log('No feasible delivery solution found.');
		return;
	}

	const deliveryPlan = bestSolutionInfo.bestSolution;
	const reportLines = [];

	// --- Header and Overall Statistics ---
	reportLines.push('=== DELIVERY SOLUTION REPORT ===');
	reportLines.push(`Total Distance: ${bestSolutionInfo.bestTotal} km`);
	reportLines.push(`Total Solutions Evaluated: ${bestSolutionInfo.totalNumberOfSolutions}`);
	reportLines.push(`Number of Airplanes Used: ${deliveryPlan.length}`);
	reportLines.push(`Airplane Capacity: ${deliveryPlan[0].totalWeightCapacity} kg`);
	reportLines.push(`Airplane Speed: ${deliveryPlan[0].speed} km/h`);
	reportLines.push('');

	// --- Count Total Packages Delivered ---
	const totalDeliveredPackages = deliveryPlan.reduce(
		(sum, airplane) => sum + airplane.packages.length,
		0,
	);

	reportLines.push(`Total Packages Delivered: ${totalDeliveredPackages}`);
	reportLines.push('');

	// --- Each Airplane-Specific Details ---
	deliveryPlan.forEach((airplane, index) => {
		reportLines.push(`--- AIRPLANE ${index + 1} ---`);
		reportLines.push(
			`Load: ${airplane.currentLoad} kg (${((airplane.currentLoad / airplane.totalWeightCapacity) * 100).toFixed(1)}%)`,
		);
		reportLines.push(`Total Distance Traveled: ${airplane.totalDistance} km`);
		reportLines.push(`Departure Time: ${formatTime(airplane.departureTimeOrigin)} (HH:MM)`);
		reportLines.push(`Return Time: ${formatTime(airplane.returnTimeOrigin)} (HH:MM)`);
		reportLines.push('');

		reportLines.push('DETAILED ROUTE:');

		// Route Details
		let currentAirport = 0; // Origin
		let currentTime = airplane.departureTimeOrigin;

		airplane.packages.forEach((packageInfo, packageIndex) => {
			const connection = airportNetwork[currentAirport].connections.get(packageInfo.destination);
			const legDistance = connection.distance;
			const legTravelTimeMinutes = (legDistance / airplane.speed) * 60;
			currentTime += legTravelTimeMinutes;

			const legDescription = `  Leg ${packageIndex + 1}: ${airportNetwork[currentAirport].formatPathToAirport(packageInfo.destination)}`;
			legDescription += `\n    Distance: ${legDistance} km`;
			legDescription += `\n    Travel Time: ${formatTime(legTravelTimeMinutes)} (HH:MM)`;
			legDescription += `\n    Arrival Time: ${formatTime(currentTime)} (HH:MM)`;
			legDescription += `\n    Delivering: Package ${packageInfo.id} (${packageInfo.weight} kg)`;
			legDescription += `\n    Deadline: ${formatTime(packageInfo.deadlineTime)} (HH:MM)`;
			reportLines.push(legDescription);

			currentAirport = packageInfo.destination;
		});

		// --- Return to Origin ---
		const returnDistance = airportNetwork[currentAirport].connections.get(0).distance;
		const returnTravelTimeMinutes = (returnDistance / airplane.speed) * 60;
		currentTime += returnTravelTimeMinutes;

		const returnLegDescription = `  Return to Origin: ${airportNetwork[currentAirport].formatPathToAirport(0)}`;
		returnLegDescription += `\n    Distance: ${returnDistance} km`;
		returnLegDescription += `\n    Travel Time: ${formatTime(returnTravelTimeMinutes)} (HH:MM)`;
		returnLegDescription += `\n    Arrival Time: ${formatTime(currentTime)} (HH:MM)`;
		reportLines.push(returnLegDescription);
	});

	// --- Output the Report ---
	console.log(reportLines.join('\n'));
}

/**
 * Formats and prints the solution in a readable format
 * @param {Object} bestSolutionInfo - Object containing the best solution found
 * @param {Object} airportNetwork - Graph network of airports
 *  */
function printSolution(bestSolutionInfo, airportNetwork) {
	if (!bestSolutionInfo.bestSolution) {
		return 'No solution found.';
	}

	const solution = bestSolutionInfo.bestSolution;
	let output = [];

	// Print header and overall statistics
	output.push('=== DELIVERY SOLUTION ===');
	output.push(`Total Distance: ${bestSolutionInfo.bestTotal} km`);
	output.push(`Total Solutions Evaluated: ${bestSolutionInfo.totalNumberOfSolutions}`);

	output.push(`Number of Airplanes Used: ${solution.length}`);
	output.push(`Each Airplane's Weight Capacity: ${solution[0].totalWeightCapacity}`);
	output.push(`Each Airplane's speed: ${solution[0].speed}`);

	output.push('');

	// Total Packages Delivered
	let totalPackages = 0;
	solution.forEach((airplane) => {
		totalPackages += airplane.packages.length;
	});
	output.push(`Total Packages Delivered: ${totalPackages}`);
	output.push('');

	// Details for each airplane
	solution.forEach((airplane, index) => {
		output.push(`--- AIRPLANE ${index} ---`);
		output.push(
			`Load: ${airplane.currentLoad} kg (${((airplane.totalWeightCapacity / airplane.totalWeightCapacity) * 100).toFixed(1)}%)`,
		);
		output.push(`Total Distance Traveled: ${airplane.totalDistance}`);
		output.push(`Departure Time From Origin: ${formatTime(airplane.departureTimeOrigin)} (HH:MM)`);
		output.push(`Return Time To Origin: ${formatTime(airplane.returnTimeOrigin)} (HH:MM)`);

		// Print detailed route with distances
		output.push('');
		output.push('DETAILED ROUTE:');

		// Initialize current location and time
		let currentLocation = 0; // Origin
		let currentTime = airplane.departureTimeOrigin;

		airplane.packages.forEach((pkg, index) => {
			const connection = airportNetwork[currentLocation].connections.get(pkg.destination);
			const distance = connection.distance;
			const travelTime = (distance / airplane.speed) * 60; //  in minutes
			currentTime += travelTime;

			let legDescription = `  Leg ${index + 1}: ${airportNetwork[currentLocation].formatPathToAirport(pkg.destination)}`;

			legDescription += `\n    Distance: ${distance} km`;
			legDescription += `\n    Travel Time: ${formatTime(travelTime)} (hh:mm)`;
			legDescription += `\n    Arrival Time: ${formatTime(currentTime)} (hh:mm)`;
			legDescription += `\n    Delivering: Package ${pkg.id} (${pkg.weight} kg)`;
			legDescription += `\n    Deadline: ${formatTime(pkg.deadlineTime)}`;
			output.push(legDescription);

			currentLocation = pkg.destination;
		});

		const returnToORiginDist = airportNetwork[currentLocation].connections.get(0).distance;
		const travelTime = (returnToORiginDist / airplane.speed) * 60;
		let returnToORiginLeg = `  Return to Origin Leg : ${airportNetwork[currentLocation].formatPathToAirport(0)}`;
		currentTime += travelTime;
		returnToORiginLeg += `\n    Distance: ${returnToORiginDist} km`;
		returnToORiginLeg += `\n    Travel Time: ${formatTime(travelTime)} (hh:mm)`;
		returnToORiginLeg += `\n    Arrival Time: ${formatTime(currentTime)} (hh:mm)`;
		output.push(returnToORiginLeg);
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
