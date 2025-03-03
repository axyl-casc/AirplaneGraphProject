const ORIGIN = 0;
//const { Airplane } = require('./airplane');
const { TemooPackage } = require('./package');
class Route {
	constructor(planeSpeed, weightCapacity) {
		this.packages = [];
		this.departureTimeOrigin = 0;
		this.returnTimeOrigin = 0;
		this.totalWeight = 0;
		this.totalDistance = 0;
		this.totalCapacity = weightCapacity;
		this.planeSpeed = planeSpeed;
	}

	isTimeFeasible(packageToInsert, insertPosition, earliestDepartureTime, airportNetwork) {
		let currentTime = earliestDepartureTime;
		let currentLocation = ORIGIN;

		for (let i = 0; i < insertPosition; i++) {
			const pkg = this.packages[i];
			const travelTime = getTravelTime(
				currentLocation,
				pkg.destination,
				airportNetwork,
				this.planeSpeed,
			);
			currentTime += travelTime;

			if (currentTime > pkg.deadlineTime) {
				return false;
			}

			currentLocation = pkg.destination;
		}

		// Check inserted package
		const travelTimeToNew = getTravelTime(
			currentLocation,
			packageToInsert.destination,
			airportNetwork,
			this.planeSpeed,
		);
		currentTime += travelTimeToNew;

		if (currentTime > packageToInsert.deadlineTime) {
			return false;
		}

		currentLocation = packageToInsert.destination;

		for (let i = insertPosition; i < this.packages.length; i++) {
			const pkg = this.packages[i];
			const travelTime = getTravelTime(
				currentLocation,
				pkg.destination,
				airportNetwork,
				this.planeSpeed,
			);
			currentTime += travelTime;

			if (currentTime > pkg.deadlineTime) {
				return false;
			}

			currentLocation = pkg.destination;
		}
		return true;
	}

	calculateDistanceWithInsertion(packageToInsert, insertPosition, airportNetwork) {
		let totalDistance = 0;
		let currentLocation = ORIGIN;

		// Distance up to insertion
		for (let i = 0; i < insertPosition; i++) {
			const pkg = this.packages[i];
			totalDistance += airportNetwork[currentLocation].connections.get(pkg.destination).distance;
			currentLocation = pkg.destination;
		}
		// Add new package
		totalDistance += airportNetwork[currentLocation].connections.get(
			packageToInsert.destination,
		).distance;
		currentLocation = packageToInsert.destination;

		// Remaining packages
		for (let i = insertPosition; i < this.packages.length; i++) {
			const pkg = this.packages[i];
			totalDistance += airportNetwork[currentLocation].connections.get(pkg.destination).distance;
			currentLocation = pkg.destination;
		}

		// Return to origin
		totalDistance += airportNetwork[currentLocation].connections.get(ORIGIN).distance;

		return totalDistance;
	}

	tryAddPackage(packageToInsert, airportNetwork) {
		let bestDistance = Infinity;
		let bestInsertPosition = -1;
		if (this.totalWeight + packageToInsert.weight > this.totalCapacity) {
			return -1;
		}

		const earliestDepartureTime = Math.max(packageToInsert.arrivalTime, this.departureTimeOrigin);

		// Special case for empty routes
		if (this.packages.length === 0) {
			// Check if package can be delivered on time
			let travelTime = getTravelTime(
				ORIGIN,
				packageToInsert.destination,
				airportNetwork,
				this.planeSpeed,
			);
			let arrivalTime = earliestDepartureTime + travelTime;

			if (arrivalTime > packageToInsert.deadlineTime) {
				return -1;
			}
			bestDistance =
				airportNetwork[ORIGIN].connections.get(packageToInsert.destination).distance * 2;

			bestInsertPosition = 0;
		} else {
			for (let i = 0; i <= this.packages.length; i++) {
				let newDistance = this.calculateDistanceWithInsertion(packageToInsert, i, airportNetwork);
				if (
					newDistance < bestDistance &&
					this.isTimeFeasible(packageToInsert, i, earliestDepartureTime, airportNetwork)
				) {
					bestDistance = newDistance;
					bestInsertPosition = i;
				}
			}
		}

		// If we found a feasible position, update route
		if (bestInsertPosition !== -1) {
			this.totalDistance = bestDistance;
			this.packages.splice(bestInsertPosition, 0, packageToInsert);

			this.totalWeight += packageToInsert.weight;

			this.departureTimeOrigin = earliestDepartureTime;

			this.returnTimeOrigin = (bestDistance / this.planeSpeed) * 60 + this.departureTimeOrigin;
		}

		return bestInsertPosition;
	}

	deepCopy() {
		const newRoute = new Route(this.planeSpeed, this.totalCapacity);

		newRoute.packages = this.packages.map((pkg) => pkg.deepCopy());

		newRoute.departureTimeOrigin = this.departureTimeOrigin;
		newRoute.returnTimeOrigin = this.returnTimeOrigin;
		newRoute.totalWeight = this.totalWeight;
		newRoute.totalDistance = this.totalDistance;

		return newRoute;
	}
}

/**
 * Gets the travel time in minutes from origin to destination using the graph provided.
 *
 * @param {Number} origin - The index of the from
 * @param {Array} destination - Available airplanes
 * @param {Number} currentSolutionTotal - Current solution cost
 * @param {Object} bestSolutionInfo - Object containing bestTotal and bestSolution
 * @param {Object} airportNetwork - Network with travel distances
 * @returns {Object} - Updated bestSolutionInfo
 */
function getTravelTime(origin, destination, graph, speed) {
	const val = graph[origin].connections.get(destination).distance / speed;
	return val * 60;
}

/**
 * Main scheduling function that uses branch and cut to find the optimal package assignment
 * @param {Array} unassignedPackages - Packages that need to be assigned
 * @param {Array} airplanes - Available airplanes
 * @param {Number} currentSolutionTotal - Current solution cost
 * @param {Object} bestSolutionInfo - Object containing bestTotal and bestSolution
 * @param {Object} airportNetwork - Network with travel distances
 * @returns {Object} - Updated bestSolutionInfo
 */
function scheduleDeliveries(
	unassignedPackages,
	airplanes,
	currentSolutionTotal,
	bestSolutionInfo,
	airportNetwork,
) {
	// creating the object if it is accidentally not provided
	if (!bestSolutionInfo) {
		bestSolutionInfo = {
			bestTotal: Infinity,
			bestSolution: null,
			totalNumberOfSolutions: 0,
		};
	}

	// Prune if this branch can't be better than the current best solution
	if (currentSolutionTotal >= bestSolutionInfo.bestTotal) {
		return bestSolutionInfo;
	}

	// Base case - all packages assigned
	if (unassignedPackages.length === 0) {
		bestSolutionInfo.totalNumberOfSolutions++;
		//console.log(JSON.stringify(bestSolutionInfo, null, 2));

		if (currentSolutionTotal < bestSolutionInfo.bestTotal) {
			bestSolutionInfo.bestTotal = currentSolutionTotal;
			bestSolutionInfo.bestSolution = deepCopyArray(airplanes);
		}
		return bestSolutionInfo;
	}

	// Select next package by earliest deadline
	const nextPackage = selectNextPackage(unassignedPackages);

	// New array with the selected package removed
	const remainingPackages = unassignedPackages.filter((pkg) => pkg.id !== nextPackage.id);

	// Try creating a new route for each plane
	for (let i = 0; i < airplanes.length; i++) {
		// Create a deep copy for backtracking
		const planesCopy = deepCopyArray(airplanes);
		const targetPlane = planesCopy[i];

		// Store the previous route distance
		const prevRouteDistance = targetPlane.route.totalDistance;
		// Try adding package to this route
		const insertionResult = targetPlane.route.tryAddPackage(nextPackage, airportNetwork);

		if (insertionResult !== -1) {
			// Calculate new solution total
			const newTotal = currentSolutionTotal - prevRouteDistance + targetPlane.route.totalDistance;

			// Recursive call with updated state
			bestSolutionInfo = scheduleDeliveries(
				remainingPackages,
				planesCopy,
				newTotal,
				bestSolutionInfo,
				airportNetwork,
			);
		}
	}

	return bestSolutionInfo;
}

/**
 * Selects the package with the earliest deadline
 * @param {Array} unassignedPackages - Packages to choose from
 * @returns {Object} - Package with earliest deadline
 */
function selectNextPackage(unassignedPackages) {
	return unassignedPackages.reduce(
		(earliest, current) => (current.deadlineTime < earliest.deadlineTime ? current : earliest),
		unassignedPackages[0],
	);
}

/**
 * Creates deep copies of an array of objects that have deepCopy methods
 * @param {Array} objects - Array of objects to copy
 * @returns {Array} - Array of deep-copied objects
 */
function deepCopyArray(objects) {
	return objects.map((elem) => elem.deepCopy());
}

module.exports = { scheduleDeliveries, Route, getTravelTime, deepCopyArray, selectNextPackage };
