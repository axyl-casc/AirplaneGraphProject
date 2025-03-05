const { TemooPackage } = require('./package');
const { Airplane } = require('./airplane');
/**
 * Main scheduling function that uses branch and cut to find the optimal package assignment
 * @param {Array} unassignedPackages - Packages that need to be assigned
 * @param {Array} airplanes - Available airplanes
 * @param {Number} currentSolutionTotal - Current solution cost
 * @param {Object} bestSolutionInfo - Object containing bestTotal and bestSolution
 * @param {Object} airportNetwork - Network with travel distances
 * @returns {Object} - Updated bestSolutionInfo
 */
function scheduleDeliveries(unassignedPackages, airplanes, currentSolutionTotal, bestSolutionInfo) {
	// Creating the object if it is accidentally not provided
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

	// Try add the package to each route in each plane
	for (let i = 0; i < airplanes.length; i++) {
		// Create a deep copy for backtracking
		const planesCopy = deepCopyArray(airplanes);
		const targetPlane = planesCopy[i];

		// Store the previous route distance
		const prevRouteDistance = targetPlane.totalDistance;

		// Try adding package to this route
		const insertionResult = targetPlane.tryAddPackage(nextPackage);

		if (insertionResult !== -1) {
			// Calculate new solution total
			const newTotal = currentSolutionTotal - prevRouteDistance + targetPlane.totalDistance;
			// Recursive call with updated state
			bestSolutionInfo = scheduleDeliveries(
				remainingPackages,
				planesCopy,
				newTotal,
				bestSolutionInfo,
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

module.exports = { scheduleDeliveries, deepCopyArray, selectNextPackage };
