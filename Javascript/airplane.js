const { TemooPackage } = require('./package');

class Airplane {
	static #ORIGIN = 0;
	static #AIRPORTNETWORK = null;

	constructor(planeSpeed, weightCapacity) {
		this.speed = planeSpeed;

		this.totalWeightCapacity = weightCapacity;
		this.returnTimeOrigin = 0;

		this.currentLoad = 0;
		this.totalDistance = 0;

		this.departureTimeOrigin = 0;
		this.packages = [];
	}
	/**
	 * Sets the airport network for all Airplane instances
	 * This should be called once during application initialization and before calling the solver
	 * @param {Object} airportNetwork - The airport network with distances
	 */
	static setAirportNetwork(airportNetwork) {
		Airplane.#AIRPORTNETWORK = airportNetwork;
	}

	/**
	 * Evaluates a route with package insertion, calculating both time feasibility and distance
	 *
	 * @param {Object} packageToAdd - The package to be added
	 * @param {Number} insertPosition - Position in the route where the package would be inserted
	 * @param {Number} departureTimeOrigin - The departure time from origin
	 * @returns {Object} - Object containing { feasible: boolean, totalDistance: number }
	 */
	evaluateRouteWithInsertion(packageToAdd, insertPosition, departureTimeOrigin) {
		const airportNetwork = Airplane.#AIRPORTNETWORK;

		let currentTime = departureTimeOrigin;
		let currentLocation = Airplane.#ORIGIN;
		let totalDistance = 0;

		// Process packages before insertion point
		for (let i = 0; i < insertPosition; i++) {
			const pkg = this.packages[i];
			const travelDistance = airportNetwork[currentLocation].connections.get(
				pkg.destination,
			).distance;

			totalDistance += travelDistance;

			const travelTime = (travelDistance / this.speed) * 60;
			currentTime += travelTime;

			if (currentTime > pkg.deadlineTime) {
				return { feasibility: false, totalDistance: totalDistance };
			}
			currentLocation = pkg.destination;
		}

		const travelDistanceToNewPackage = airportNetwork[currentLocation].connections.get(
			packageToAdd.destination,
		).distance;
		totalDistance += travelDistanceToNewPackage;
		const travelTime = (travelDistanceToNewPackage / this.speed) * 60;
		currentTime += travelTime;
		currentLocation = packageToAdd.destination;

		if (currentTime > packageToAdd.deadlineTime) {
			return { feasibility: false, totalDistance: totalDistance };
		}

		for (let i = insertPosition; i < this.packages.length; i++) {
			const pkg = this.packages[i];
			const travelDistance = airportNetwork[currentLocation].connections.get(
				pkg.destination,
			).distance;

			totalDistance += travelDistance;

			const travelTime = (travelDistance / this.speed) * 60;
			currentTime += travelTime;

			if (currentTime > pkg.deadlineTime) {
				return { feasibility: false, totalDistance: totalDistance };
			}
			currentLocation = pkg.destination;
		}
		// Adding return trip distance
		totalDistance += airportNetwork[currentLocation].connections.get(Airplane.#ORIGIN).distance;

		return { feasibility: true, totalDistance: totalDistance };
	}

	/**
	 * Attempts to add a package to the route at the optimal position
	 *
	 * This method examines all possible insertion positions in the current route,
	 * evaluates each for feasibility and total distance, then selects the optimal position.
	 * If no feasible insertion is found, the package cannot be added to this route.
	 *
	 * @param {Package} packageToAdd - The package to be added to the route
	 * @returns {Number} - The insertion position if successful, -1 if insertion is not feasible
	 */
	tryAddPackage(packageToAdd) {
		let minTotalDistance = Infinity;
		let optimalPosition = 0;

		// Basic weight capacity check
		if (this.currentLoad + packageToAdd.weight > this.totalWeightCapacity) {
			return -1;
		}

		// Can only depart once all the packages have arrived at the depot
		const earliestDepartureTimeFromOrigin = Math.max(
			packageToAdd.arrivalTime,
			this.departureTimeOrigin,
		);

		let insertionPosition = 0;

		// Evaluate all possible insertion positions in the route
		// USing a do loop to avoid having to create extra code for a special case for the first package
		do {
			let routeEvaluation = this.evaluateRouteWithInsertion(
				packageToAdd,
				insertionPosition,
				earliestDepartureTimeFromOrigin,
			);

			// Update if this insertion is feasible and improves minimum total distance
			if (routeEvaluation.feasibility && routeEvaluation.totalDistance < minTotalDistance) {
				minTotalDistance = routeEvaluation.totalDistance;
				optimalPosition = insertionPosition;
			}
			insertionPosition++;
		} while (insertionPosition <= this.packages.length);

		// If we found a feasible position, update route
		if (minTotalDistance !== Infinity) {
			this.totalDistance = minTotalDistance;
			this.packages.splice(optimalPosition, 0, packageToAdd);
			this.currentLoad += packageToAdd.weight;
			this.departureTimeOrigin = earliestDepartureTimeFromOrigin;
			this.returnTimeOrigin = (minTotalDistance / this.speed) * 60 + this.departureTimeOrigin;
			return optimalPosition;
		}

		return -1;
	}

	deepCopy() {
		const newPlane = new Airplane(this.speed, this.totalWeightCapacity);
		newPlane.packages = this.packages.map((pkg) => pkg.deepCopy());

		newPlane.departureTimeOrigin = this.departureTimeOrigin;
		newPlane.returnTimeOrigin = this.returnTimeOrigin;
		newPlane.currentLoad = this.currentLoad;
		newPlane.totalDistance = this.totalDistance;
		return newPlane;
	}
}

module.exports = { Airplane };
