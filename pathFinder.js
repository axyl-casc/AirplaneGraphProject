const TIME_LIMIT = 24 * 60; // 24 hours in minutes
const ORIGIN = 0;

class Route {
	constructor(plane) {
		this.plane = plane;
		this.packages = [];
		this.departureTimeOrigin = 0;
		this.arrivalTimesAtDestinations = [];
		this.returnTimeOrigin = 0;
		this.totalWeight = 0;
		this.lastDestination = null;
	}

	addPackage(packageToAdd, airportNetwork) {
		if (packageToAdd.weight + this.totalWeight > this.plane.weightCapacity) {
			return false;
		}
		const newPackages = [...this.packages, packageToAdd];

		newPackages.sort((a, b) => a.arrivalTime - b.arrivalTime);
		const earliestDepartureTime = Math.max(
			this.plane.availableTime,
			newPackages[newPackages.length - 1].arrivalTime,
		);

		const newArrivalTimesAtDestinations = [];

		let currentTime = earliestDepartureTime;

		let currLocation = 0;
		for (const pkg of newPackages) {
			const travelTime = getTravelTime(
				currLocation,
				pkg.destination,
				airportNetwork,
				this.plane.speed,
			);
			if (currentTime > pkg.deadlineTime) {
				return false;
			}
			currentTime += travelTime;
			currLocation = pkg.destination;
			newArrivalTimesAtDestinations.push(currentTime);
		}

		this.returnTimeOrigin =
			currentTime + getTravelTime(currLocation, 0, airportNetwork, this.plane.speed);
		this.plane.availableTime = this.returnTimeOrigin;

		this.packages = newPackages;
		this.lastDestination = currLocation;
		this.totalWeight += packageToAdd.weight;
		this.departureTimeOrigin = earliestDepartureTime;
		this.arrivalTimesAtDestinations = newArrivalTimesAtDestinations;
		return true;
	}

	static RouteCreationPossible(packageToInsert, plane, airportNetwork) {
		// Weight Capacity Check
		if (plane.weightCapacity < packageToInsert.weight) {
			return false; // Over capacity
		}
		const earliestDepartureTime = Math.max(plane.availableTime, packageToInsert.arrivalTime);
		const arrivalTime =
			earliestDepartureTime +
			getTravelTime(ORIGIN, packageToInsert.destination, airportNetwork, plane.speed);

		//process.exit(-1);
		if (arrivalTime > packageToInsert.deadlineTime) {
			return false;
		}

		return true;
	}

	clone() {
		const clonedRoute = new Route(this.plane);

		clonedRoute.packages = [...this.packages];
		clonedRoute.arrivalTimesAtDestinations = [...this.arrivalTimesAtDestinations];

		clonedRoute.departureTimeOrigin = this.departureTimeOrigin;
		clonedRoute.returnTimeOrigin = this.returnTimeOrigin;
		clonedRoute.totalWeight = this.totalWeight;
		clonedRoute.lastDestination = this.lastDestination;

		return clonedRoute;
	}
}

function getTravelTime(origin, destination, graph, speed) {
	const val = graph[origin].connections.get(destination).distance / speed;
	return val * 60;
}

function scheduleDeliveries(unprocessedPackages, planes, currentRoutes, airportNetwork) {
	if (unprocessedPackages.length === 0) {
		return currentRoutes; // Base case: All packages are scheduled
	}

	// Select the first unprocessed package
	const currentPackage = unprocessedPackages[0];

	// Try to insert into an existing route
	for (let routeIndex = 0; routeIndex < currentRoutes.length; routeIndex++) {
		const route = currentRoutes[routeIndex];
		const previousAvailableTime = route.plane.availableTime; // Save original available time for backtracking

		const clonedRoute = route.clone(); // Clone to avoid modifying the original route
		if (clonedRoute.addPackage(currentPackage, airportNetwork)) {
			const newRoutes = [...currentRoutes];
			newRoutes[routeIndex] = clonedRoute; // Replace modified route instead of appending

			const remainingPackages = unprocessedPackages.slice(1);
			const solution = scheduleDeliveries(remainingPackages, planes, newRoutes, airportNetwork);
			if (solution) {
				return solution;
			}

			//Restore plane's available time if solution fails
			route.plane.availableTime = previousAvailableTime;
		}
	}

	// Try to start a new route with this package
	for (let planeIndex = 0; planeIndex < planes.length; planeIndex++) {
		const plane = planes[planeIndex];

		const previousAvailableTime = plane.availableTime; // Save plane's available time for backtracking
		if (Route.RouteCreationPossible(currentPackage, plane, airportNetwork)) {
			const newRoute = new Route(plane);

			newRoute.addPackage(currentPackage, airportNetwork);

			const remainingPackages = unprocessedPackages.slice(1);
			const nextRoutes = [...currentRoutes, newRoute]; // Add new route
			plane.availableTime = newRoute.returnTimeOrigin;

			const solution = scheduleDeliveries(remainingPackages, planes, nextRoutes, airportNetwork);

			if (solution) {
				return solution;
			}

			// Backtrack: Restore plane's available time if solution fails
			plane.availableTime = previousAvailableTime;
		}
	}

	return null; // No valid schedule found, backtrack
}

module.exports = { scheduleDeliveries };
