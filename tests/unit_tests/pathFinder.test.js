const { Route, getTravelTime, selectNextPackage, deepCopyArray } = require('../../pathFinder.js'); // Adjust path as needed
const { Airplane } = require('../../airplane.js');
const { TemooPackage } = require('../../package.js');
const { buildAirportNetwork } = require('../../airport_graph_builder.js'); // Assuming you have this

describe('pathFinder Unit Tests', () => {
	//Helper Functions for Test Setup
	function createTestAirportNetwork(distances) {
		return buildAirportNetwork(distances);
	}
	function createTestPackages(packageData) {
		return packageData.map((data) => new TemooPackage(data));
	}

	describe('getTravelTime', () => {
		test('should calculate travel time correctly', () => {
			const distances = [
				[0, 60],
				[60, 0],
			];

			const airportNetwork = createTestAirportNetwork(distances);
			const speed = 60;
			const origin = 0;
			const destination = 1;
			const expectedTime = 60;

			expect(getTravelTime(origin, destination, airportNetwork, speed)).toBeCloseTo(
				expectedTime,
				6,
			);
		});
	});

	describe('Route.isTimeFeasible', () => {
		test('should return true when insertion in an existing route is possible', () => {
			const route = new Route(50, 400, 0);
			const airportNetwork = createTestAirportNetwork([
				[0, 60],
				[60, 0],
			]);

			const packageToInsert = new TemooPackage({
				id: 1,
				destination: 1,
				deadlineTime: '10:00',
				arrivalTime: '00:00',
				weight: 100,
			});

			const insertPosition = 0;
			const earliestDepartureTime = 0;
			expect(
				route.isTimeFeasible(
					packageToInsert,
					insertPosition,
					earliestDepartureTime,
					airportNetwork,
				),
			).toBe(true);
		});

		test('should return false when insertion is not possible as the deadline will be missed', () => {
			const route = new Route(50, 400, 0);
			const airportNetwork = createTestAirportNetwork([
				[0, 60],
				[60, 0],
			]);
			const packageToInsert = new TemooPackage({
				id: 1,
				destination: 1,
				deadlineTime: '00:30',
				arrivalTime: '00:00',
				weight: 100,
			});
			const insertPosition = 0;
			const earliestDepartureTime = 0;
			expect(
				route.isTimeFeasible(
					packageToInsert,
					insertPosition,
					earliestDepartureTime,
					airportNetwork,
				),
			).toBe(false);
		});
	});

	describe('Route.calculateDistanceWithInsertion', () => {
		test('should calculate distance correctly after insertion', () => {
			const route = new Route(50, 400, 0);

			const airportNetwork = createTestAirportNetwork([
				[0, 10, 20],
				[10, 0, 15],
				[20, 15, 0],
			]);

			const packageToInsert = new TemooPackage({
				id: 1,
				destination: 1,
				deadlineTime: '10:00',
				arrivalTime: '00:00',
				weight: 100,
			});

			const insertPosition = 0;
			expect(
				route.calculateDistanceWithInsertion(packageToInsert, insertPosition, airportNetwork),
			).toBe(airportNetwork[0].connections.get(1).distance * 2);
		});
	});

	describe('Route.prototype.tryAddPackage', () => {
		test('should add package if feasible', () => {
			const route = new Route(50, 400, 0);
			const airportNetwork = createTestAirportNetwork([
				[0, 60],
				[60, 0],
			]);
			const packageToAdd = new TemooPackage({
				id: 1,
				destination: 1,
				deadlineTime: '10:00',
				arrivalTime: '00:00',
				weight: 100,
			});
			expect(route.tryAddPackage(packageToAdd, airportNetwork)).toBeGreaterThanOrEqual(0);
			expect(route.packages.length).toBe(1); // // checking if packages was also  modified
		});

		test('should not add package if not feasible (weight)', () => {
			const route = new Route(50, 50, 0);
			const airportNetwork = createTestAirportNetwork([
				[0, 60],
				[60, 0],
			]);
			const packageToAdd = new TemooPackage({
				id: 1,
				destination: 1,
				deadlineTime: '10:00',
				arrivalTime: '00:00',
				weight: 100,
			});
			expect(route.tryAddPackage(packageToAdd, airportNetwork)).toBe(-1);
			expect(route.packages.length).toBe(0); // checking if packages was also not modified
		});
	});

	describe('Route.deepCopy', () => {
		test('should deep copy Route object', () => {
			const route = new Route(50, 400, 0);
			const copiedRoute = route.deepCopy();
			expect(copiedRoute).toEqual(route);
		});
	});

	describe('Airplane.deepCopy', () => {
		test('should create a deep copy of an Airplane object', () => {
			const airplane = new Airplane(50, 400);
			const copiedAirplane = airplane.deepCopy();
			expect(copiedAirplane).toEqual(airplane);
		});
	});

	describe('deepCopyArray', () => {
		test('should create a deep copy of an array of Routes', () => {
			const routes = [new Route(50, 400, 0), new Route(50, 400, 1)];
			const copiedRoutes = deepCopyArray(routes);
			expect(copiedRoutes).toEqual(routes.map((r) => r.deepCopy()));
		});
	});

	describe('selectNextPackage', () => {
		test('should select package with earliest deadline', () => {
			const packages = createTestPackages([
				{ id: 1, destination: 1, deadlineTime: '11:00', arrivalTime: '00:00', weight: 100 },
				{ id: 2, destination: 1, deadlineTime: '10:00', arrivalTime: '00:00', weight: 100 }, // Earliest deadline
			]);
			const earliestPackage = selectNextPackage(packages);
			expect(earliestPackage.id).toBe(2);
		});
	});
});
