const { Airplane } = require('../../airplane.js');
const { TemooPackage } = require('../../package.js');
const { buildAirportNetwork } = require('../../airport_graph_builder.js'); // Assuming you have this

describe('Airplane Unit Tests', () => {
	const testDistances = [
		[0, 60, 100],
		[60, 0, 80],
		[100, 80, 0],
	];
	Airplane.setAirportNetwork(buildAirportNetwork(testDistances));

	describe('Airplane.#evaluateRouteWithInsertion', () => {
		test('should evaluate route insertion as feasible when possible', () => {
			const airplane = new Airplane(50, 400);
			const packageToInsert = new TemooPackage({
				id: 0,
				destination: 1,
				deadlineTime: '10:00', // in minutes
				arrivalTime: '00:00',
				weight: 100,
			});
			const result = airplane.evaluateRouteWithInsertion(packageToInsert, 0, 0);

			expect(result.feasibility).toBe(true);
			expect(result.totalDistance).toBe(120);
		});

		test('should evaluate route insertion as infeasible when deadline would be missed', () => {
			const airplane = new Airplane(50, 400);

			const packageToInsert = new TemooPackage({
				id: 0,
				destination: 1,
				deadlineTime: '00:01',
				arrivalTime: '00:00',
				weight: 100,
			});

			const result = airplane.evaluateRouteWithInsertion(packageToInsert, 0, 0);
			expect(result.feasibility).toBe(false);
		});
	});

	describe('Airplane.tryAddPackage', () => {
		test('should add package if feasible', () => {
			const airplane = new Airplane(50, 400);

			const packageToAdd = new TemooPackage({
				id: 0,
				destination: 1,
				deadlineTime: '10:00',
				arrivalTime: '00:00',
				weight: 100,
			});

			const position = airplane.tryAddPackage(packageToAdd);
			expect(position).toBe(0);
			expect(airplane.packages.length).toBe(1);
			expect(airplane.currentLoad).toBe(100);
		});

		test('should not add package if not feasible (weight)', () => {
			const airplane = new Airplane(50, 50); // Lower weight capacity

			const packageToAdd = new TemooPackage({
				id: 0,
				destination: 1,
				deadlineTime: '10:00',
				arrivalTime: '00:00',
				weight: 100,
			});

			const position = airplane.tryAddPackage(packageToAdd);
			expect(position).toBe(-1);
			expect(airplane.packages.length).toBe(0);
			expect(airplane.currentLoad).toBe(0);
		});

		test('should not add package if deadline cannot be met', () => {
			const airplane = new Airplane(50, 400);

			const packageToAdd = new TemooPackage({
				id: 0,
				destination: 1,
				deadlineTime: '00:01',
				arrivalTime: '00:00',
				weight: 100,
			});

			const position = airplane.tryAddPackage(packageToAdd);
			expect(position).toBe(-1);
			expect(airplane.packages.length).toBe(0);
		});
	});

	describe('Airplane.deepCopy', () => {
		test('should create a deep copy of an Airplane object', () => {
			const airplane = new Airplane(50, 400);

			// Add a package to make the test more meaningful
			const packageToAdd = new TemooPackage({
				id: 1,
				destination: 1,
				deadlineTime: '10:00',
				arrivalTime: '00:00',
				weight: 100,
			});

			airplane.tryAddPackage(packageToAdd);

			const copiedAirplane = airplane.deepCopy();

			expect(copiedAirplane.speed).toBe(airplane.speed);
			expect(copiedAirplane.totalWeightCapacity).toBe(airplane.totalWeightCapacity);
			expect(copiedAirplane.currentLoad).toBe(airplane.currentLoad);
			expect(copiedAirplane.packages.length).toBe(airplane.packages.length);

			// ensuring that they are two different copies. Modifying the original should not change the copy
			airplane.currentLoad += 50;
			expect(copiedAirplane.currentLoad).not.toBe(airplane.currentLoad);
		});
	});
});
