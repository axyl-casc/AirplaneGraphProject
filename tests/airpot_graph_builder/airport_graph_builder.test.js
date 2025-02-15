const {
	shortestDistanceMatrix,
	buildAirportNetwork,
	Airport,
} = require('../../airport_graph_builder.js');

describe('Testing shortestDistanceMatrix function', () => {
	test('should calculate shortest distances for a simple graph 3 x 3 graph with no intermediate Nodes(all shortest paths are direct edges)', () => {
		const distanceMatrix = [
			[0, 1, 1],
			[1, 0, 1],
			[1, 1, 0],
		];

		const expectedPathData = [
			[
				{ distance: 0, via: -1 },
				{ distance: 1, via: -1 },
				{ distance: 1, via: -1 },
			],
			[
				{ distance: 1, via: -1 },
				{ distance: 0, via: -1 },
				{ distance: 1, via: -1 },
			],
			[
				{ distance: 1, via: -1 },
				{ distance: 1, via: -1 },
				{ distance: 0, via: -1 },
			],
		];
		expect(shortestDistanceMatrix(distanceMatrix)).toEqual(expectedPathData);
	});

	test('should handle graph with a single node', () => {
		const distanceMatrix = [[0]];
		const expectedPathData = [[{ distance: 0, via: -1 }]];
		expect(shortestDistanceMatrix(distanceMatrix)).toEqual(expectedPathData);
	});

	test('should handle a disconnected graph , all nodes disconnected', () => {
		const distanceMatrix = [
			[-1, -1, -1],
			[-1, -1, -1],
			[-1, -1, -1],
		];
		const expectedPathData = [
			[
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
			],
			[
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
			],
			[
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
				{ distance: Number.POSITIVE_INFINITY, via: -1 },
			],
		];
		expect(shortestDistanceMatrix(distanceMatrix)).toEqual(expectedPathData);
	});

	test('should handle a 3x3 graph with a path with intermediate nodes', () => {
		const distanceMatrix = [
			[0, -1, 5],
			[2, 0, -1],
			[-1, 1, 0],
		];
		const expectedPathData = [
			[
				{ distance: 0, via: -1 },
				{ distance: 6, via: 2 },
				{ distance: 5, via: -1 },
			],
			[
				{ distance: 2, via: -1 },
				{ distance: 0, via: -1 },
				{ distance: 7, via: 0 },
			],
			[
				{ distance: 3, via: 1 },
				{ distance: 1, via: -1 },
				{ distance: 0, via: -1 },
			],
		];
		expect(shortestDistanceMatrix(distanceMatrix)).toEqual(expectedPathData);
	});
});

describe('Testing buildAirportNetwork function', () => {
	test('should build a simple 3x3 airport network with no intermediate nodes (all shortest paths are direct edges)', () => {
		const distanceMatrix = [
			[0, 1, 1],
			[1, 0, 1],
			[1, 1, 0],
		];

		const expectedAirport0Connections = new Map();
		expectedAirport0Connections.set(0, { path: [0], distance: 0 });
		expectedAirport0Connections.set(1, { path: [0, 1], distance: 1 });
		expectedAirport0Connections.set(2, { path: [0, 2], distance: 1 });

		const expectedAirport1Connections = new Map();
		expectedAirport1Connections.set(0, { path: [1, 0], distance: 1 });
		expectedAirport1Connections.set(1, { path: [1], distance: 0 });
		expectedAirport1Connections.set(2, { path: [1, 2], distance: 1 });

		const expectedAirport2Connections = new Map();
		expectedAirport2Connections.set(0, { path: [2, 0], distance: 1 });
		expectedAirport2Connections.set(1, { path: [2, 1], distance: 1 });
		expectedAirport2Connections.set(2, { path: [2], distance: 0 });
		const resultAirportData = buildAirportNetwork(distanceMatrix);
		expect(resultAirportData[0]).toEqual(new Airport(0, expectedAirport0Connections));
		expect(resultAirportData[1]).toEqual(new Airport(1, expectedAirport1Connections));
		expect(resultAirportData[2]).toEqual(new Airport(2, expectedAirport2Connections));
	});

	test('should handle a single airport network ', () => {
		const distanceMatrix = [[0]];

		const expectedAirport0Connections = new Map();
		expectedAirport0Connections.set(0, { path: [0], distance: 0 });

		const resultAirportData = buildAirportNetwork(distanceMatrix);
		expect(resultAirportData[0]).toEqual(new Airport(0, expectedAirport0Connections));
	});

	test('should build a 3 x 3 airport network with some intermediate nodes', () => {
		const distanceMatrix = [
			[0, 10, -1],
			[10, 0, 20],
			[-1, 20, 0],
		];

		const expectedAirport0Connections = new Map();
		expectedAirport0Connections.set(0, { path: [0], distance: 0 });
		expectedAirport0Connections.set(1, { path: [0, 1], distance: 10 });
		expectedAirport0Connections.set(2, { path: [0, 1, 2], distance: 30 }); // Shortest path 0 -> 1 -> 2 = 10 + 20 = 30

		const expectedAirport1Connections = new Map();
		expectedAirport1Connections.set(0, { path: [1, 0], distance: 10 });
		expectedAirport1Connections.set(1, { path: [1], distance: 0 });
		expectedAirport1Connections.set(2, { path: [1, 2], distance: 20 });

		const expectedAirport2Connections = new Map();
		expectedAirport2Connections.set(0, { path: [2, 1, 0], distance: 30 }); // Shortest path 2 -> 1 -> 0 = 20 + 10 = 30
		expectedAirport2Connections.set(1, { path: [2, 1], distance: 20 });
		expectedAirport2Connections.set(2, { path: [2], distance: 0 });

		const resultAirportData = buildAirportNetwork(distanceMatrix);

		expect(resultAirportData[0]).toEqual(new Airport(0, expectedAirport0Connections));
		expect(resultAirportData[1]).toEqual(new Airport(1, expectedAirport1Connections));
		expect(resultAirportData[2]).toEqual(new Airport(2, expectedAirport2Connections));
	});
});
