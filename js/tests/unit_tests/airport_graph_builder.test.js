// References : GOt the (5 x 5) graph test case from gemini

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
				{ distance: 0, path: [] },
				{ distance: 1, path: [1] },
				{ distance: 1, path: [2] },
			],
			[
				{ distance: 1, path: [0] },
				{ distance: 0, path: [] },
				{ distance: 1, path: [2] },
			],
			[
				{ distance: 1, path: [0] },
				{ distance: 1, path: [1] },
				{ distance: 0, path: [] },
			],
		];
		expect(shortestDistanceMatrix(distanceMatrix)).toEqual(expectedPathData);
	});

	test('should handle graph with a single node', () => {
		const distanceMatrix = [[0]];
		const expectedPathData = [[{ distance: 0, path: [] }]];
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
				{ distance: Number.POSITIVE_INFINITY, path: [] },
				{ distance: Number.POSITIVE_INFINITY, path: [] },
				{ distance: Number.POSITIVE_INFINITY, path: [] },
			],
			[
				{ distance: Number.POSITIVE_INFINITY, path: [] },
				{ distance: Number.POSITIVE_INFINITY, path: [] },
				{ distance: Number.POSITIVE_INFINITY, path: [] },
			],
			[
				{ distance: Number.POSITIVE_INFINITY, path: [] },
				{ distance: Number.POSITIVE_INFINITY, path: [] },
				{ distance: Number.POSITIVE_INFINITY, path: [] },
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
				{ distance: 0, path: [] },
				{ distance: 6, path: [2, 1] },
				{ distance: 5, path: [2] },
			],
			[
				{ distance: 2, path: [0] },
				{ distance: 0, path: [] },
				{ distance: 7, path: [0, 2] },
			],
			[
				{ distance: 3, path: [1, 0] },
				{ distance: 1, path: [1] },
				{ distance: 0, path: [] },
			],
		];
		expect(shortestDistanceMatrix(distanceMatrix)).toEqual(expectedPathData);
	});

	test('should handle a larger graph (5x5) with various path lengths', () => {
		const distanceMatrix = [
			[0, 3, -1, -1, 2],
			[-1, 0, 5, 1, -1],
			[4, -1, 0, 2, -1],
			[-1, -1, -1, 0, 6],
			[-1, -1, 3, -1, 0],
		];
		const expectedPathData = [
			[
				{ distance: 0, path: [] },
				{ distance: 3, path: [1] },
				{ distance: 5, path: [4, 2] },
				{ distance: 4, path: [1, 3] },
				{ distance: 2, path: [4] },
			],
			[
				{ distance: 9, path: [2, 0] },
				{ distance: 0, path: [] },
				{ distance: 5, path: [2] },
				{ distance: 1, path: [3] },
				{ distance: 7, path: [3, 4] },
			],
			[
				{ distance: 4, path: [0] },
				{ distance: 7, path: [0, 1] },
				{ distance: 0, path: [] },
				{ distance: 2, path: [3] },
				{ distance: 6, path: [0, 4] },
			],
			[
				{ distance: 13, path: [4, 2, 0] },
				{ distance: 16, path: [4, 2, 0, 1] },
				{ distance: 9, path: [4, 2] },
				{ distance: 0, path: [] },
				{ distance: 6, path: [4] },
			],
			[
				{ distance: 7, path: [2, 0] },
				{ distance: 10, path: [2, 0, 1] },
				{ distance: 3, path: [2] },
				{ distance: 5, path: [2, 3] },
				{ distance: 0, path: [] },
			],
		];
		expect(shortestDistanceMatrix(distanceMatrix)).toEqual(expectedPathData);
	});
});
