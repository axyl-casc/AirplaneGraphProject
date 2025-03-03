class Airport {
	constructor(airportID, connections) {
		this.airportID = airportID;
		this.connections = connections;
	}
}

/**
 * Calculates the shortest path distances and paths between all pairs of nodes
 * in a graph using the Floyd-Warshall algorithm. It returns an object with the minimum distance as well
 * as the path to reach the target node.
 *
 * @param {number[][]} distanceMatrix
 * @returns {object[][]} } matrix where matrix[i][j] has the following properties:
 *        - `distance` : shortest path distance
 *        -  `path`     : An array representing the shortest path to the target node (empty array for the path to node itself)
 */
function shortestDistanceMatrix(distanceMatrix) {
	const numNodes = distanceMatrix.length;
	const pathData = distanceMatrix.map((row) => {
		return row.map((dist) => {
			return {
				distance: dist === -1 ? Number.POSITIVE_INFINITY : dist,
				path: [], // Change to array to store complete path
			};
		});
	});

	// Initialize direct connections
	for (let i = 0; i < numNodes; i++) {
		for (let j = 0; j < numNodes; j++) {
			if (distanceMatrix[i][j] !== -1 && i !== j) {
				pathData[i][j].path = [j]; // Direct connection
			}
		}
	}

	for (let k = 0; k < numNodes; k++) {
		for (let i = 0; i < numNodes; i++) {
			for (let j = 0; j < numNodes; j++) {
				const distanceViaK = pathData[i][k].distance + pathData[k][j].distance;

				if (pathData[i][j].distance > distanceViaK) {
					pathData[i][j].distance = distanceViaK;
					pathData[i][j].path = [...pathData[i][k].path, ...pathData[k][j].path];
				}
			}
		}
	}

	return pathData;
}

/**
 * Builds an airport network from a distance matrix using shortest paths helper funcitons.
 *
 * This function takes a distance matrix representing distances between airports and
 * constructs an array of `Airport` objects. Each `Airport` object contains information
 * about its connections to other airports, including the shortest path and distance.
 *
 * @param {number[][]} distanceMatrix - A square matrix representing the distances between airports.
 *                                      - `distanceMatrix[i][j]` is the direct distance from airport `i` to airport `j`.
 *                                      - Use `-1` to represent no direct connection between airports
 * @returns {Airport[]} An array of `Airport` objects. Each `Airport` object has the following properties:
 *                     - `id`: The index of the airport in the network (starting from 0).
 *                     - `connections`: A Map where keys are the IDs of connected airports and values are objects
 *                                      containing path information:
 *                       			- `path`: An array of airport IDs representing the shortest path from the source airport
 *                                 to the target airport.ex : for the shortest from a to b: path = [c,d....,b]
 *                      			 - `distance`: The shortest path distance to the target airport.
 */
function buildAirportNetwork(distanceMatrix) {
	const numOfAirports = distanceMatrix.length;
	const pathData = shortestDistanceMatrix(distanceMatrix);
	const airports = [];
	for (let i = 0; i < numOfAirports; i++) {
		const connections = new Map();
		for (let j = 0; j < numOfAirports; j++) {
			connections.set(j, pathData[i][j]);
		}
		airports.push(new Airport(i, connections));
	}
	return airports;
}

module.exports = { buildAirportNetwork, Airport, shortestDistanceMatrix };
