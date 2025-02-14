class Airport {
	constructor(airportID, connections) {
		this.airportID = airportID;
		this.connections = connections;
	}
}

/**
 * Calculates the shortest path distances and paths between all pairs of nodes
 * in a graph using the Floyd-Warshall algorithm. It returns an object with the minimum distance as well
 * as the intermediate node used to reach the target node.
 *
 * @param {number[][]} distanceMatrix
 * @returns {object[][]} } matrix where matrix[i][j] has the following properties:
 *        - `distance` : shortest path distance
 *        -  `via`     : The intermediate node used to visit the target node ( -1 if direct edge exists)
 */
function shortestDistanceMatrix(distanceMatrix) {
	const numNodes = distanceMatrix.length;
	const pathData = distanceMatrix.map((row) => {
		return row.map((dist) => {
			return {
				distance: dist === -1 ? Infinity : dist,
				via: -1,
			};
		});
	});

	for (let intermediateNode = 0; intermediateNode < numNodes; intermediateNode++) {
		for (let sourceNode = 0; sourceNode < numNodes; sourceNode++) {
			for (let targetNode = 0; targetNode < numNodes; targetNode++) {
				const distanceViaIntermediateNode =
					pathData[sourceNode][intermediateNode].distance +
					pathData[intermediateNode][targetNode].distance;
				if (pathData[sourceNode][targetNode].distance > distanceViaIntermediateNode) {
					pathData[sourceNode][targetNode].distance = distanceViaIntermediateNode;
					pathData[sourceNode][targetNode].via = intermediateNode;
				}
			}
		}
	}

	return pathData;
}

/**
 * Builds an airport network from a distance matrix using shortest paths.
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
 *                                 to the target airport.ex : for the shortest from a to b: path = [a,c,d....,b]
 *                      			 - `distance`: The shortest path distance to the target airport.
 */
function buildAirportNetwork(distanceMatrix) {
	const numOfAirports = distanceMatrix.length;
	const shortestPaths = shortestDistanceMatrix(distanceMatrix);
	let airports = [];

	for (let i = 0; i < numOfAirports; i++) {
		const connections = new Map();
		for (let j = 0; j < numOfAirports; j++) {
			let path = [i]; // start from the source node
			let intermediateNode = shortestPaths[i][j].via;
			// check if there is a direct connection or if the target node is the source node
			if (i === j || intermediateNode === -1) {
				if (i !== j) {
					path.push(j);
				}
				connections.set(j, { path: path, distance: shortestPaths[i][j].distance });
				continue;
			}
			// Keep adding to the path until a direct edge is found
			while (intermediateNode !== -1) {
				path.push(intermediateNode);
				intermediateNode = shortestPaths[intermediateNode][j].via;
			}
			path.push(j);
			connections.set(j, { path: path, distance: shortestPaths[i][j].distance });
		}
		airports.push(new Airport(i, connections));
	}

	return airports;
}

module.exports = { buildAirportNetwork, Airport, shortestDistanceMatrix };
