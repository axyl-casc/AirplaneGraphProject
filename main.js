// For testing graphs:https://graphonline.top

const { parseAndLoadInputFiles } = require('./parser.js');
const { buildAirportNetwork, Airport } = require('./airport_graph_builder.js');

//const [distanceMatrix, packageData, constraints] = parseAndLoadInputFiles();
const graph = [
	[0, 2, -1, 5],
	[2, 0, 3, -1],
	[-1, 3, 0, 2],
	[5, -1, 2, 0],
];

const airport = buildAirportNetwork(graph);
