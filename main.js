// For testing graphs:https://graphonline.top

const { parseAndLoadInputFiles } = require('./parser.js');
const { buildAirportNetwork } = require('./airport_graph_builder.js');
const { TemooPackage } = require('./package.js');
const { Airplane } = require('./airplane.js');
const { Route, scheduleDeliveries } = require('./pathFinder.js');

const [distanceMatrix, packageData, constraints] = parseAndLoadInputFiles();

const packages = packageData.map((pkg) => new TemooPackage(pkg));

const planes = [];
for (let i = 0; i < constraints.numOfPlanes; i++) {
	planes.push(new Airplane(constraints.speed, constraints.weightCapacity));
}

const airportNetwork = buildAirportNetwork(distanceMatrix);
var bestSolutionInfo = {
	bestTotal: Infinity,
	bestSolution: null,
	totalNumberOfSolutions: 0,
};
const deliveryPlan = scheduleDeliveries(packages, planes, 0, bestSolutionInfo, airportNetwork);
console.log(JSON.stringify(deliveryPlan, null, 2));
