const { buildAirportNetwork } = require('../../airport_graph_builder');
const { scheduleDeliveries } = require('../../pathFinder.js');
const { Airplane } = require('../../airplane');
const { TemooPackage } = require('../../package');
let nodesExplored = 0;

// 15 packages with similar weights and deadlines but different destinations
const packages = [
	// Each package weighs 200, arrives early, has end-of-day deadline
	new TemooPackage({
		id: 1,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 2,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 2,
	}),
	new TemooPackage({
		id: 3,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 3,
	}),
	new TemooPackage({
		id: 4,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 6,
	}),
	new TemooPackage({
		id: 5,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 6,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 7,
	}),
	new TemooPackage({
		id: 7,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 8,
	}),
	new TemooPackage({
		id: 8,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 1,
	}),
	new TemooPackage({
		id: 9,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 9,
	}),
	new TemooPackage({
		id: 10,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 11,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 2,
	}),
	new TemooPackage({
		id: 12,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 3,
	}),
	new TemooPackage({
		id: 13,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 6,
	}),
	new TemooPackage({
		id: 14,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 15,
		weight: 200,
		arrivalTime: '08:00',
		deadlineTime: '17:00',
		destination: 7,
	}),
];

// 10x10 distance matrix
const distanceMatrix = [
	[0, 60, 75, 90, 100, 110, 120, 130, 140, 150],
	[60, 0, 65, 85, 95, 100, 110, 120, 130, 140],
	[75, 65, 0, 60, 80, 90, 100, 110, 120, 130],
	[90, 85, 60, 0, 70, 80, 90, 100, 110, 120],
	[100, 95, 80, 70, 0, 60, 70, 80, 90, 100],
	[110, 100, 90, 80, 60, 0, 60, 70, 80, 90],
	[120, 110, 100, 90, 70, 60, 0, 60, 70, 80],
	[130, 120, 110, 100, 80, 70, 60, 0, 60, 70],
	[140, 130, 120, 110, 90, 80, 70, 60, 0, 60],
	[150, 140, 130, 120, 100, 90, 80, 70, 60, 0],
];

// Just 3 planes with identical capacity and speed
const planes = [new Airplane(50, 400, 0), new Airplane(50, 400, 1), new Airplane(50, 400, 2)];

const graph = buildAirportNetwork(distanceMatrix);

var bestSolutionInfo = {
	bestTotal: Infinity,
	bestSolution: null,
};

console.time('DeliveryScheduling');

graph.forEach((e) => {
	console.log(JSON.stringify('airportID : ' + e.airportID, null, 2));

	for (let i = 0; i < graph.length; i++) {
		console.log(JSON.stringify(e.connections.get(i), null, 2));
	}
});

const deliveryPlan = scheduleDeliveries(packages, planes, 0, bestSolutionInfo, graph);
console.timeEnd('DeliveryScheduling');

console.log(`Total Packages: ${packages.length}`);
console.log(`Total Distance: ${deliveryPlan.bestTotal}`);
console.log(
	`Routes Generated: ${deliveryPlan.bestSolution.reduce((sum, plane) => sum + plane.routes.length, 0)}`,
);
