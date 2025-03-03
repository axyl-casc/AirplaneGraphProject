const { buildAirportNetwork } = require('../../airport_graph_builder');
const { scheduleDeliveries } = require('../../pathFinder.js');
const { Airplane } = require('../../airplane');
const { TemooPackage } = require('../../package');

const pack2 = [
	new TemooPackage({
		id: 1,
		weight: 500,
		arrivalTime: '17:59',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 2,
		weight: 500,
		arrivalTime: '21:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 3,
		weight: 500,
		arrivalTime: '21:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 4,
		weight: 500,
		arrivalTime: '21:59',
		deadlineTime: '24:00',
		destination: 5,
	}),
];

const distanceMatrix = [
	[0, 50, -1, -1, -1, -1],
	[50, 0, 50, 30, -1, -1],
	[-1, 50, 0, 60, -1, -1],
	[-1, 30, 60, 0, 60, 20],
	[-1, -1, -1, 60, 0, -1],
	[-1, -1, -1, 20, -1, 0],
];

const planes = [new Airplane(50, 500, 0), new Airplane(50, 500, 1), new Airplane(50, 500, 2)];
const copy = planes.map((e) => e.deepCopy());
const graph = buildAirportNetwork(distanceMatrix);
var bestSolutionInfo = {
	bestTotal: Infinity,
	bestSolution: null,
};
const deliveryPlan = scheduleDeliveries(pack2, planes, 0, bestSolutionInfo, graph);

console.log(JSON.stringify(deliveryPlan, null, 2));
