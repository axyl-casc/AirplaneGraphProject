const { buildAirportNetwork, Airport } = require('../../airport_graph_builder');
const { scheduleDeliveries } = require('../../pathFinder');
const { Airplane } = require('../../airplane');
const { TemooPackage } = require('../../package'); // ✅ Corrected class name

const TIME_LIMIT = 24 * 60; // 24 hours in minutes

const distanceMatrix = [
	[0, 50, -1, -1, -1, -1],
	[50, 0, 50, 30, -1, -1],
	[-1, 50, 0, 60, -1, -1],
	[-1, 30, 60, 0, 60, 20],
	[-1, -1, -1, 50, 0, -1],
	[-1, -1, -1, 20, -1, 0],
];

const packages = [
	new TemooPackage({
		id: 1,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 2,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 3,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 4,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 5,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 6,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 7,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 8,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 9,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 10,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 11,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 12,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
	new TemooPackage({
		id: 13,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 14,
		weight: 500,
		arrivalTime: '00:00',
		deadlineTime: '24:00',
		destination: 5,
	}),
];

// Initialize airplane objects
const planes = [new Airplane(50, 500, 0), new Airplane(50, 500, 1), new Airplane(50, 500, 2)];

// Build airport graph
const graph = buildAirportNetwork(distanceMatrix);
console.log(JSON.stringify(graph[0].connections, null, 2));
var bestSolutionInfo = {
	bestTotal: Infinity,
	bestSolution: null,
};
const deliveryPlan = scheduleDeliveries(packages, planes, 0, bestSolutionInfo, graph);
console.log(JSON.stringify(deliveryPlan, null, 2));
process.exit(-1);
// Run the delivery scheduler for 10 rounds, incrementing current_time by 30 minutes each time
let current_time = 0;
for (let round = 1; round <= 10; round++) {
	console.log(`\n🔹 **Round ${round}: Current Time = ${current_time} minutes**`);

	const deliveryPlan = scheduleDeliveries(graph, packages, current_time, planes[0]); // ✅ Passes a single plane

	console.log(JSON.stringify(deliveryPlan, null, 2));

	// Increment current_time by 30 minutes for next round
	current_time += 30;
}
