const { buildAirportNetwork } = require('../airport_graph_builder');
const { scheduleDeliveries } = require('../pathFinder.js');
const { Airplane } = require('../airplane');
const { TemooPackage } = require('../package');

// 25 packages with varied properties
const packages = [
	new TemooPackage({
		id: 1,
		weight: 220,
		arrivalTime: '05:30',
		deadlineTime: '09:45',
		destination: 5,
	}),
	new TemooPackage({
		id: 2,
		weight: 180,
		arrivalTime: '05:45',
		deadlineTime: '10:15',
		destination: 12,
	}),
	new TemooPackage({
		id: 3,
		weight: 310,
		arrivalTime: '06:00',
		deadlineTime: '11:30',
		destination: 8,
	}),
	new TemooPackage({
		id: 4,
		weight: 275,
		arrivalTime: '06:15',
		deadlineTime: '10:45',
		destination: 3,
	}),
	new TemooPackage({
		id: 5,
		weight: 195,
		arrivalTime: '06:30',
		deadlineTime: '12:00',
		destination: 7,
	}),

	new TemooPackage({
		id: 6,
		weight: 340,
		arrivalTime: '07:45',
		deadlineTime: '14:30',
		destination: 10,
	}),
	new TemooPackage({
		id: 7,
		weight: 290,
		arrivalTime: '08:15',
		deadlineTime: '13:45',
		destination: 14,
	}),
	new TemooPackage({
		id: 8,
		weight: 210,
		arrivalTime: '08:30',
		deadlineTime: '15:00',
		destination: 2,
	}),
	new TemooPackage({
		id: 9,
		weight: 360,
		arrivalTime: '09:00',
		deadlineTime: '14:15',
		destination: 6,
	}),
	new TemooPackage({
		id: 10,
		weight: 255,
		arrivalTime: '09:30',
		deadlineTime: '16:30',
		destination: 11,
	}),

	// Midday deliveries
	new TemooPackage({
		id: 11,
		weight: 320,
		arrivalTime: '10:15',
		deadlineTime: '17:00',
		destination: 4,
	}),
	new TemooPackage({
		id: 12,
		weight: 185,
		arrivalTime: '11:00',
		deadlineTime: '16:45',
		destination: 9,
	}),
	new TemooPackage({
		id: 13,
		weight: 270,
		arrivalTime: '11:30',
		deadlineTime: '18:15',
		destination: 13,
	}),
	new TemooPackage({
		id: 14,
		weight: 390,
		arrivalTime: '12:00',
		deadlineTime: '19:30',
		destination: 1,
	}),
	new TemooPackage({
		id: 15,
		weight: 230,
		arrivalTime: '12:30',
		deadlineTime: '20:00',
		destination: 5,
	}),

	// Afternoon deliveries
	new TemooPackage({
		id: 16,
		weight: 305,
		arrivalTime: '13:45',
		deadlineTime: '19:15',
		destination: 12,
	}),
	new TemooPackage({
		id: 17,
		weight: 245,
		arrivalTime: '14:15',
		deadlineTime: '21:30',
		destination: 8,
	}),
	new TemooPackage({
		id: 18,
		weight: 370,
		arrivalTime: '15:00',
		deadlineTime: '22:00',
		destination: 3,
	}),
	new TemooPackage({
		id: 19,
		weight: 215,
		arrivalTime: '15:30',
		deadlineTime: '21:45',
		destination: 7,
	}),
	new TemooPackage({
		id: 20,
		weight: 285,
		arrivalTime: '16:00',
		deadlineTime: '23:00',
		destination: 10,
	}),

	// Evening deliveries
	new TemooPackage({
		id: 21,
		weight: 325,
		arrivalTime: '17:15',
		deadlineTime: '23:30',
		destination: 14,
	}),
	new TemooPackage({
		id: 22,
		weight: 260,
		arrivalTime: '18:00',
		deadlineTime: '23:45',
		destination: 2,
	}),
	new TemooPackage({
		id: 23,
		weight: 350,
		arrivalTime: '18:30',
		deadlineTime: '23:59',
		destination: 6,
	}),
	new TemooPackage({
		id: 24,
		weight: 200,
		arrivalTime: '19:15',
		deadlineTime: '23:59',
		destination: 11,
	}),
	new TemooPackage({
		id: 25,
		weight: 330,
		arrivalTime: '20:00',
		deadlineTime: '23:59',
		destination: 4,
	}),
];

// 15x15 distance matrix with complex routing options
// This is a large, connected graph with various path options
const distanceMatrix = [
	// 0    1    2    3    4    5    6    7    8    9    10   11   12   13   14
	[0, 75, -1, -1, -1, 110, -1, -1, -1, 90, -1, -1, -1, -1, -1], // 0
	[75, 0, 55, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 120, -1], // 1
	[-1, 55, 0, 40, -1, -1, -1, -1, 85, -1, -1, -1, -1, -1, -1], // 2
	[-1, -1, 40, 0, 60, -1, 70, -1, -1, -1, -1, -1, -1, -1, -1], // 3
	[-1, -1, -1, 60, 0, 50, -1, 80, -1, -1, -1, -1, -1, -1, -1], // 4
	[110, -1, -1, -1, 50, 0, -1, -1, -1, 65, 90, -1, -1, -1, -1], // 5
	[-1, -1, -1, 70, -1, -1, 0, 45, -1, -1, -1, 100, -1, -1, -1], // 6
	[-1, -1, -1, -1, 80, -1, 45, 0, 55, -1, -1, -1, -1, -1, 115], // 7
	[-1, -1, 85, -1, -1, -1, -1, 55, 0, 70, -1, -1, 80, -1, -1], // 8
	[90, -1, -1, -1, -1, 65, -1, -1, 70, 0, 60, -1, -1, -1, -1], // 9
	[-1, -1, -1, -1, -1, 90, -1, -1, -1, 60, 0, 50, 95, -1, -1], // 10
	[-1, -1, -1, -1, -1, -1, 100, -1, -1, -1, 50, 0, -1, -1, 85], // 11
	[-1, -1, -1, -1, -1, -1, -1, -1, 80, -1, 95, -1, 0, 65, -1], // 12
	[-1, 120, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 65, 0, 75], // 13
	[-1, -1, -1, -1, -1, -1, -1, 115, -1, -1, -1, 85, -1, 75, 0], // 14
];

// 5 planes with varied speeds and capacities
const planes = [
	new Airplane(50, 400, 0),
	new Airplane(50, 400, 1),
	new Airplane(50, 400, 2),
	new Airplane(50, 400, 3),
	new Airplane(50, 400, 4),
];

const graph = buildAirportNetwork(distanceMatrix);
console.log(packages.length);
process.exit(-1);
var bestSolutionInfo = {
	bestTotal: Infinity,
	bestSolution: null,
};
console.timeLog('DeliveryScheduling');

const deliveryPlan = scheduleDeliveries(packages, planes, 0, bestSolutionInfo, graph);
console.timeEnd('DeliveryScheduling');

console.log(`Total Packages: ${packages.length}`);
console.log(`Total Distance: ${deliveryPlan.bestTotal}`);
console.log(
	`Routes Generated: ${deliveryPlan.bestSolution.reduce((sum, plane) => sum + plane.routes.length, 0)}`,
);
console.log(JSON.stringify(deliveryPlan, null, 2));
