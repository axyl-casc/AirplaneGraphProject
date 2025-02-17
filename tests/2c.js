const { buildAirportNetwork, Airport } = require('../airport_graph_builder');
const { scheduleDeliveries } = require('../pathFinder.js');
const { Airplane } = require('../airplane');
const { TemooPackage } = require('../package'); // ✅ Corrected class name

const TIME_LIMIT = 24 * 60; // 24 hours in minutes
const pack = [];
let alt = 4;
for (let i = 0; i < 14; i++) {
	pack.push(new TemooPackage({ id: i + 1, weight: 500, arrivalTime: '00:00', destination: alt }));
	alt = alt === 4 ? 5 : 4;
}

const distanceMatrix = [
	[0, 50, -1, -1, -1, -1],
	[50, 0, 50, 30, -1, -1],
	[-1, 50, 0, 60, -1, -1],
	[-1, 30, 60, 0, 60, 20],
	[-1, -1, -1, 60, 0, -1],
	[-1, -1, -1, 20, -1, 0],
];

const planes = [new Airplane(50, 500, 0), new Airplane(50, 500, 1), new Airplane(50, 500, 2)];
const graph = buildAirportNetwork(distanceMatrix);
let currentRoutes = [];
const deliveryPlan = scheduleDeliveries(pack, planes, currentRoutes, graph); // ✅ Passes a single plane
console.log(JSON.stringify(deliveryPlan, null, 2));
