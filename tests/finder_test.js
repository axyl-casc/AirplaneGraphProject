const { buildAirportNetwork, Airport } = require('../airport_graph_builder');
const { scheduleDeliveries } = require('../pathFinder');
const { Airplane } = require('../airplane');
const { TemooPackage } = require('../package'); // ✅ Corrected class name

const TIME_LIMIT = 24 * 60; // 24 hours in minutes

// Distance matrix between airports (0 and 1)
const distanceMatrix = [
    [0, 40],  // Airport 0 to Airport 1 (40 km)
    [40, 0]   // Airport 1 to Airport 0 (40 km)
];

// Initialize package objects correctly
const packages = [
  new TemooPackage({ id: 1, weight: 100, arrivalTime: "00:34", destination: 1 }) // ✅ Uses `TemooPackage`
];

// Initialize airplane objects
const planes = [
  new Airplane(100, 500) // ✅ Plane with speed 100 km/h and weight capacity 500
];

// Build airport graph
const graph = buildAirportNetwork(distanceMatrix);

// Run the delivery scheduler
const deliveryPlan = scheduleDeliveries(graph, packages, 0, planes[0]); // ✅ Passes a single plane

// Output the delivery plan
console.log(JSON.stringify(deliveryPlan, null, 2));
