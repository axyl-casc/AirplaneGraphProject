const { buildAirportNetwork, Airport } = require('../airport_graph_builder');
const { scheduleDeliveries } = require('../pathFinder');
const { Airplane } = require('../airplane');
const { TemooPackage } = require('../package'); // ✅ Corrected class name

const TIME_LIMIT = 24 * 60; // 24 hours in minutes

// Distance matrix between airports (0 and 1)
const distanceMatrix = [
    [0, 100, -1],  // Airport 0 to Airport 1 (40 km)
    [100, 0, 100],   // Airport 1 to Airport 0 (40 km)
    [-1, 100, 0]
];

// Generate a list of 10 packages with different arrival times
const packages = [];
packages.push(new TemooPackage({
    id: 1,
    weight: 50,
    arrivalTime: "00:00", // Arrival at 00:00, 01:00, ..., 09:00
    destination: 1
}));
packages.push(new TemooPackage({
    id: 2,
    weight: 50,
    arrivalTime: "00:00", // Arrival at 00:00, 01:00, ..., 09:00
    destination: 2
}));
packages.push(new TemooPackage({
    id: 3,
    weight: 50,
    arrivalTime: "00:10", // Arrival at 00:00, 01:00, ..., 09:00
    destination: 2
}));
// Initialize airplane objects
const planes = [
  new Airplane(100, 100), // ✅ Plane with speed 100 km/h and weight capacity 500
  new Airplane(100, 100) // ✅ Plane with speed 100 km/h and weight capacity 500
];

// Build airport graph
const graph = buildAirportNetwork(distanceMatrix);

// Run the delivery scheduler for 10 rounds, incrementing current_time by 30 minutes each time
let current_time = 0;
for (let round = 1; round <= 10; round++) {
    console.log(`\n🔹 **Round ${round}: Current Time = ${current_time} minutes**`);
    
    const deliveryPlan = scheduleDeliveries(graph, packages, current_time, planes[0]); // ✅ Passes a single plane
    
    console.log(JSON.stringify(deliveryPlan, null, 2));

    // Increment current_time by 30 minutes for next round
    current_time += 30;
}
