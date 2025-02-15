const { buildAirportNetwork } = require('../airport_graph_builder');
const { scheduleDeliveries } = require('../pathFinder');
const { Airplane } = require('../airplane')
const { Package } = require('../package')

const distanceMatrix = [
    [0, 40],
    [40, 0]
];

const packages = [
  new Package()
];

const planes = [
    
];

const graph = buildAirportNetwork(distanceMatrix);
const deliveryPlan = scheduleDeliveries(graph, packages, planes);
console.log(JSON.stringify(deliveryPlan, null, 2));
