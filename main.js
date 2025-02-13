const loadFiles = require('./fileLoader.js');

const [distanceMatrix, packageData, constraints] = loadFiles();
console.log(distanceMatrix);
console.log(packageData);
console.log(constraints);


