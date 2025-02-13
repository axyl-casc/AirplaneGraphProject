const { parseAndLoadInputFiles } = require("./parser.js");

const [distanceMatrix, packageData, constraints] = parseAndLoadInputFiles();
console.log(distanceMatrix);
console.log(packageData);
console.log(constraints);
