const { parseAndLoadInputFiles } = require("./parser.js");
// node main.js ./sample_input_files/sample_distance.js ./sample_input_files/sample_package.js ./sample_input_files/sample_constraints.js
const [distanceMatrix, packageData, constraints] = parseAndLoadInputFiles();
console.log(distanceMatrix);
console.log(packageData);
console.log(constraints);
