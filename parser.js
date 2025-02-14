const fs = require('fs');

/**
 * Loads, parses and returns the JSON files specified as command-line arguments
 *
 * This function expects exactly three file paths as arguments: distance matrix, package data, and constraints.
 * If the number of arguments is incorrect or a file is not found or cannot be parsed,
 * the program will exit with an error message.
 *
 * @returns {Array<object>} An array containing the parsed JSON data from the three files.
 *          The order of elements in the array corresponds to the order of file paths provided
 *          in command-line arguments (distance matrix, package data, constraints).
 */
function loadFiles() {
	const cmdArgs = process.argv.slice(2);
	if (cmdArgs.length !== 3) {
		console.log(
			'Incorrect usage, Example usage: node main.js distance_matrix.json package_data.json constraints.json',
		);
		process.exit(-1);
	}
	return cmdArgs.map((filePath) => {
		if (!fs.existsSync(filePath)) {
			console.log(`Error: File not found: ${filePath}`);
			process.exit(-1);
		}

		try {
			const fileData = fs.readFileSync(filePath, 'utf-8');
			return JSON.parse(fileData);
		} catch (error) {
			console.log(`Error reading file: ${filePath}, Error: ` + error);
			process.exit(-1);
		}
	});
}

/**
 * Verifies if the provided matrix is in the proper distance matrix structure.
 *
 * A valid distance matrix must meet the following criteria:
 * - It must be an array (matrix).
 * - It must not be empty.
 * - Each row must be an array.
 * - Each row must not be empty.
 * - It is a square matrix
 * - All elements within the matrix must be integers.
 * - All elements within the matrix must be greater than or equal to -1.
 *
 * The program will exit with an error message if the matrix does not meet these criterion
 *
 * @param {Array<Array<number>>} matrix - The matrix to be verified as a distance matrix.
 *                                        It should be a 2D array of Integers.
 */
function verifyDistanceMatrixStructure(matrix) {
	const BASE_ERROR_MSG = 'Error parsing distance matrix file: ';

	try {
		if (!Array.isArray(matrix)) {
			printErrorAndExit(BASE_ERROR_MSG + 'Input is not a matrix (not an array).');
		}
		const numOfRows = matrix.length;
		if (matrix.length === 0) {
			printErrorAndExit(BASE_ERROR_MSG + 'Matrix is empty');
		}

		for (let row of matrix) {
			if (!Array.isArray(row)) {
				printErrorAndExit(BASE_ERROR_MSG + 'One of the rows is not an array');
			}
			if (row.length === 0) {
				printErrorAndExit(BASE_ERROR_MSG + 'One of the rows is empty');
			}
			const numOfColumns = row.length;
			if (numOfColumns != numOfRows) {
				printErrorAndExit(BASE_ERROR_MSG + "Number of columns and rows don't match");
			}

			for (let element of row) {
				if (!Number.isInteger(element)) {
					printErrorAndExit(BASE_ERROR_MSG + `${element} is not an integer`);
				}
				if (element < -1) {
					printErrorAndExit(BASE_ERROR_MSG + `${element} in the distance matrix is less than -1`);
				}
			}
		}
	} catch (error) {
		printErrorAndExit(
			BASE_ERROR_MSG +
				`An unexpected error occurred while validating the matrix data.\n` +
				`Error: ${error.message}`,
		);
	}
}

/**
 * Verifies if the provided constraints data is in the proper/expected constraints structure.
 *
 * The constraints data must be an object with exactly three keys:
 * 'num_of_planes', 'weight_capacity', and 'speed'.
 * Each value from the key must be greater than or equal to 1 and must also be an Integer
 *
 * The program will exit with an error message if the constraintsData does not match the expected structure does not meet these criterion
 *
 * @param {object} constraintsData - The data to be verified as constraints.
 *                                   It should be an object with keys 'num_of_planes', 'weight_capacity', and 'speed',
 *                                   where each value is an Integer greater than or equal to 1.
 */
function verifyConstraintsStructure(constraintsData) {
	const BASE_ERROR_MSG = 'Error parsing constraints file: ';
	try {
		if (typeof constraintsData !== 'object' || constraintsData === null) {
			printErrorAndExit(BASE_ERROR_MSG + 'constraints data must be a valid JSON object.');
		}
		if (Object.keys(constraintsData).length !== 3) {
			printErrorAndExit(
				BASE_ERROR_MSG +
					'Incorrect number of keys. Constraints object must contain exactly 3 keys, but found ${keys.length}.\n' +
					"Expected keys are: 'num_of_planes', 'weight_capacity', and 'speed'.",
			);
		}

		if (!('numOfPlanes' in constraintsData)) {
			printErrorAndExit(BASE_ERROR_MSG + "Missing required key 'numOfPlanes'.");
		}

		if (!('weightCapacity' in constraintsData)) {
			printErrorAndExit(BASE_ERROR_MSG + "Missing required key 'weightCapacity'.");
		}

		if (!('speed' in constraintsData)) {
			printErrorAndExit(BASE_ERROR_MSG + "Missing required key 'speed'.");
		}

		for (let property in constraintsData) {
			if (!Number.isInteger(constraintsData[property])) {
				printErrorAndExit(BASE_ERROR_MSG + `${property} has to be an Integer `);
			}

			if (constraintsData[property] < 1) {
				printErrorAndExit(
					BASE_ERROR_MSG + `${property} has to be more than greater than or equal to 1 `,
				);
			}
		}
	} catch (error) {
		printErrorAndExit(
			BASE_ERROR_MSG +
				`An unexpected error occurred while validating the constraints data\n` +
				'Error: ' +
				error.message,
		);
	}
}

/**
 * Verifies if the provided package data is in the proper/expected package data structure.
 *
 * The package data must be an array of objects. Each object must have exactly 4 keys:
 * 'arrivalTime', 'destination', 'weight' and Id.
 * 'arrivalTime' must be a string. 'destination', 'weight' and 'id' must be integers and greater than 0
 *
 * The program will exit with an error message if the packageData does not match the expected structure.
 *
 * @param {object} packageData - The data to be verified as package data.
 */
function verifyPackageStructure(packageData) {
	const BASE_ERROR_MSG = 'Error parsing package data file: ';

	try {
		if (!Array.isArray(packageData)) {
			printErrorAndExit(BASE_ERROR_MSG + 'Input is not an array.');
		}

		const numOfRows = packageData.length;
		if (packageData.length === 0) {
			printErrorAndExit(BASE_ERROR_MSG + 'packageData is empty');
		}

		for (const row of packageData) {
			if (typeof row !== 'object' || row === null) {
				printErrorAndExit(BASE_ERROR_MSG + 'Each package must be a valid JSON object.');
			}

			const expectedKeys = ['id', 'weight', 'arrivalTime', 'destination'];
			if (Object.keys(row).length !== expectedKeys.length) {
				printErrorAndExit(
					BASE_ERROR_MSG +
						`Incorrect number of keys. Package data object must contain exactly ${expectedKeys.length} keys\n` +
						"Expected keys are: 'id', 'weight','arrivalTime' and 'destination' .",
				);
			}

			for (const key of expectedKeys) {
				if (!(key in row)) {
					printErrorAndExit(BASE_ERROR_MSG + `Missing required key '${key}'`);
				}
			}

			if (!Number.isInteger(row.id) || row.id < 1) {
				printErrorAndExit(
					BASE_ERROR_MSG + 'id is in invalid format. Must be an integer and greater than 0',
				);
			}

			if (!Number.isInteger(row.weight) || row.weight < 1) {
				printErrorAndExit(
					BASE_ERROR_MSG + 'weight is in invalid format. Must be an integer and greater than 0',
				);
			}

			if (!Number.isInteger(row.destination) || row.destination < 1) {
				printErrorAndExit(
					BASE_ERROR_MSG +
						'destination is in invalid format. Must be an integer and greater than 0',
				);
			}

			if (typeof row.arrivalTime != 'string') {
				printErrorAndExit(BASE_ERROR_MSG + 'arrivalTime must be a string');
			}
		}
	} catch (error) {
		printErrorAndExit(
			BASE_ERROR_MSG +
				`An unexpected error occurred while validating the package data.\n` +
				`Error: ${error.message}`,
		);
	}
}

/**
 * This function will print the error message and then exit with the code of '-1'
 * @param {string} errorMessage - error message to be printed before exiting
 */
function printErrorAndExit(errorMessage) {
	console.log(errorMessage);
	process.exit(-1);
}

/**
 * Loads, parses, and verifies input files (distance matrix, package data, and constraints).
 *
 * This function first loads the input files using the loadFiles function which assumes that the user provided the
 * file paths in the command line arguments.
 * It then validates the structure of each loaded data set using the respective verification functions:
 * verifyDistanceMatrixStructure, verifyConstraintsStructure}, and link verifyPackageStructure.
 *
 * If any of the loading or validation steps fail (e.g., file not found, invalid JSON, data structure mismatch),
 * the program will terminate with an error message.
 *
 * @returns {Array<object>} An array containing the parsed and validated JSON data from the three input files
 *          in the following order:
 *            0: Distance matrix data
 *            1: Package data (as returned by
 *            2: Validated constraints data
 */
function parseAndLoadInputFiles() {
	const [distanceMatrix, packageData, constraintsData] = loadFiles();
	verifyDistanceMatrixStructure(distanceMatrix);
	verifyConstraintsStructure(constraintsData);
	verifyPackageStructure(packageData);
	return [distanceMatrix, packageData, constraintsData];
}

module.exports = {
	loadFiles,
	verifyDistanceMatrixStructure,
	verifyConstraintsStructure,
	verifyPackageStructure,
	printErrorAndExit,
	parseAndLoadInputFiles,
};

function parseDistanceMatrix(matrixData) {
	let airports = [[]];
}

function parsePackageData(packageData) {
	//stuff
}

function parseShippingConstraints(shipppingConstraints) {
	//stuff
}
