const {
  loadFiles,
  verifyDistanceMatrixStructure,
  verifyConstraintsStructure,
  verifyPackageStructure,
} = require("../../parser.js"); // Adjust path if needed
const fs = require("fs");

// replace the standard fs module
jest.mock("fs");

// saving the original to restore later
const originalProcessExit = process.exit;
const originalProcessArgv = process.argv;
const originalConsoleLog = console.log;

describe("loadFiles", () => {
  beforeEach(() => {
    // replacing these calls inside the loadfiles() function with mock versions
    process.exit = jest.fn();
    console.log = jest.fn();
    jest.clearAllMocks(); // Don't necessarily need this, but good practice
  });

  afterEach(() => {
    // restoring to the original
    process.exit = originalProcessExit;
    process.argv = originalProcessArgv;
    console.log = originalConsoleLog;
  });

  test("should successfully load three valid JSON files", () => {
    process.argv = [
      "node",
      "main.js",
      "file1.json",
      "file2.json",
      "file3.json",
    ];

    fs.existsSync.mockReturnValue(true);

    const mockFile1 = '{"key1": "value1"}';
    const mockFile2 = '{"key2": "value2"}';
    const mockFile3 = '{"key3": "value3"}';

    fs.readFileSync
      .mockReturnValueOnce(mockFile1)
      .mockReturnValueOnce(mockFile2)
      .mockReturnValueOnce(mockFile3);

    const result = loadFiles();
    expect(result).toEqual([
      { key1: "value1" },
      { key2: "value2" },
      { key3: "value3" },
    ]);
  });

  test("should exit when file does not exist", () => {
    process.argv = [
      "node",
      "main.js",
      "file1.json",
      "file2.json",
      "file3.json",
    ];

    fs.existsSync
      .mockReturnValueOnce(true)
      .mockReturnValueOnce(false)
      .mockReturnValueOnce(true);

    loadFiles();
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when JSON parsing fails", () => {
    process.argv = [
      "node",
      "main.js",
      "file1.json",
      "file2.json",
      "file3.json",
    ];

    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue("invalid json");

    loadFiles();
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when cmd arguments are less than 5", () => {
    process.argv = ["node", "main.js", "file1.json", "file2.json"];
    fs.existsSync.mockReturnValue(true);
    fs.readFileSync.mockReturnValue("invalid json");

    loadFiles();
    expect(process.exit).toHaveBeenCalledWith(-1);
  });
});

describe("verifyDistanceMatrixStructure", () => {
  beforeEach(() => {
    // replacing these calls inside the verifyDistanceMatrixStructure() function with mock versions
    process.exit = jest.fn();
    console.log = jest.fn();
    jest.clearAllMocks();
  });

  afterEach(() => {
    process.exit = originalProcessExit;
    process.argv = originalProcessArgv;
    console.log = originalConsoleLog;
  });

  test("should exit when the provided matrix is not a 2d array", () => {
    verifyDistanceMatrixStructure("Incorrect format ");
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when the provided matrix is not a square matrix", () => {
    verifyDistanceMatrixStructure([[22, 33], [2]]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when matrix is empty", () => {
    verifyDistanceMatrixStructure([]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the row is not an array", () => {
    verifyDistanceMatrixStructure([3, 2, 4], "adaa ", [2, 2, 2]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the row is empty an array", () => {
    verifyDistanceMatrixStructure([[3, 2, 4], [], [2, 2, 2]]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should accept a valid square (3 x 3) matrix", () => {
    verifyDistanceMatrixStructure([
      [3, 2, 4],
      [2, 2, 2],
      [1, 2, 3],
    ]);
    expect(process.exit).not.toHaveBeenCalled();
  });

  test("should exit when an element in the matrix is a string ", () => {
    verifyDistanceMatrixStructure([
      [3, "a", 4],
      [2, 2, 2],
      [1, 2, 3],
    ]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when an element in the matrix is less than -1 ", () => {
    verifyDistanceMatrixStructure([
      [3, -22, 4],
      [2, 2, 2],
      [1, 2, 3],
    ]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when an element is null ", () => {
    verifyDistanceMatrixStructure([
      [3, null, 4],
      [2, 2, 2],
      [1, 2, 3],
    ]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when an element is a float ", () => {
    verifyDistanceMatrixStructure([
      [3, 0.11, 4],
      [2, 2, 2],
      [1, 2, 3],
    ]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });
});

describe("verifyConstraintsStructure", () => {
  beforeEach(() => {
    // replacing these calls inside the verifyConstraintsStructure() function with mock versions
    process.exit = jest.fn();
    console.log = jest.fn();
    jest.clearAllMocks();
  });

  afterEach(() => {
    process.exit = originalProcessExit;
    process.argv = originalProcessArgv;
    console.log = originalConsoleLog;
  });

  test("should exit when constraints data is not an object", () => {
    verifyConstraintsStructure("Incorrect format ");
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when the constraints data is empty", () => {
    verifyConstraintsStructure({});
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when numOfPlanes is missing", () => {
    verifyConstraintsStructure({ speed: 3, weightCapacity: 2 });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when weightCapacity is missing", () => {
    verifyConstraintsStructure({ speed: 3, numOfPlanes: 2 });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when speed is missing", () => {
    verifyConstraintsStructure({ numOfPlanes: 3, weightCapacity: 2 });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when numOfPlanes is not an Integer", () => {
    verifyConstraintsStructure({
      numOfPlanes: "a",
      weightCapacity: 2,
      speed: 3,
    });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when weightCapacity is not an Integer", () => {
    verifyConstraintsStructure({
      numOfPlanes: 1,
      weightCapacity: "a",
      speed: 3,
    });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when speed is not an Integer", () => {
    verifyConstraintsStructure({
      numOfPlanes: 2,
      weightCapacity: 2,
      speed: "a",
    });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should accept constraints data ({speed: 2, weightCapacity: 20, numOfPlanes: 2})", () => {
    verifyConstraintsStructure({
      speed: 2,
      weightCapacity: 20,
      numOfPlanes: 2,
    });
    expect(process.exit).not.toHaveBeenCalled();
  });
  test("should exit when speed is negative", () => {
    verifyConstraintsStructure({
      numOfPlanes: 2,
      weightCapacity: 2,
      speed: -1,
    });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when weightCapacity is negative", () => {
    verifyConstraintsStructure({
      numOfPlanes: 2,
      weightCapacity: -1,
      speed: 3,
    });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when numOfPlanes is negative", () => {
    verifyConstraintsStructure({
      numOfPlanes: -2,
      weightCapacity: 4,
      speed: 3,
    });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when constraint object has extra value", () => {
    verifyConstraintsStructure({
      numOfPlanes: 2,
      weightCapacity: 4,
      speed: 3,
      random: 222,
    });
    expect(process.exit).toHaveBeenCalledWith(-1);
  });
});

describe("verifyPackageStructure", () => {
  beforeEach(() => {
    // replacing these calls inside the verifyPackageStructure() function with mock versions
    process.exit = jest.fn();
    console.log = jest.fn();
    jest.clearAllMocks();
  });

  afterEach(() => {
    process.exit = originalProcessExit;
    process.argv = originalProcessArgv;
    console.log = originalConsoleLog;
  });

  test("should exit when package data is not an array", () => {
    verifyPackageStructure("Incorrect format ");
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when the package data is empty", () => {
    verifyPackageStructure([]);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of packages is not an object", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 150, arrivalTime: "12:34", destination: 3 },
      {},
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
    ];

    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the packages is missing the'id' field", () => {
    const testPackage = [
      { weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 150, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the packages is missing the 'weight' field", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the packages is missing the 'arrivalTime' field", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, destination: 2 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the packages is missing the 'destination' field", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34" },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should accept a valid package data array", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
      { id: 4, weight: 15990, arrivalTime: "11:34", destination: 2 },
      { id: 9, weight: 15990, arrivalTime: "11:24", destination: 99 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).not.toHaveBeenCalled();
  });

  test("should exit when one of the packages id filed is not an Integer", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
      { id: "a", weight: 15990, arrivalTime: "11:34", destination: 2 },
      { id: 9, weight: 15990, arrivalTime: "11:24", destination: 99 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the weight filed is not an Integer", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
      { id: 4, weight: "b", arrivalTime: "11:34", destination: 2 },
      { id: 9, weight: 15990, arrivalTime: "11:24", destination: 99 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the arrivalTime filed is not an string", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
      { id: 4, weight: 999, arrivalTime: 99, destination: 2 },
      { id: 9, weight: 15990, arrivalTime: "11:24", destination: 99 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the destination filed is not an Integer", () => {
    const testPackage = [
      { id: 1, weight: 100, arrivalTime: "00:34", destination: 2 },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
      { id: 4, weight: 999, arrivalTime: "11:22", destination: 2 },
      { id: 9, weight: 15990, arrivalTime: "11:24", destination: "ss" },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });

  test("should exit when one of the packages has an extra field", () => {
    const testPackage = [
      {
        id: 1,
        weight: 100,
        arrivalTime: "00:34",
        destination: 2,
        random: 212121,
      },
      { id: 2, weight: 200, arrivalTime: "12:34", destination: 3 },
      { id: 3, weight: 150, arrivalTime: "11:34", destination: 2 },
      { id: 4, weight: 999, arrivalTime: "11:22", destination: 2 },
      { id: 9, weight: 15990, arrivalTime: "11:24", destination: 2 },
    ];
    verifyPackageStructure(testPackage);
    expect(process.exit).toHaveBeenCalledWith(-1);
  });
});
