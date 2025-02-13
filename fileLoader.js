
const fs = require('fs');
const path = require('path');


function loadFiles () {
    const cmdArgs = process.argv.slice(2);
    if(cmdArgs.length !== 3) {
        console.log("Incorrect usage, Example usage: node main.js distance_matrix.json package_data.json constraints.json");
        process.exit(-1);
    }
    
    return cmdArgs.map((filePath) => {
        if(!fs.existsSync(filePath)) {
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


module.exports = loadFiles;

