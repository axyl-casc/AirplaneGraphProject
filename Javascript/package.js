/**
 * Represents a package in the TEMOO Cargo system.
 */
class TemooPackage {
    /**
     * Creates an instance of TemooPackage.
     * 
     * @constructor
     * @param {Object} singlePackageData - An object containing package properties.
     * @param {number} singlePackageData.id - Unique identifier for the package.
     * @param {number} singlePackageData.weight - Weight of the package.
     * @param {string} singlePackageData.arrivalTime - Arrival time in "hh:mm" format.
     * @param {string} singlePackageData.deadlineTime - Deadline time in "hh:mm" format.
     * @param {number} singlePackageData.destination - Destination ID of the package.
     * 
     */
    constructor(singlePackageData) {
        this.id = singlePackageData.id;
        this.weight = singlePackageData.weight;
        this.arrivalTime = this.convertStringToInt(singlePackageData.arrivalTime);
        this.deadlineTime = this.convertStringToInt(singlePackageData.deadlineTime);
        this.destination = singlePackageData.destination;
    }

    /**
     * Creates a deep copy of the current package instance.
     * 
     * @returns {TemooPackage} A new TemooPackage object with the same properties.
     * 
     */
    deepCopy() {
        return new TemooPackage({
            id: this.id,
            weight: this.weight,
            arrivalTime: this.convertIntToString(this.arrivalTime),
            deadlineTime: this.convertIntToString(this.deadlineTime),
            destination: this.destination,
        });
    }

    /**
     * Converts an integer representing time in minutes to a "hh:mm" formatted string.
     * 
     * @private
     * @param {number} timeInMinutes - The time in minutes.
     * @returns {string} The formatted time string in "hh:mm" format.
     * 
     */
    convertIntToString(timeInMinutes) {
        const hours = Math.floor(timeInMinutes / 60);
        const minutes = timeInMinutes % 60;
        return `${hours.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}`;
    }

    /**
     * Converts a "hh:mm" formatted time string to minutes as an integer.
     * 
     * @private
     * @param {string} timeString - The time string in "hh:mm" format.
     * @returns {number} The time in minutes.
     * 
     */
    convertStringToInt(timeString) {
        let stringParts = timeString.split(':');
        let hours = Number(stringParts[0]);
        let minutes = Number(stringParts[1]);
        let timeStamp = hours * 60 + minutes;
        return timeStamp;
    }
}

module.exports = { TemooPackage };
