const TIME_LIMIT = 1440; //in minutes, the time limit for delivery from arrival to destintation.

class TemooPackage {
	//{ "id": 1, "weight": 100, "arrivalTime": "00:34", "destination": 2 } is sample object for constructor
	constructor(singlePackageData) {
		this.id = singlePackageData.id;
		this.weight = singlePackageData.weight;
		this.arrivalTime = convertStringToInt(singlePackageData.arrivalTime);
		this.deadlineTime = 60 * 24;
		this.destination = singlePackageData.destination;
	}
}
/*
 *  Input: Expected input format is a string of the form "hh:mm" where hh is hours and mm is minutes
 *
 *  Output: An integer value, a timestamp in minutes.
 */
function convertStringToInt(timeString) {
	let stringParts = timeString.split(':');
	let hours = Number(stringParts[0]);
	let minutes = Number(stringParts[1]);
	let timeStamp = hours * 60 + minutes;
	return timeStamp;
}

module.exports = { TemooPackage };
