class TemooPackage {
	//{ "id": 1, "weight": 100, "arrivalTime": "00:34", deadlineTime: "12:24", "destination": 2 } is sample object for constructor
	constructor(singlePackageData) {
		this.id = singlePackageData.id;
		this.weight = singlePackageData.weight;
		this.arrivalTime = this.convertStringToInt(singlePackageData.arrivalTime);
		this.deadlineTime = this.convertStringToInt(singlePackageData.deadlineTime);
		this.destination = singlePackageData.destination;
	}

	deepCopy() {
		return new TemooPackage({
			id: this.id,
			weight: this.weight,
			arrivalTime: this.convertIntToString(this.arrivalTime),
			deadlineTime: this.convertIntToString(this.deadlineTime),
			destination: this.destination,
		});
	}

	/*
	 * Converts time in minutes to a "hh:mm" string
	 */
	convertIntToString(timeInMinutes) {
		const hours = Math.floor(timeInMinutes / 60);
		const minutes = timeInMinutes % 60;
		return `${hours.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}`;
	}

	/*
	 * Converts "hh:mm" string to minutes in integer format
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
