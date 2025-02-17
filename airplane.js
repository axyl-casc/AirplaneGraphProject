class Airplane {
	constructor(planeSpeed, weightCapacity, planeNumber) {
		this.speed = planeSpeed;
		this.weightCapacity = weightCapacity;
		this.availableTime = 0;
		this.planeNumber = planeNumber;
	}
}

module.exports = { Airplane };
