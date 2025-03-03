const { Route } = require('./pathFinder.js');

class Airplane {
	constructor(planeSpeed, weightCapacity, planeNumber) {
		this.speed = planeSpeed;
		this.weightCapacity = weightCapacity;
		this.availableTime = 0;
		this.planeNumber = planeNumber;
		this.routes = [];
	}

	deepCopy() {
		const newPlane = new Airplane(this.speed, this.weightCapacity, this.planeNumber);

		newPlane.availableTime = this.availableTime;
		newPlane.routes = this.routes.map((route) => route.deepCopy());

		return newPlane;
	}
}

module.exports = { Airplane };
