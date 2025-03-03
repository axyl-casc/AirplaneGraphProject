const { Route } = require('./pathFinder.js');

class Airplane {
	constructor(planeSpeed, weightCapacity) {
		this.speed = planeSpeed;
		this.weightCapacity = weightCapacity;
		this.route = new Route(this.speed, this.weightCapacity);
	}

	deepCopy() {
		const newPlane = new Airplane(this.speed, this.weightCapacity);

		newPlane.route = this.route.deepCopy();

		return newPlane;
	}
}

module.exports = { Airplane };
