const { TemooPackage } = require('./package.js');

class Airplane {
	constructor(planeSpeed, weightCapacity) {
		this.planeSpeed = planeSpeed;
		this.weightCapacity = weightCapacity;
		this.readyToDeliver = true;
		this.currentLocation = 0; // NODE ID
		this.temooPackages = [];
	}

	/*
	 *  Input: Takes in an object of type Package
	 *
	 *  Details: fills in a planes weight capacity and removes weight from the Package, or removes it if necessary.
	 *
	 *  Returns: a boolean value indicating whether taking the package was successful
	 */
	takePackage(temooPackage) {
		let remainingCapacity = this.weightCapacity - this.sumPackagesOnPlane();
		if (remainingCapacity - temooPackage.weight >= 0) {
			this.weightCapacity -= temooPackage.weight;
			temooPackage.weight = 0;
			return true;
		} else {
			return false;
		}
	}

	sumPackagesOnPlane() {
		let sum = 0;
		for (let temooPackage of this.temooPackages) {
			sum += temooPackage.weight;
		}
		return sum;
	}
}

module.exports = { Airplane };
