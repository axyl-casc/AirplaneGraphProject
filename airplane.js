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
	 *  Returns: a boolean value indicating whether to remove the Package or not.
	 */
	takePackage(temooPackage) {
		let remainingCapacity = this.weightCapacity - this.sumPackagesOnPlane();
		//this.weightCapacity
	}

	sumPackagesOnPlane(temooPackages) {
		let sum = 0;
		for (let temooPackage of temooPackages) {
			sum += temooPackage.weight;
		}
	}
}
