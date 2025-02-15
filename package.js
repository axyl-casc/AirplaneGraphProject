const TIME_LIMIT = 1440; //in minutes, the time limit for delivery from arrival to destintation.

class TemooPackage {
	constructor({ id, weight, arrivalTime, destination }) {
	  this.id = id;
	  this.weight = weight;
	  this.arrivalTime = this.parseTime(arrivalTime);
	  this.destination = destination;
	}
  
	parseTime(timeStr) {
	  const [hours, minutes] = timeStr.split(':').map(Number);
	  return hours * 60 + minutes;
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

module.exports = {TemooPackage}