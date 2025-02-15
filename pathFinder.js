const TIME_LIMIT = 24 * 60; // 24 hours in minutes
const TURNAROUND_TIME = 0; // minutes

/**
 * Schedules deliveries for a single plane using the precomputed airport graph.
 * @param {Airport[]} graph - The airport network with shortest paths.
 * @param {Package[]} packages - Array of package objects.
 * @param {number} current_time - The current time in minutes.
 * @param {Airplane} plane - The single airplane object.
 * @returns {Object} - Flight schedule for the plane.
 */
function scheduleDeliveries(graph, packages, current_time, plane) {
  const hub = 0; // Central hub airport ID
  const planeSchedule = [];

  console.log("Starting delivery scheduling...");

  while (packages.length > 0) {
    console.log("Current time:", current_time);
    console.log("Remaining packages:", packages.length);

    // Only take packages that are within the delivery timeframe.
    let availablePackages = packages.filter(pkg => 
      pkg.getTime() <= current_time && (pkg.getTime() + TIME_LIMIT) >= current_time
    );

    console.log("Available packages for this iteration:", availablePackages.length);

    if (availablePackages.length === 0) {
      return
    }

    const routePlan = findBestRoute(graph, availablePackages, plane, hub, current_time);
    if (!routePlan) {
      console.log("No valid route found. Ending scheduling.");
      break;
    }

    console.log("Planned route:", routePlan);
    planeSchedule.push(routePlan);

    // Update current_time to the finish time of this flight.
    current_time = routePlan.time;

    // Remove delivered packages.
    packages = packages.filter(pkg => !routePlan.packages.includes(pkg.id));
    console.log("Packages after delivery:", packages.length);
  }

  console.log("Final delivery schedule:", planeSchedule);
  return { plane: plane.id, flights: planeSchedule };
}

/**
 * Finds the best route for a plane to deliver packages from the hub.
 * Uses the **precomputed shortest paths** from the airport graph.
 * @param {Airport[]} graph - The airport network.
 * @param {Object[]} packages - Packages to deliver.
 * @param {Object} plane - Plane object with weight_capacity.
 * @param {number} hub - The central hub airport ID.
 * @param {number} current_time - The current time in minutes.
 * @returns {Object|null} - Best route with package details or null if no route found.
 */
function findBestRoute(graph, packages, plane, hub, current_time) {
  console.log("Finding best route for plane:", plane.id);
  const selectedPackages = selectPackages(packages, plane.weightCapacity);
  console.log("Selected packages for delivery:", selectedPackages);

  if (selectedPackages.length === 0) return null;

  // Build the route: Hub -> Destinations -> Hub.
  const destinations = selectedPackages.map(pkg => pkg.destination);
  const route = [hub, ...destinations, hub];

  let totalDistance = 0;
  for (let i = 0; i < route.length - 1; i++) {
    const from = route[i];
    const to = route[i + 1];
    // Using the precomputed shortest path distance.
    totalDistance += graph[from].connections.get(to).distance;
  }

  // Compute the flight duration in minutes.
  const flightDuration = (totalDistance / plane.planeSpeed) * 60;
  // Ensure the plane doesn't depart before the last package arrives.
  const departureTime = Math.max(current_time, ...selectedPackages.map(pkg => pkg.arrivalTime));
  const finishTime = departureTime + flightDuration + TURNAROUND_TIME;

  console.log("Computed route distance:", totalDistance);
  return {
    route,
    packages: selectedPackages.map(pkg => pkg.id),
    time: finishTime
  };
}

/**
 * Selects packages based on plane weight capacity.
 * @param {Object[]} packages - Packages to deliver.
 * @param {number} capacity - Plane weight capacity.
 * @returns {Object[]} - Selected packages within capacity.
 */
function selectPackages(packages, capacity) {
  let totalWeight = 0;
  const selected = [];

  console.log("Selecting packages within weight capacity:", capacity);
  for (const pkg of packages) {
    if (totalWeight + pkg.weight <= capacity) {
      selected.push(pkg);
      totalWeight += pkg.weight;
      console.log(`Package ${pkg.id} added. Total weight:`, totalWeight);
    }
  }

  return selected;
}

module.exports = { scheduleDeliveries };
