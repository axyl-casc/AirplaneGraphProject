document.addEventListener("DOMContentLoaded", () => {
    document.getElementById("graphForm").addEventListener("submit", (event) => {
        event.preventDefault();
        
        const nodes = document.getElementById("nodes").value.split(",").map(n => n.trim());
        const edgesInput = document.getElementById("edges").value.split(",");
        const edges = edgesInput.map(edge => edge.trim().split("-"));
        const airplane = document.getElementById("airplane").value.trim();
        const fuel = Number.parseInt(document.getElementById("fuel").value, 10);

        const graph = { nodes: nodes, edges: edges, airplane: airplane, fuel: fuel };
        
        document.getElementById("output").textContent = JSON.stringify(graph, null, 2);
        console.log("Graph submitted:", graph);
    });
});