library(shortestpath)
library(igraph)
library(magrittr)
library(ggplot2)
require(gridExtra)

algorithms <- list(
    "Dijkstra"=function(graph, from, to){
        dijkstra(graph, from, to)
    },
    "A* Search"=function(graph, from, to){
        aStarSearch(graph, from, to)
    },
    "Bellman-Ford"=function(graph, from, to){
        bellmanFord(graph, from, to)
    },
    "Floyd-Warshall"=function(graph, from, to){
        floydWarshall(graph)
    }
)

instance_sizes <- seq(1,13,1)
iterations <- 1
results <- data.frame(nodes=instance_sizes**2)

#instance_sizes <- seq(100, 5100, 1000)
get_graph <- function(i){
    make_lattice(length=i,dim=2) %>%
        as.spgraph() %>%
        setVertexCoordinatesFromLayout(on_grid(width=i,height=i)) %>%
        permuteGraph()
}

for(algorithm in names(algorithms)){
    print(algorithm)
    execution_time <- c()
    steps <- c()

    for(i in instance_sizes){
        print(i)

        algorithm_fun <- algorithms[[algorithm]]
        graph <- get_graph(i)
        t <- system.time( for(iters in seq_len(iterations))
            r <- algorithm_fun(graph,
                               ifelse(i > 5,"1","A"),
                               ifelse(i > 5,as.character(i), LETTERS[i]))
        )[["elapsed"]]

        execution_time <- c(execution_time, t/iterations)
        steps <- c(steps, length(r))
    }
    results[[paste(algorithm,"Time")]] <- execution_time
    results[[paste(algorithm,"Steps")]] <- steps
}

showPlots <- function(attribute){

    plots <- lapply(names(algorithms), function(algorithm){
        yvals <- results[[paste(algorithm, attribute)]]
        qplot(instance_sizes,
              yvals,
              main=algorithm,
              xlab="Grid Size",
              ylab=attribute
        )
    })
    do.call("grid.arrange", c(
        plots, ncol=2, nrow=2, top = "Comparison of Shortest Path Algorithms on a Grid"
        ))
}


for(attribute in c("Time","Steps")){
    png(filename=paste0("demo/benchmark_",attribute,".png"),
        type="cairo",
        units="cm",
        width=20,
        height=16,
        pointsize=12,
        res=300)
    showPlots(attribute)
    dev.off()
}

