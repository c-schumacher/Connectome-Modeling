# The function below takes a graph object and scrambles the edge relationships and computes
# modularity based on the custom directed/weighted modularity function using walktrap communities 
shuffle_edges_mod_dir_wt <- function(g, n_graphs){
  original <- as.matrix(get.adjacency(g, type="upper", attr="weight"))
  n <- vcount(g)
  
  sims <- list()
  i <- 1
  while(i < n_graphs)
    for(i in seq(1, n_graphs)){
      new <- original
      new <- new[sample.int(n),]
      new <- new[,sample.int(n)]
      new <- graph_from_adjacency_matrix(new, mode='directed', weighted=T)
      sims[i] <- modularity_dir_wt(new)
      i <- i + 1
    }
  return(unlist(sims))
}