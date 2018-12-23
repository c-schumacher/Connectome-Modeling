modularity_dir_wt <- function(g, wt_steps=6){
  model <- as.matrix(get.adjacency(g, type="upper", attr="weight"))
  W = sum(model)
  communities <- cluster_walktrap(g, weights=E(g)$weight, steps=wt_steps)
  membership <- communities$membership
  
  delta <- function(c1, c2){
    if(c1== c2){
      return(1)
    } else {
      return(0)
    }
  }
  modularity_matrix <- function(g, c){
    c_in <- colSums(g)
    c_out <- rowSums(g)
    num_nodes <- nrow(g)  
    B <- matrix(0, nrow=num_nodes, ncol=num_nodes)
    for (i in seq(1, num_nodes)){
      c_in_i <- c_in[i]
      for (j in seq(1, num_nodes)){
        if (delta(c[i], c[j]) == 1){
          B[i,j] <- g[i,j] - (c_in_i * c_out[j])/W
        }
      }
    }
    return(B)
  }
  return( (1/W)*sum(modularity_matrix(model, membership)))
}