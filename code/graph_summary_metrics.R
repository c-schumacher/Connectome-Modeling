graph_metrics <- function(g, file_comms, wts){
  df <- data.frame(Model.Alg=deparse(substitute(file_comms)),
                   Modularity=modularity_dir_wt(g),
                   Transitivity=transitivity(g, "global"),
                   Edge.Within.Count=length(E(g)[!crossing(file_comms, g)]),
                   Edge.Between.Count=length(E(g)[crossing(file_comms, g)]),
                   Edge.Density=edge_density(g),
                   Total.Edge.Count=ecount(g))
  return(df)
}