communities_metrics <- function(file, file_comms){
  subg <- induced_subgraph(file, vids=file_comms[[1]])
  df <- data.frame(Model.Alg=deparse(substitute(file_comms)),
                   Community.Number=1,
                   Edge.Density=edge_density(subg),
                   Transitivity=transitivity(subg, "global"),
                   Vertex.Count=vcount(subg),
                   Diameter=diameter(subg),
                   Avg.Path.Length=average.path.length(subg))
  
  for(i in seq(2, length(sizes(file_comms)))){
    subg <- induced_subgraph(file, vids=file_comms[[i]])
    df.temp <- data.frame(Model.Alg=deparse(substitute(file_comms)),
                          Community.Number=i,
                          Edge.Density=edge_density(subg),
                          Transitivity=transitivity(subg, "global"),
                          Vertex.Count=vcount(subg),
                          Diameter=diameter(subg),
                          Avg.Path.Length=average.path.length(subg))
    
    df <- rbind(df, df.temp)
  }
  return(df)
}