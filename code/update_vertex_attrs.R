update_vertex_attrs <- function(g){
  model <- g
  V(model)$bet <- betweenness(model, V(model), weights=NA)
  V(model)$eig <- eigen_centrality(model, directed=TRUE)$vector
  V(model)$pr <- page_rank(model)$vector
  #V(model)$cl <- closeness(model, normalize=TRUE, weights=NA)
  model.wdeg <- graph.strength(model)
  V(model)$wdeg <- model.wdeg
  V(model)$wdegnorm <- model.wdeg / sqrt(sum(model.wdeg^2))
  V(model)$deg <- degree(model)
  return(model)
}