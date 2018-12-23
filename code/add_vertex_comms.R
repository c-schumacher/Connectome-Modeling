add_vertex_comms <- function(g, list_comms, pattern=NULL){
  model <- g
  if (is.null(pattern)){
    for(i in colnames(list_comms)){
      model <- set_vertex_attr(model, i, index=V(model), value=list_comms[,i])
    }
  } else {
    for(i in colnames(list_comms)){
      if(grepl(pattern, i) == TRUE){
        model <- set_vertex_attr(model, i, index=V(model), value=get(i)$membership)
      }
    }
  }
  return(model)
}