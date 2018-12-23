degree_dist <- function(file, mode_select="total", weighted=FALSE, weight_vals=NULL,
                        title_select="Degree Distribution of Neurons", output="graph"){
  if(weighted == FALSE){
    deg <- degree(file, mode=mode_select)
  } else if (weighted == TRUE){
    deg <- graph.strength(file, weights=weight_vals)
  }
  if(output == "graph") {
    g <- ggplot(data.frame(Degree=deg), aes(x=Degree))
    g <- g + geom_histogram(binwidth=5, col="white", breaks=seq(0,max(deg)+10,by=5))
    g <- g + labs(x="Degree", y="Count", title=title_select)
    g <- g + scale_x_continuous(breaks=seq(0,max(deg)+10, by=25))
    print(g)    
  } else if (output == "stats") {
    return(deg)
  }
}