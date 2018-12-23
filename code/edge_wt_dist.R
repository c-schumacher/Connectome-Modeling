edge_wt_dist <- function(file, mode_select="total",
                        title_select="Edge Weight Distribution of Neurons", 
                        output="graph", bwidth=5, xtick=5, ymax=1250, ytick=250){
  wts <- E(file)$weight
  type.factor = factor(E(file)$gap.type)
  send <- wts[type.factor == "Send"]
  gap <- wts[type.factor == "GapJunction"]
  
  edge_types.df <- rbind(data.frame(Weight=send, Type="Send (Chemical)"),
                         data.frame(Weight=gap, Type="Gap Junction (Electrical)"))
  if(output == "graph"){
    g <- ggplot(edge_types.df , aes(x=Weight, fill=Type))
    g <- g + geom_histogram(binwidth=bwidth, col="white", breaks=seq(0,max(wts)+10,by=1))
    g <- g + labs(x="Edge Weights", y="Count (Log Scale)", title=title_select)
    g <- g + scale_x_continuous(breaks=seq(0,max(wts)+10, by=xtick))
    g <- g + scale_y_log10()
    print(g)    
  } else if (output == "stats") {
    return(wts)
  }
}