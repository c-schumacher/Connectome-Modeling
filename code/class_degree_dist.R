class_degree_dist <- function(file, mode_select="total", weighted=FALSE, weight_vals=NULL,
                           title_select="Degree Distribution by Neuron Class", 
                           output="graph", bwidth=5, xtick=10, ytick=5, pos="dodge",
                           ymax=25){
  if(weighted == FALSE){
    deg <- degree(file, mode=mode_select)
    labx <- "Degree"
  } else if (weighted == TRUE){
    deg <- graph.strength(file, mode=mode_select, weights=weight_vals)
    labx<- "Weighted Degree"
  }
    type.factor = factor(V(file)$Type_Main)
    motor <- deg[type.factor == "Motor Neuron"]
    sense <- deg[type.factor == "Sensory Neuron"]
    inter <- deg[type.factor == "Interneuron"]
    unknown <- deg[type.factor == "Unknown"]
    
    deg_types.df <- rbind(data.frame(Degree=motor, Type="Motor Neuron"),
                          data.frame(Degree=sense, Type="Sensory Neuron"),
                          data.frame(Degree=inter, Type="Interneuron"),
                          data.frame(Degree=unknown, Type="Unknown"))
    if(output == "graph"){
      g <- ggplot(deg_types.df, aes(x=Degree, fill=Type))
      g <- g + geom_histogram(position=pos, binwidth=bwidth)
      g <- g + scale_x_continuous(breaks=seq(0,max(deg_types.df$Degree)+5, by=xtick))
      g <- g + scale_y_continuous(breaks=seq(0,ymax, by=ytick))
      g <- g + labs(x=labx, y="Count of Neurons", title=title_select)
      print(g)
    } else if (output == "stats"){
      return(deg_types.df)
    }
}
