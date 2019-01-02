# this uses weighted degree as node size for community plots
w_community_plots <- function(g, comm_list, lay_out, title1, title2){
  fname <- paste(visuals_path,
                 title1,"-Full.png", sep="")
  png(filename=fname, width=1000, height=1000, bg="transparent")
  plot(g, vertex.color=membership(comm_list),
       mark.groups=communities(comm_list), vertex.size=3, 
       vertex.label=NA, layout=lay_out,
       main=title1)
  dev.off()
  
  fname <- paste(visuals_path,
                 title1, "-Between.png", sep="")
  png(filename=fname, width=1000, height=1000, bg="transparent")
  g_f <- delete_edges(g, E(g)[!crossing(comm_list, g)])
  plot(g_f, vertex.size=log(V(g_f)$wdeg), 
       vertex.label=ifelse(degree(g_f)==0, V(g_f)$name, NA), 
       vertex.color=V(g_f)$Type_Color,
       edge.width=E(g_f)$weight/2, main=title2, 
       layout=layout_with_fr(g_f))
  dev.off()
  
  for(i in seq(comm_list)){
    subg <- induced_subgraph(g, vids=comm_list[[i]])
    fname <- paste(visuals_path,
                   title1,"-",i,'.png', sep="")
    png(filename=fname, width=1000, height=1000, bg="transparent")
    plot(subg, vertex.size=sqrt(V(subg)$wdeg), vertex.color=V(subg)$Type_Color, 
         edge.width=E(subg)$weight/2, main=paste("Community",i), 
         layout=layout_with_fr(subg))
    dev.off()
  }
}