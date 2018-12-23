centrality_scores <- function(file, title_select="Correlation Matrix of Centrality Measures", 
                              output="stats"){
  df.dc <- degree(file, normalized=TRUE)
  wdeg <- graph.strength(file, weights=E(file)$weight)
  df.wdc <- wdeg / sqrt(sum(wdeg^2))  
  df.bet <- betweenness(file, normalized=TRUE, weights=NA) 
  #df.clo <- closeness(file, normalized=TRUE, weights=NA)
  df.pr <- page_rank(file)
  df.eig <- eigen_centrality(file, directed=TRUE)
  df.cent <- data.frame(degree=df.dc,
                        wdegree=df.wdc,
                        pr=df.pr$vector,
                        eig=df.eig$vector,
                        betw=df.bet)
  #clo=df.clo)
  if(output=="graph"){
    ggcorr(centrality_scores(som_con), label=TRUE)+labs(title=title_select)
  } else if (output == "stats"){
    return(df.cent)
  }
}
