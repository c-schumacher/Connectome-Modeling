community_table <- function(community_df, c1, c1_name, c2, c2_name){
  dev.off()
  grid.table(table(community_df[,c(c1, c2)]),
             rows=c(paste(c1_name,'-C1'), paste(c1_name,'-C2'),
                    paste(c1_name,'-C3'), paste(c1_name,'-C4'),
                    paste(c1_name,'-C5'), paste(c1_name,'-C6')),
             cols=c('Ablated', paste(c2_name,'-C1'), paste(c2_name,'-C2'),
                    paste(c2_name,'-C3'), paste(c2_name,'-C4'),
                    paste(c2_name,'-C5'), paste(c2_name,'-C6')))
}