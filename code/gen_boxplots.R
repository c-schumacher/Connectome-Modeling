# function wrapper to generate and necessary box plots
gen_boxplots <- function(output_list, comm_metric, xnames, title='NA'){
  boxplot(output_list[,comm_metric] ~ output_list[,"Model.Alg"],
          xlab='Model', ylab=comm_metric, names=xnames, main=title,
          col=c("red", "lightblue", "yellow", "green", "orange")
  )
}
