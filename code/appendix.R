# ---- C1 ----
library(gridExtra)
library(ggplot2)
library(GGally)
library(sand)

#setting up directory
visuals_path <- 'C:/Users/Charlie/Desktop/Grad School Docs/Fall 2018/DSC 480/Final Project/FinalVisuals/'
path <- "C:/Users/Charlie/Desktop/Grad School Docs/Fall 2018/DSC 480/Final Project"
setwd(path)

# setting up the base model
edge.df <- read.csv("connectome_edges - somatic only - directed (full).csv")
node.df <- read.csv("connectome_vertices - somatic only (full).csv")
som_con <- graph.data.frame(edge.df, vertices=node.df, directed=TRUE)
summary(som_con)

# Removing any vertices where there are nodes with edge weights of 0 (disconnected neurons)
som_con <- delete_edges(som_con, E(som_con)[E(som_con)$weight < 1])
som_con <- delete_vertices(som_con, V(som_con)$name[degree(som_con)<1])
write_graph(som_con, "som_con_dir_279_neurons.graphml", format="graphml")

# ---- C2 ---- 
# Creating connectome models for various mutants
som_con.AIB <- delete_vertices(som_con, 
                               V(som_con)[V(som_con)$name %in% c("AIBR", "AIBL")])
write_graph(som_con.AIB, "som_con_dir_minus_AIB.graphml", format="graphml")

som_con.AIY <- delete_vertices(som_con, 
                               V(som_con)[V(som_con)$name %in% c("AIYR", "AIYL")])
write_graph(som_con.AIY, "som_con_dir_minus_AIY.graphml", format="graphml")

som_con.RIM <- delete_vertices(som_con, 
                               V(som_con)[V(som_con)$name %in% c("RIMR", "RIML")])
write_graph(som_con.RIM, "som_con_dir_minus_RIM.graphml", format="graphml")

som_con.RIB <- delete_vertices(som_con, 
                               V(som_con)[V(som_con)$name %in% c("RIBR", "RIBL")])
write_graph(som_con.RIB, "som_con_dir_minus_RIB.graphml", format="graphml")


# ---- C3 ---- 
# Examining the overall degree distribution for the connectome 
degree_dist(som_con, title_select="Degree Distribution of Somatic Nervous System")
degree_dist(som_con.AIB, 
            title_select="Degree Distribution of Somatic Nervous System (AIB Ablated)")
degree_dist(som_con.AIY, 
            title_select="Degree Distribution of Somatic Nervous System (AIY Ablated)")
degree_dist(som_con.RIM, 
            title_select="Degree Distribution of Somatic Nervous System (RIM Ablated)")
degree_dist(som_con.RIB, 
            title_select="Degree Distribution of Somatic Nervous System (RIB Ablated)")
boxplot(degree(som_con), degree(som_con.AIB), degree(som_con.AIY),
        degree(som_con.RIM), degree(som_con.RIB),
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Degree", main="Degree Distribution by Model")

# ---- C4 ---- 
# Examining the overall degree distribution by neuron class
class_deg_dist(som_con, pos="dodge",
               title_select="Degree Distribution of Somatic Nervous System by Neuron Class",
               ymax=50)
class_deg_dist(som_con.AIB, pos="dodge",
               title_select="Degree Distribution of Somatic Nervous System by 
               Neuron Class (AIB Ablated)",
               ymax=50)
class_deg_dist(som_con.AIY, pos="dodge",
               title_select="Degree Distribution of Somatic Nervous System by 
               Neuron Class (AIY Ablated)",
               ymax=50)
class_deg_dist(som_con.RIM, pos="dodge",
               title_select="Degree Distribution of Somatic Nervous System by 
               Neuron Class (RIM Ablated)",
               ymax=50)
class_deg_dist(som_con.RIB, pos="dodge",
               title_select="Degree Distribution of Somatic Nervous System by 
               Neuron Class (RIB Ablated)",
               ymax=50)
# boxplot of the interneuron degree distribution by ablated model compared to base 
boxplot(degree(som_con)[V(som_con)$Type_Main=="Interneuron"],
        degree(som_con.AIB)[V(som_con.AIB)$Type_Main=="Interneuron"],
        degree(som_con.AIY)[V(som_con.AIY)$Type_Main=="Interneuron"],
        degree(som_con.RIM)[V(som_con.RIM)$Type_Main=="Interneuron"],
        degree(som_con.RIB)[V(som_con.RIB)$Type_Main=="Interneuron"],
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Degree", 
        col=c("red", "lightblue", "yellow", "green", "orange"),
        main="Degree Distribution by Model (Interneurons)")
# boxplot of the motor degree distribution by ablated model compared to base 
boxplot(degree(som_con)[V(som_con)$Type_Main=="Motor Neuron"],
        degree(som_con.AIB)[V(som_con.AIB)$Type_Main=="Motor Neuron"],
        degree(som_con.AIY)[V(som_con.AIY)$Type_Main=="Motor Neuron"],
        degree(som_con.RIM)[V(som_con.RIM)$Type_Main=="Motor Neuron"],
        degree(som_con.RIB)[V(som_con.RIB)$Type_Main=="Motor Neuron"],
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Degree", 
        col=c("red", "lightblue", "yellow", "green", "orange"),
        main="Degree Distribution by Model (Motor Neurons)")
# boxplot of the sensory degree distribution by ablated model compared to base 
boxplot(degree(som_con)[V(som_con)$Type_Main=="Sensory Neuron"],
        degree(som_con.AIB)[V(som_con.AIB)$Type_Main=="Sensory Neuron"],
        degree(som_con.AIY)[V(som_con.AIY)$Type_Main=="Sensory Neuron"],
        degree(som_con.RIM)[V(som_con.RIM)$Type_Main=="Sensory Neuron"],
        degree(som_con.RIB)[V(som_con.RIB)$Type_Main=="Sensory Neuron"],
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Degree", 
        col=c("red", "lightblue", "yellow", "green", "orange"),
        main="Degree Distribution by Model (Sensory Neurons)")


# ---- C5 ---- 
# Examining the overall weighted degree distribution 
degree_dist(som_con, title_select="Weighted Degree Distribution of 
            Somatic Nervous System", weighted=TRUE)
degree_dist(som_con.AIB, 
            title_select="WeightedDegree Distribution of Somatic 
            Nervous System (AIB Ablated)", weighted=TRUE)
degree_dist(som_con.AIY, 
            title_select="Degree Distribution of Somatic 
            Nervous System (AIY Ablated)", weighted=TRUE)
degree_dist(som_con.RIM, 
            title_select="Degree Distribution of Somatic 
            Nervous System (RIM Ablated)", weighted=TRUE)
degree_dist(som_con.RIB, 
            title_select="Degree Distribution of Somatic 
            Nervous System (RIB Ablated)", weighted=TRUE)
boxplot(graph.strength(som_con), graph.strength(som_con.AIB), graph.strength(som_con.AIY),
        graph.strength(som_con.RIM), graph.strength(som_con.RIB),
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Weighted Degree", main="Weighted Degree Distribution by Model")


# ---- C6 ---- 
# Examining the weighted degree distribution by class
class_deg_dist(som_con, pos="dodge",
               title_select="Weighted Degree Distribution of Somatic Nervous System by Neuron Class",
               ymax=50, weighted=TRUE, xtick=25, bwidth=15)
class_deg_dist(som_con.AIB, pos="dodge",
               title_select="Weighted Degree Distribution of Somatic Nervous System by 
               Neuron Class (AIB Ablated)",
               ymax=50, weighted=TRUE, xtick=25)
class_deg_dist(som_con.AIY, pos="dodge",
               title_select="Weighted Degree Distribution of Somatic Nervous System by 
               Neuron Class (AIY Ablated)",
               ymax=50, weighted=TRUE, xtick=25)
class_deg_dist(som_con.RIM, pos="dodge",
               title_select="Weighted Degree Distribution of Somatic Nervous System by 
               Neuron Class (RIM Ablated)",
               ymax=50, weighted=TRUE, xtick=25)
class_deg_dist(som_con.RIB, pos="dodge",
               title_select="Weighted Degree Distribution of Somatic Nervous System by 
               Neuron Class (RIB Ablated)",
               ymax=50, weighted=TRUE, xtick=25)
# boxplot of the interneuron degree distribution by ablated model compared to base 
boxplot(graph.strength(som_con)[V(som_con)$Type_Main=="Interneuron"],
        graph.strength(som_con.AIB)[V(som_con.AIB)$Type_Main=="Interneuron"],
        graph.strength(som_con.AIY)[V(som_con.AIY)$Type_Main=="Interneuron"],
        graph.strength(som_con.RIM)[V(som_con.RIM)$Type_Main=="Interneuron"],
        graph.strength(som_con.RIB)[V(som_con.RIB)$Type_Main=="Interneuron"],
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Degree", 
        col=c("red", "lightblue", "yellow", "green", "orange"),
        main="Weighted Degree Distribution by Model (Interneurons)")
# boxplot of the motor degree distribution by ablated model compared to base 
boxplot(graph.strength(som_con)[V(som_con)$Type_Main=="Motor Neuron"],
        graph.strength(som_con.AIB)[V(som_con.AIB)$Type_Main=="Motor Neuron"],
        graph.strength(som_con.AIY)[V(som_con.AIY)$Type_Main=="Motor Neuron"],
        graph.strength(som_con.RIM)[V(som_con.RIM)$Type_Main=="Motor Neuron"],
        graph.strength(som_con.RIB)[V(som_con.RIB)$Type_Main=="Motor Neuron"],
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Weighted Degree", 
        col=c("red", "lightblue", "yellow", "green", "orange"),
        main="Weighted Degree Distribution by Model (Motor Neurons)")
# boxplot of the sensory degree distribution by ablated model compared to base 
boxplot(graph.strength(som_con)[V(som_con)$Type_Main=="Sensory Neuron"],
        graph.strength(som_con.AIB)[V(som_con.AIB)$Type_Main=="Sensory Neuron"],
        graph.strength(som_con.AIY)[V(som_con.AIY)$Type_Main=="Sensory Neuron"],
        graph.strength(som_con.RIM)[V(som_con.RIM)$Type_Main=="Sensory Neuron"],
        graph.strength(som_con.RIB)[V(som_con.RIB)$Type_Main=="Sensory Neuron"],
        names=c("Base", "AIB Ablated", "AIY Ablated", "RIM Ablated", "RIB Ablated"),
        xlab="Model", ylab="Weighted Degree", 
        col=c("red", "lightblue", "yellow", "green", "orange"),
        main="Weighted Degree Distribution by Model (Sensory Neurons)")


# ---- C7 ---- 
# Edge weight distribution by each connectome model
edgewt_dist(som_con, title_select="Edge Weight Distribution (Base)")
edgewt_dist(som_con.AIB, title_select="Edge Weight Distribution (AIB Ablated)")
edgewt_dist(som_con.AIY, title_select="Edge Weight Distribution (AIY Ablated)")
edgewt_dist(som_con.AIB, title_select="Edge Weight Distribution (RIM Ablated)")
edgewt_dist(som_con.AIY, title_select="Edge Weight Distribution (RIB Ablated)")


# ---- C8 ---- 
# Centrality scores for each model
centrality_scores(som_con, output="graph", 
                  title_select="Correlation Matrix of Centrality Metrics (Full Connectome)")
centrality_scores(som_con.AIB, output="graph", 
                  title_select="Correlation Matrix of Centrality Metrics (AIB Ablated)")
centrality_scores(som_con.AIY, output="graph", 
                  title_select="Correlation Matrix of Centrality Metrics (AIY Connectome)")
centrality_scores(som_con.RIM, output="graph", 
                  title_select="Correlation Matrix of Centrality Metrics (RIM Ablated)")
centrality_scores(som_con.RIB, output="graph", 
                  title_select="Correlation Matrix of Centrality Metrics (RIB Connectome)")


# ---- C9 ----
# Detecting communities in the network models using weighted walktrap
som_con.wtcomm <- cluster_walktrap(som_con, steps=6, weights=E(som_con)$weight)
som_con.AIB.wtcomm <- cluster_walktrap(som_con.AIB, steps=6, weights=E(som_con.AIB)$weight)
som_con.AIY.wtcomm <- cluster_walktrap(som_con.AIY, steps=6, weights=E(som_con.AIY)$weight)
som_con.RIM.wtcomm <- cluster_walktrap(som_con.RIM, steps=6, weights=E(som_con.RIM)$weight)
som_con.RIB.wtcomm <- cluster_walktrap(som_con.RIB, steps=6, weights=E(som_con.RIB)$weight)

# setting up a dataframe of community membership. Ablated neuron communities are 0 
commnames <- c("som_con.wtcomm", "som_con.AIB.wtcomm", "som_con.AIY.wtcomm",
               "som_con.RIM.wtcomm","som_con.RIB.wtcomm")
comms <- data.frame(matrix(ncol=5, nrow=length(V(som_con)$name)))
row.names(comms) <- V(som_con)$name
colnames(comms) <- commnames
for(i in V(som_con)$name){
  for(j in names(comms)){
    comms[i,j] <- membership(get(j))[i] 
  }
}
comms <- data.frame(comms)
comms[is.na(comms)] <- 0     # representing ablated neurons as community 0


# ---- C10 ---- 
# Updating the base model to contain various attributes and communitie of ablated versions
som_con <- update_vertex_attrs(som_con)
som_con <- add_vertex_comms(som_con, comms)
V(som_con)$nameAIBshift.wt <- ifelse(V(som_con)$som_con.wtcomm != V(som_con)$som_con.AIB.wtcomm,
                                     V(som_con)$name, NA)
V(som_con)$nameAIYshift.wt <- ifelse(V(som_con)$som_con.wtcomm != V(som_con)$som_con.AIY.wtcomm,
                                     V(som_con)$name, NA)
V(som_con)$nameRIMshift.wt <- ifelse(V(som_con)$som_con.wtcomm != V(som_con)$som_con.RIM.wtcomm,
                                     V(som_con)$name, NA)
V(som_con)$nameRIBshift.wt <- ifelse(V(som_con)$som_con.wtcomm != V(som_con)$som_con.RIB.wtcomm,
                                     V(som_con)$name, NA)

# base model
write_graph(som_con, "base_plus_ablated_comms.graphml", format="graphml")

# ablated models
som_con.AIB <- update_vertex_attrs(som_con.AIB)
som_con.AIB <- add_vertex_comms(som_con.AIB, comms, "AIB")
write_graph(som_con.AIB, "aib_ablated.graphml", format="graphml")

som_con.AIY <- update_vertex_attrs(som_con.AIY)
som_con.AIY <- add_vertex_comms(som_con.AIY, comms, "AIY")
write_graph(som_con.AIY, "aiy_ablated.graphml", format="graphml")

som_con.RIM <- update_vertex_attrs(som_con.RIM)
som_con.RIM <- add_vertex_comms(som_con.RIM, comms, "RIM")
write_graph(som_con.RIM, "rim_ablated.graphml", format="graphml")

som_con.RIB <- update_vertex_attrs(som_con.RIB)
som_con.RIB <- add_vertex_comms(som_con.RIB, comms, "RIB")
write_graph(som_con.RIB, "rib_ablated.graphml", format="graphml")


# ---- C11 ----
# Percent of Weights in Each Connectome Model - PLOT
wttab_somcon <- tabulate(E(som_con)$weight) / ecount(som_con)
wttab_somcon_df <- data.frame(weight = seq(1, max(E(som_con)$weight)),
                              Freq=wttab_somcon, Model='Full Connectome')

wttab_somcon.AIB <- tabulate(E(som_con.AIB)$weight) / ecount(som_con.AIB)
wttab_somcon.AIB_df <- data.frame(weight = seq(1, max(E(som_con.AIB)$weight)),
                                  Freq=wttab_somcon.AIB, Model='AIB Ablated')

wttab_somcon.AIY <- tabulate(E(som_con.AIY)$weight) / ecount(som_con.AIY)
wttab_somcon.AIY_df <- data.frame(weight = seq(1, max(E(som_con.AIY)$weight)),
                                  Freq=wttab_somcon.AIY, Model='AIY Ablated')

wttab_somcon.RIM <- tabulate(E(som_con.RIM)$weight) / ecount(som_con.RIM)
wttab_somcon.RIM_df <- data.frame(weight = seq(1, max(E(som_con.RIM)$weight)),
                                  Freq=wttab_somcon.RIM, Model='RIM Ablated')

wttab_somcon.RIB <- tabulate(E(som_con.RIB)$weight) / ecount(som_con.RIB)
wttab_somcon.RIB_df <- data.frame(weight = seq(1, max(E(som_con.RIB)$weight)),
                                  Freq=wttab_somcon.RIB, Model='RIB Ablated')

wttab_df <- rbind(wttab_somcon_df, wttab_somcon.AIB_df, wttab_somcon.AIY_df,
                  wttab_somcon.RIM_df, wttab_somcon.RIB_df)
wttab_df <- wttab_df[wttab_df$Freq > 0,]

g <- ggplot(wttab_df, aes(x=weight, y=Freq, color=Model))
g <- g + geom_point(alpha=0.5) + geom_smooth(se=F)
g <- g + scale_y_log10()
#g <- g + scale_x_log10()
g <- g + labs(x="Edge Weight", y="Log of Frequency", title="Frequency of Edge Weights")
print(g)


# ---- C12 ----
# bar plot of of EDGE DENSITY
mods <- list(som_con.AIB, som_con.AIY, som_con.RIB, som_con.RIM)
res_ed.df <- data.frame(Edge.Density=sapply(mods, edge_density),
                        Model=c( "AIB Ablated", "AIY Ablated",
                                 "RIB Ablated", "RIM Ablated"))
g <- ggplot(res_ed.df, aes(x=Model, y=Edge.Density-edge_density(som_con), label=Model,
                           fill=Model))
g <- g + geom_col()
g <- g + labs(x="Model", y="Delta Edge Density", 
              title="Edge Density of Mutant Models Relative to Base Model")
print(g)
## interesting to note that AIY has a higher edge density than full connectome


# ---- C12 ----
# bar plot of of TRANSITIVITY
mods <- list(som_con.AIB, som_con.AIY, som_con.RIB, som_con.RIM)
res_t.df <- data.frame(Transitivity=sapply(mods, transitivity),
                       Model=c( "AIB Ablated", "AIY Ablated",
                                "RIB Ablated", "RIM Ablated"))
g <- ggplot(res_t.df, aes(x=Model, y=Transitivity-transitivity(som_con), label=Model,
                          fill=Model))
g <- g + geom_col()
g <- g + labs(x="Model", y="Delta Transitivity", 
              title="Transitivity of Mutant Models Relative to Base Model")
print(g)


# ---- C13 ---- 
# Function for weighted and directed modularity. automatically computes walktrap comms
mods <- list(som_con.AIB, som_con.AIY, som_con.RIB, som_con.RIM)
res_mod.df <- data.frame(Modularity=sapply(mods, modularity_dir_wt),
                         Model=c( "AIB Ablated", "AIY Ablated",
                                  "RIB Ablated", "RIM Ablated"))
g <- ggplot(res_mod.df, aes(x=Model, y=Modularity-modularity_dir_wt(som_con), 
                            label=Model, fill=Model))
g <- g + geom_col()
g <- g + labs(x="Model", y="Delta Modularity", 
              title="Modularity of Mutant Models Relative to Base Model")
print(g)


# ---- C14 ---- 
# Plotting different communities
# setting neuron type colors for plotting
som_con <- set_neuron_colors(som_con)
som_con.AIB <- set_neuron_colors(som_con.AIB)
som_con.AIY <- set_neuron_colors(som_con.AIY)
som_con.RIB <- set_neuron_colors(som_con.RIB)
som_con.RIM <- set_neuron_colors(som_con.RIM)

set.seed(0)
parOrig <- par()
lo <- layout_with_fr(som_con)
lo.aib <- lo[-which(V(som_con)$name %in% c("AIBR", "AIBL")), ]
lo.aiy <- lo[-which(V(som_con)$name %in% c("AIYR", "AIYL")), ]
lo.rib <- lo[-which(V(som_con)$name %in% c("RIBR", "RIBL")), ]
lo.rim <- lo[-which(V(som_con)$name %in% c("RIMR", "RIML")), ]


##### generating plots of whole network, communities, and between communities

# Full somatic - walktrap - weighted comm detection
w_community_plots(som_con, som_con.wtcomm, lo, "Base Model",
                  "Base Model - Between Community Connections")
# AIB ablated - walktrap - weighted comm detection
w_community_plots(som_con.AIB, som_con.AIB.wtcomm, lo.aib, "AIB Ablated",
                  "AIB Ablated - Between Community Connections")
# AIY ablated - walktrap - weighted comm detection
w_community_plots(som_con.AIY, som_con.AIY.wtcomm, lo.aiy, "AIY Ablated ",
                  "AIY Ablated - Between Community Connections")
# RIB ablated - walktrap - weighted comm detection
w_community_plots(som_con.RIB, som_con.RIB.wtcomm, lo.rib, "RIB Ablated ",
                  "RIB Ablated - Between Community Connections")
# RIM ablated - walktrap - weighted comm detection
w_community_plots(som_con.RIM, som_con.RIM.wtcomm, lo.rim, "RIM Ablated",
                  "RIM Ablated - Between Community Connections")


# ---- C15 ---- 

# Saving communities metrics
som_con.cmetrics <- communities_metrics(som_con, som_con.wtcomm)
som_con.AIB.cmetrics <- communities_metrics(som_con.AIB, som_con.AIB.wtcomm)
som_con.AIY.cmetrics <- communities_metrics(som_con.AIY, som_con.AIY.wtcomm)
som_con.RIB.cmetrics <- communities_metrics(som_con.RIB, som_con.RIB.wtcomm)
som_con.RIM.cmetrics <- communities_metrics(som_con.RIM, som_con.RIM.wtcomm)

all_model_cmetrics <- rbind(som_con.cmetrics, som_con.AIB.cmetrics, 
                            som_con.AIY.cmetrics, som_con.AIY.cmetrics,
                            som_con.RIB.cmetrics, som_con.RIM.cmetrics)
all_model_cmetrics
#write.csv(all_model_cmetrics, "community metrics - all models.csv")


# ---- C16 ----
# Box plots to compare the different community metrics
model_list <- list(som_con, som_con.AIB, som_con.AIY, som_con.RIB, som_con.RIM)
model_names <- list("Base", "AIB Ablated", "AIY Ablated","RIB Ablated", "RIM Ablated")


# Comparing distribution of vertex count in communities 
gen_boxplots(all_model_cmetrics, "Vertex.Count", 
             xnames=model_names, title='Distribution of Neuron Count in Communities')

# Comparing distribution of edge density  in communities 
gen_boxplots(all_model_cmetrics, "Edge.Density", 
             xnames=model_names, title='Distribution of Synaptic Density in Communities')

# Comparing distribution of transitivity in communities 
gen_boxplots(all_model_cmetrics, "Transitivity", 
             xnames=model_names, title='Distribution of Structural Transitivity in Communities')

# Comparing distribution of longest shortest paths in communities 
gen_boxplots(all_model_cmetrics, "Diameter", 
             xnames=model_names, title='Distribution of Longest Geodesic-Paths within Communities')

# Comparing distribution of longest shortest paths in communities 
gen_boxplots(all_model_cmetrics, "Avg.Path.Length", 
             xnames=model_names, title='Distribution of Average Path Length within Communities')


# ---- C17 ----
# Metrics for whole graphs
all_model_gmetrics <- rbind(graph_metrics(som_con, som_con.wtcomm, E(som_con)$weight),
                            graph_metrics(som_con.AIB, som_con.AIB.wtcomm, 
                                          E(som_con.AIB)$weight),
                            graph_metrics(som_con.AIY, som_con.AIY.wtcomm, 
                                          E(som_con.AIY)$weight),
                            graph_metrics(som_con.RIB, som_con.RIB.wtcomm, 
                                          E(som_con.RIB)$weight),
                            graph_metrics(som_con.RIM, som_con.RIM.wtcomm, 
                                          E(som_con.RIM)$weight))
all_model_gmetrics
#write.csv(all_model_gmetrics, "graph metrics - all models.csv")


# ---- C18 ---- 
# Generating matrices of community assignment between models to show shifts

# base vs AIB membership
community_table(comms, "som_con.wtcomm", "Base", "som_con.AIB.wtcomm", "AIB")

# base vs AIY membership
community_table(comms, "som_con.wtcomm", "Base", "som_con.AIY.wtcomm", "AIY")

# base vs RIB membership
community_table(comms, "som_con.wtcomm", "Base", "som_con.RIB.wtcomm", "RIB")

# base vs RIM membership
community_table(comms, "som_con.wtcomm", "Base", "som_con.RIM.wtcomm", "RIM")


# ---- C19 ----
#base_shuff <- shuffle_edges_mod_dir_wt(som_con, 1000)
#save(base_shuff, file="base - edge rewire sims.Rdata")
load("base - edge rewire sims.Rdata")
base_shuff.df <- data.frame(Iter=seq(1,1000), Modularity=base_shuff)

# ---- C20 ---- 
# Plotting the randomly rewired graphs compared to our models 
g <- ggplot(base_shuff.df, aes(Modularity))
g <- g + geom_histogram(col='white')
g <- g + labs(x="Modularity", y="Count", 
              title="Directed & Weighted Modularity in Simulated Connectomes")
g <- g +  scale_x_continuous(breaks=seq(0.2, 0.5, by=0.05))
g <- g + geom_vline(xintercept=modularity_dir_wt(som_con), color="red", size=2) # base model
g <- g + geom_vline(xintercept=modularity_dir_wt(som_con.AIB), color="yellow", size=2) #AIB ablated
g <- g + geom_vline(xintercept=modularity_dir_wt(som_con.AIY), color="orange", size=2) #AIY ablated
g <- g + geom_vline(xintercept=modularity_dir_wt(som_con.RIB), color="darkgreen", size=2) #RIB ablated
g <- g + geom_vline(xintercept=modularity_dir_wt(som_con.RIM), color="purple", size=2) #RIM ablated
print(g)

