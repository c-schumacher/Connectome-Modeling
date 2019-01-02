set_neuron_colors <- function(g){
  model <- g
  V(model)[V(model)$Type_Main == "Interneuron"]$Type_Color <- "lightblue"
  V(model)[V(model)$Type_Main == "Motor Neuron"]$Type_Color <- "pink"
  V(model)[V(model)$Type_Main == "Sensory Neuron"]$Type_Color <- "yellow"
  V(model)[V(model)$Type_Main == "Unknown"]$Type_Color <- "grey"
  return(model)
}