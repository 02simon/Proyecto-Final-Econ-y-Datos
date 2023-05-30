#Cargado de librerías
library(tidyverse)
library(igraph)
#Importamos la data
#Indexamos la tabla 
data_2019 <- Asia2019[c("País","asociados","Trade.Balance..en.miles.de.US..")]

#Creamos los nodos
nodos_19 <- unique(c(data_2019$País, data_2019$asociados))

#Creamos los links
exportaciones_19 <- data.frame(
  Exportador = data_2019$País,
  Receptor = data_2019$asociados,
  Cantidad = data_2019$Trade.Balance..en.miles.de.US..
)

edges_2019 <- data.frame(
  from = exportaciones_19$Exportador,
    to = exportaciones_19$Receptor,
    weight = exportaciones_19$Cantidad
)

#Creamos la matriz de adyacencia 
adj_matrix_2019 <- matrix(0, nrow = length(nodos_19),ncol = length(nodos_19))
for (i in 1:nrow(edges_2019)) {
  from <- match(edges_2019[i, "from"], nodos_19)
  to <- match(edges_2019[i, "to"], nodos_19)
  adj_matrix_2019[from, to] <- 1  # Solo estamos interesados en la existencia de un enlace, no en su peso
}

#Creamos la red
red_2019 <- graph_from_adjacency_matrix(adj_matrix_2019, mode = "directed")

# Asignar nombres de los nodos
V(red_2019)$name <- nodos_19

# PLOTEAMOS LA RED
grafico_2019 <- plot(
  red_2019,
  layout = layout.fruchterman.reingold(red_2019, repulserad = 10, niter = 1000),
  vertex.color = ifelse(V(red_2019)$name %in% exportaciones_19$Exportador, "red4", "cyan3"),
  vertex.size = ifelse(V(red_2019)$name %in% exportaciones_19$Exportador, 10, 5),
  edge.width = edges_2019$weight / max(edges_2019$weight),
  edge.color = "grey35",
  edge.arrow.size = 0.25,
  vertex.label.color = "black" # Set the color of the node labels
)

grafico_sin_etiqueta_2019 <- plot(
  red_2019,
  layout = layout.fruchterman.reingold(red_2019, repulserad = 10, niter = 1000),
  vertex.color = ifelse(V(red_2019)$name %in% exportaciones_19$Exportador, "red4", "cyan3"),
  vertex.size = ifelse(V(red_2019)$name %in% exportaciones_19$Exportador, 10, 5),
  edge.width = 0.1,
  edge.color = "grey35",
  edge.arrow.size = 0.25,
  vertex.label = "" # Remove the node labels
)
