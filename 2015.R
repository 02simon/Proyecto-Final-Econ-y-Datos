#CARGADO DE LIBRERIAS
library(tidyverse)
library(igraph)
library(dplyr)
library()

#IMPORTACION DE DATASET
Data_2015 <- Dataset.Asia.Países.2015

#CAMBIAMOS EL NOMBRE DE LAS COLUMNAS
colnames(Data_2015) <- c("Exportador","Producto/Servicio","Receptor","cantidad exportada(MU$D)")
is.numeric("cantidad exportada(MU$D)")

#LO CONVERTIMOS EN UN DATAFRAME
Data_2015 <- as.data.frame(Data_2015)
is.data.frame(Data_2015)

#REMOVEMOS LOS VALORES QUE SON 0
Data_2015Mej <- subset(Data_2015, Data_2015$"cantidad exportada(MU$D)">0) 
Data_2015Mej <- as.data.frame(Data_2015Mej)
is.data.frame(Data_2015Mej)


# CREAMOS LOS NODOS
vertices <- unique(c(edges$Exportador, edges$Receptor))

# CREAMOS LOS EDGES
edges <- Data_2015Mej[, c("Exportador", "Receptor", "cantidad exportada(MU$D)")]
edges <- as.data.frame(edges)

# COMENZAMOS A CREAR LA RED
red_2015 <- graph_from_data_frame(edges, vertices = vertices, directed = TRUE)

# PLOTEAMOS LA RED
grafico_2015 <- plot(
  red_2015,
  layout = layout.fruchterman.reingold(red_2015, repulserad = 10, niter = 1000),
  vertex.color = ifelse(V(red_2015)$name %in% edges$Exportador, "red4", "cyan3"),
  vertex.size = ifelse(V(red_2015)$name %in% edges$Exportador, 10, 5),
  edge.width = 0.1,
  edge.color = "grey35",
  edge.arrow.size = 0.25,
  vertex.label.color = "black" # Set the color of the node labels
)

edges <- as.data.frame(edges)
is.data.frame(edges)
remove(edges_names)#rem

#COMENZAMOS A CREAR LA RED
V(red_2015)
E(red_2015)

red_2015 <- graph(edges = matrix(nrow = 0, ncol = 2), directed = TRUE)
is.igraph(red_2015)
str(red_2015)

red_2015 <- add_vertices(red_2015, nv = length(vertices), name = vertices)

red_2015 <- add_edges(red_2015, edges)
E(red_2015)$weight <- edges$"cantidad exportada(MU$D)"

#PLOTEAMOS LA RED
plot(red_2015, layout = layout_on_sphere(red_2015), vertex.color = "darkorchid4",
     edge.color = "yellow", vertex.shape = "sphere", vertex.label.color = "grey34")
layout_fruchterman_reingold(red_2015)
