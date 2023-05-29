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


#EDA
#ventas de china segun productos
china_expo <- subset(Data_2015Mej, Exportador == "China")
china_expo
expos_china <- ggplot(china_expo, aes(x = china_expo$"Producto/Servicio" , y = china_expo$"cantidad exportada(MU$D)"))+
  geom_bar(stat = "identity", fill = "darkorchid4")+
  xlab("Producto/Servicio")+
  ylab("Cantidad exportada")+
  ggtitle("Exportaciones de China por producto")
expos_china <- expos_china + theme_classic()
expos_china
#exportaciones segun pais asiatico
datadef <- Data_2015Mej[c("Exportador","Producto/Servicio", "Receptor", "cantidad exportada(MU$D)")]
datadef <- as.data.frame(datadef)
asia_expos <- datadef %>%
  group_by(Exportador)+
  summarize(mediaexpo = mean(datadef$"cantidad exportada(MU$D)", na.rm = TRUE))


ggplot(Data_2015, aes(x = Data_2015Mej$Exportador, y = Data_2015Mej$"cantidad exportada(MU$D)"))+
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Quantity Exported") +
  ggtitle("Quantity Exported by Country")

#CREAMOS LOS NODOS
str(vertices)
vertices <- unique(c(edges$Exportador, edges$Receptor))

#CREAMOS LOS EDGES
str(edges)
edges <- Data_2015Mej[, c("Exportador", "Receptor", "cantidad exportada(MU$D)")]
edges <- data.frame(
  From = edges$Exportador,
  to = edges$Receptor,
  weight = edges$`cantidad exportada(MU$D)`)

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
