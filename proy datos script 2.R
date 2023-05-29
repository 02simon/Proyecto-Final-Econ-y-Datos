library(tidyverse)
library(igraph)

data_97 <- WITS_Partner_Timeseries_2_
data_97 <- data_97[c("País" , "asociados", "1997", "2015", "2020" )]

#nodos 
nodos <- unique(c(data_97$País, data_97$asociados))

#links
m <- cbind()
edges_97 <- data_97[c(data_97$País, data_97$asociados, data_97$"1997")] 
