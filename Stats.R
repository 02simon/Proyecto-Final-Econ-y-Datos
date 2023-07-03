#Cargado de librerias
library(tidyverse)
library(ggrepel)

                               #Ordenamiento y filtrado de datos

colnames(PBI) <- c("PBI","PAIS","CODISO")
colnames(node_data) <- c("PAIS","CENTRALIDAD")

TABLAPBI <- merge(PBI, node_data, by = "PAIS")


                         #GRAFICO DE RELACION CENTRALIDAD DE GRADO/PBI

# Lista de países específicos para agregar etiquetas
paises_etiquetas <- c("United States", "China","Tuvalu", "Japan","Singapore","Malaysia","Korea, Rep.","Thailand","India","Indonesia","Hong Kong, China")
  

  #corregimos el formato
  TABLAPBI$PBI <- gsub("\\.", "", TABLAPBI$PBI)
  # Reemplazar las comas por puntos en la columna "PBI"
  TABLAPBI$PBI <- gsub(",", ".", TABLAPBI$PBI)
  
  # Convertir la columna "PBI" a numérica
  TABLAPBI$PBI <- as.numeric(TABLAPBI$PBI)
  

  # Utilizamos ggplot para crear el gráfico  
  ggplot(data = TABLAPBI, aes(x = PBI, y = CENTRALIDAD)) +
    geom_point(size = 3, aes(color = CENTRALIDAD)) +
    scale_color_gradient(low = "#FFE659", high = "red4") +
  geom_label_repel(data = filter(TABLAPBI, PAIS %in% paises_etiquetas), aes(label = PAIS),
                   vjust = -0.01, 
                   color = "black",
                   box.padding = 0.3,
                   point.padding = 0.1, 
                   force = 1, 
                   arrow = arrow(length = unit(0.05, "cm"), type = "closed"), nudge_x = 0.3,
                   nudge_y = 0.3, 
                   max.overlaps = 12) +
    labs(title = "PBI/CENTRALIDAD",
         x = "PBI Nominal 1999 en US$", y = "Centralidad de Grado") +
    theme_minimal() +
    scale_x_log10(labels = scales::comma, expand = expansion(add = c(0.01,0.01)))
  