library(tidyverse)
library(igraph)
library(ggpubr)
library(igraph)
library(AER)
library(readxl)
#install.packages("ndtv", dependencies=T)
library(ndtv)
#install.packages("visNetwork'")
library('visNetwork') 

#Importación de datos. 
#Las tablas en excel ya están limpias.
#Top 10 destinos exportadores e importadores de cada país según volumen monetario.
Partner_Export <- read_excel("C:/Users/dolor/OneDrive - Económicas - UBA/DATOS/WITS-Partner.xlsx", sheet = "Export", )
Partner_Import <- read_excel("C:/Users/dolor/OneDrive - Económicas - UBA/DATOS/WITS-Partner.xlsx", sheet = "Import")

#Limpio la base
#Le saco un outlier
Partner_Exportx=Partner_Export[-c(20414),]
#Saco columnas que estan de más
Partner_Export_def <- Partner_Export[-c(9:33)]
Partner_Export_def1 <- Partner_Export_def[-c(3:6)]

################BASE LIMPIA###################
#Saco todos los NA values
PartnerExport <- na.omit(Partner_Export_def1)

#Limpio el resto
remove(Partner_Export)
remove(Partner_Exportx)
remove(Partner_Export_def)
remove(Partner_Export_def1)

#Creamos los nodos
#unique returns a vector, data frame or array like x but with duplicate elements/rows removed.
nodos_19<-unique(c(PartnerExport$`Reporter Name`,PartnerExport$`Partner Name`))

#Links
edges_19 <-data.frame(
  from=PartnerExport$`Reporter Name`,
  to=PartnerExport$`Partner Name`,
  weight=PartnerExport$`Export (US$ Thousand)`
)

adj.matrix.19 <-matrix(0,nrow = length(nodos_19), ncol=length(nodos_19))
for (i in 1:nrow(edges_19)) {
  from<-match(edges_19[i, "from"], nodos_19)
  to<- match(edges_19[i, "to"], nodos_19)
  adj.matrix.19[from, to]<-1
}

rownames(adj.matrix.19)=nodos_19
colnames(adj.matrix.19)=nodos_19

# Creamos la red
red_19x <- graph_from_adjacency_matrix(adj.matrix.19, mode = "directed")

#Pruebo diferentes paquetes y comandos que me permitan graficar
nodos19 <- as.data.frame(nodos_19)
visIgraph(red_19x)

##_____________________________________________________________________#
##________________________HEATMAP__________________________##
#Reconfiguro la matriz de adyacencia para poder hacer el heatmap
#La idea es sacar a todos aquellos partners que nunca son reporters
# Identificar los países que son REPORTER en la tabla Partner_Exportx
reporter_countries <- unique(PartnerExport$`Reporter Name`)

# Identificar los países que son PARTNER en la tabla Partner_Exportx
partner_countries <- unique(PartnerExport$`Partner Name`)

# Filtrar los países PARTNER que también son REPORTER
partner_reporter_countries <- partner_countries[partner_countries %in% reporter_countries]

# Filtrar la tabla Partner_Exportx para mantener solo los países PARTNER que también son REPORTER
PartnerExport_filtered <- PartnerExport[PartnerExport$`Partner Name` %in% partner_reporter_countries, ]

# Filtrar la matriz de adyacencia adj.matrix.19 para mantener solo los países PARTNER que también son REPORTER
adj.matrix.19_filtered <- adj.matrix.19[rownames(adj.matrix.19) %in% partner_reporter_countries, ]
adj.matrix.19_filtered <- adj.matrix.19_filtered[, colnames(adj.matrix.19_filtered) %in% partner_reporter_countries]

# Verificar que nodos_19 contenga los nombres en el mismo orden que adj.matrix.19_filtered
nodos_19_filtered <- nodos_19[nodos_19 %in% partner_reporter_countries]

#HEATMAP SOBRE MATRIZ DE ADYACENCIA DE 1 y 0 (el heatmap que estamos descartando)
heatmap(adj.matrix.19_filtered,
        scale= "column",
        labRow = nodos_19_filtered,
        labCol = nodos_19_filtered, Rowv = NA, Colv = NA)


#Reversión del HEATMAP
# Crea un vector único de países (reporters y partners)
all_countries <- unique(c(PartnerExport_filtered$`Partner Name`, PartnerExport_filtered$`Reporter Name`))

# Ordena los países alfabéticamente para asegurar consistencia en la matriz
all_countries <- sort(all_countries)

# Crea una matriz vacía con la dimensión nxn
n_countries <- length(all_countries)
export_matrix1 <- matrix(0, nrow = 156, ncol = 156)

# Llena la matriz con los datos de saldos exportables
for (i in 1:nrow(PartnerExport_filtered)) {
  reporter_idx <- which(all_countries == PartnerExport_filtered$`Reporter Name`[i])
  partner_idx <- which(all_countries == PartnerExport_filtered$`Partner Name`[i])
  export_value <- PartnerExport_filtered$`Export (US$ Thousand)`[i]  # Obtén el valor de la columna "Export (US$ Thousand)"
  export_matrix1[reporter_idx, partner_idx] <- export_matrix1[reporter_idx, partner_idx] + export_value
}

# Asigna nombres de países a las filas y columnas de la matriz
rownames(export_matrix1) <- colnames(export_matrix1) <- all_countries

# La matriz export_matrix contiene los saldos exportables entre cada país, heatmap sobre matriz PONDERADA
heatmap(export_matrix1, Rowv = NA, Colv = NA)



#################################################################################
# Creamos un diccionario para asignar la región a cada país
region_dict <- list(
  "África del Norte" = c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia"),
  "África Subsahariana" = c("Angola", "Botswana", "Burundi", "Cameroon", "DR Congo", "Ethiopia", "Ghana", "Kenya", "Nigeria", "Rwanda", "South Africa", "Zambia", "Zimbabwe"),
  "América del Norte" = c("Canada", "United States"),
  "América Central y el Caribe" = c("Antigua and Barbuda", "Bahamas", "Barbados", "Costa Rica", "Cuba", "Dominican Republic", "Guatemala", "Honduras", "Jamaica", "Mexico", "Panama", "Trinidad and Tobago"),
  "América del Sur" = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Peru", "Uruguay", "Venezuela"),
  "Europa Occidental" = c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom"),
  "Europa Oriental" = c("Bulgaria", "Czech Republic", "Hungary", "Poland", "Romania", "Russia", "Slovakia", "Ukraine"),
  "Europa del Sur" = c("Cyprus", "Malta"),
  "Europa del Norte" = c("Estonia", "Iceland", "Latvia", "Lithuania"),
  "Asia Central" = c("Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan"),
  "Asia Oriental" = c("China", "Japan", "South Korea", "Hong Kong, China"),
  "Asia del Sur" = c("India", "Pakistan", "Bangladesh", "Sri Lanka"),
  "Asia Sudoriental" = c("Indonesia", "Malaysia", "Philippines", "Thailand", "Vietnam"),
  "Oceanía" = c("Australia", "New Zealand"),
  "Balcanes" = c("Albania", "Belarus", "Bosnia and Herzegovina", "Croatia", "Georgia", "Kosovo", "Macedonia", "Moldova", "Montenegro", "Serbia")
) 

#Te arma una columna en tu base de datos y le asigna a cada reporter su respectiva región
PartnerExport_with_regions <- PartnerExport_filtered
PartnerExport_with_regions$RegionR <- unlist(sapply(PartnerExport_filtered$`Reporter Name`, function(country) {
  region <- names(region_dict)[which(sapply(region_dict, function(reg) country %in% reg))]
  if (length(region) == 0) region <- "Desconocida" # Si no se encuentra la región en el diccionario, asignamos "Desconocida"
  return(region)
}))

# Paso 1: Crear la columna "RegionP" en el dataframe y asignar la región correspondiente a cada "partner"
PartnerExport_with_regions$RegionP <- unlist(sapply(PartnerExport_with_regions$`Partner Name`, function(country) {
  region <- names(region_dict)[which(sapply(region_dict, function(reg) country %in% reg))]
  if (length(region) == 0) region <- "Desconocida" # Si no se encuentra la región en el diccionario, asignamos "Desconocida"
  return(region)
}))

# Paso 2: Obtener la lista de regiones reporters y partners únicas
regiones_reporters <- unique(PartnerExport_with_regions$RegionR)
regiones_partners <- unique(PartnerExport_with_regions$RegionP)

# Paso 3: Crear una matriz de ceros de tamaño nxn
n <- length(regiones_reporters)
matriz_saldos_exportables <- matrix(0, nrow = n, ncol = n)

# Paso 4: Llenar la matriz con las sumas de saldos exportables
# Paso 4: Llenar la matriz con las sumas de saldos exportables
for (j in 1:n) {
  partner <- regiones_partners[j]
  subset_data <- PartnerExport_with_regions[PartnerExport_with_regions$RegionP == partner, ]
  for (i in 1:n) {
    reporter <- regiones_reporters[i]
    suma_saldos <- sum(subset_data$`Export (US$ Thousand)`[subset_data$RegionR == reporter])
    matriz_saldos_exportables[i, j] <- suma_saldos
  }
}


# Paso 5: Asignar nombres de regiones a las filas y columnas de la matriz
rownames(matriz_saldos_exportables) <- regiones_reporters
colnames(matriz_saldos_exportables) <- regiones_partners

matrix_saldos <- matriz_saldos_exportables[-c(1),]
matrix_saldos1 <- matrix_saldos[, -3]
# Paso 6: Mostrar la matriz
print(matriz_saldos_exportables)

heatmap(matriz_saldos_exportables, Rowv = NA, Colv= NA)

library(gplots)
# Genera el heatmap
heatmap(matrix_saldos1,
        scale = "column",  # Escala las columnas (regiones partners) para visualizar diferencias
        Colv = NA,  # Sin clustering de columnas
        Rowv = NA,  # Sin clustering de filas
        margins = c(13, 13),  # Ajusta los márgenes para visualizar los nombres de ejes
        main = "Matriz de Saldos Exportables",  # Título del heatmap
        cex.main = 1.5,  # Tamaño de fuente del título
        cex.axis = 0.8,  # Tamaño de fuente de los nombres de ejes
        xlab = "Partner",  # Etiqueta para el eje x (columnas)
        ylab = "Reporter"  # Etiqueta para el eje y (filas)
)


##################################################################################################
#IMPORTACIÓN
#Importación de datos. 
#Las tablas en excel ya están limpias.
#Top 10 destinos exportadores e importadores de cada país según volumen monetario.
Partner_Export <- read_excel("C:/Users/dolor/OneDrive - Económicas - UBA/DATOS/WITS-Partner.xlsx", sheet = "Export", )
Partner_Import <- read_excel("C:/Users/dolor/OneDrive - Económicas - UBA/DATOS/WITS-Partner.xlsx", sheet = "Import")

#Limpio la base
#Le saco un outlier
Partner_Importx=Partner_Import[-c(20414),]
Partner_Importx=Partner_Import[-c(20451),]
#Saco columnas que estan de más
Partner_Import_def1 <- Partner_Importx[-c(3:6)]

################BASE LIMPIA###################
#Saco todos los NA values
PartnerImport <- na.omit(Partner_Import_def1)

#Limpio el resto
remove(Partner_Import)
remove(Partner_Importx)
remove(Partner_Import_def1)

#Creamos los nodos
#unique returns a vector, data frame or array like x but with duplicate elements/rows removed.
nodos_19_impo<-unique(c(PartnerImport$`Reporter Name`,PartnerImport$`Partner Name`))

#Links
edges_19_impo <-data.frame(
  from=PartnerImport$`Reporter Name`,
  to=PartnerImport$`Partner Name`,
  weight=PartnerImport$`Import (US$ Thousand)`
)

adj.matrix.19.impo <-matrix(0,nrow = length(nodos_19_impo), ncol=length(nodos_19_impo))
for (i in 1:nrow(edges_19_impo)) {
  from<-match(edges_19_impo[i, "from"], nodos_19_impo)
  to<- match(edges_19_impo[i, "to"], nodos_19_impo)
  adj.matrix.19.impo[from, to]<-1
}

rownames(adj.matrix.19.impo)=nodos_19_impo
colnames(adj.matrix.19.impo)=nodos_19_impo

# Creamos la red
red_19x_impo <- graph_from_adjacency_matrix(adj.matrix.19.impo, mode = "directed")

#Pruebo diferentes paquetes y comandos que me permitan graficar
nodos19 <- as.data.frame(nodos_19_impo)
visIgraph(red_19x_impo)
