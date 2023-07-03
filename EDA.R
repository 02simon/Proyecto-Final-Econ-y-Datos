# EDA, cargado de librerias
library(igraph)
library(tidyverse)

                                #Ordenamiento y filtrado de datos

# Filtrar las columnas necesarias
eda99 <- Dataset.definitivo2 %>% select(Reporter.Name, Partner.Name, Export..US..Thousand., Import..US..Thousand.)
eda99 <- na.omit(eda99)

# Reemplazar las comas por puntos en la columna "expo"
eda99$Export..US..Thousand. <- gsub(",",".", eda99$Export..US..Thousand.)
# Convertir la columna "expo" a numérica
eda99$Export..US..Thousand. <- as.numeric(eda99$Export..US..Thousand.)
# Reemplazar las comas por puntos en la columna "impo"
eda99$Import..US..Thousand. <- gsub(",",".", eda99$Import..US..Thousand.)
# Convertir la columna "impo" a numérica
eda99$Import..US..Thousand. <- as.numeric(eda99$Import..US..Thousand.)


# Calcular la columna de suma de importaciones y exportaciones
eda99 <- na.omit(eda99)
eda99 <- eda99 %>% mutate(TotalComerciadoEnMilesDeUSD = Export..US..Thousand. + Import..US..Thousand.)

# Borramos las filas que contengan regiones
eda99 <- subset(eda99, eda99$Partner.Name != " World")
eda99 <- subset(eda99, eda99$Partner.Name != "East Asia & Pacific")
eda99 <- subset(eda99, eda99$Partner.Name != "Europe & Central Asia")
eda99 <- subset(eda99, eda99$Partner.Name != "Latin America & Caribbean")
eda99 <- subset(eda99, eda99$Partner.Name != "Middle East & North Africa")
eda99 <- subset(eda99, eda99$Partner.Name != "North America")


                              #GRAFICOS DE TOTAL COMERCIADO

# Calcular la suma de importaciones y exportaciones por Reporter.Name
tabla99 <- eda99 %>%
  group_by(Reporter.Name) %>%
  summarise(TotalComerciadoEnMilesDeUSD = sum(Export..US..Thousand. + Import..US..Thousand.))

# Imprimir la tabla nueva

print(tabla99)



# Paises seleccionados manualmente
paises_seleccionados <- c("China","Singapore","Korea, Rep.","India","Malaysia","Japan","Hong Kong, China","Indonesia")

# Filtrar la tabla según los países seleccionados
tabla_filtradaa <- tabla99 %>% filter(Reporter.Name %in% paises_seleccionados)

colnames(total19) <- c("Reporter.Name","2019")




graficoo <- ggplot(tabla_filtradaa, aes(x = Reporter.Name, y = TotalComerciadoEnMilesDeUSD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Comerciado en Miles de USD por País",
       x = "País", y = "Total Comerciado en Miles de USD") +
  scale_y_continuous(labels = scales::comma)

print(graficoo)



# Reemplazar las comas por puntos en la columna "expo"
total19$Total.Comerciado.US..Thousand <- gsub(",",".", total19$Total.Comerciado.US..Thousand)
# Convertir la columna "expo" a numérica
total19$Total.Comerciado.US..Thousand <- as.numeric(total19$Total.Comerciado.US..Thousand)


# Unir las tablas por la columna Reporter.Name
totalcomerciadodef <- merge(total19, tabla_filtradaa, by = "Reporter.Name")

colnames(totalcomerciadodef) <- c("Reporter.Name","2019","1999")

# Convertir los datos a formato largo
datos_largos2 <- tidyr::pivot_longer(totalcomerciadodef, cols = -Reporter.Name, names_to = "Año", values_to = "TotalComerciado")


graficof <- ggplot(datos_largos2, aes(x = Reporter.Name, y = TotalComerciado, fill = Año)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Totales Comerciados por País",
       x = "País", y = "Total Comerciado") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("blue4", "red4"))

print(graficof)





                                    #BALANZA COMERCIAL

BDP <- BDP.FINISHED
BDP <- BDP.FINISHED[c("year", "China" ,"Japan","Korea..Republic.of")]
colnames(BDP) <- c("year", "China" ,"Japan","Republic of Korea")
#PLOTEAMOS LA BALANZA DE PAGOS DE CHINA A LO LARGO DE LOS AÑOS
ggplot(BDP, aes(x = year, y = China)) +
  geom_line() +
  xlab("Año") +
  ylab("Balance de Pagos") +
  ggtitle("Balance de Pagos de los países a lo largo de los años") +
  theme_minimal()


#AGREGAMOS LOS DEMAS PAISES

BDP_long <- BDP %>%
  gather(key = "Pais", value = "Balance", -year)

# Definir una paleta de colores personalizada
colores <- c("#E41A1C", "#377EB8", "#FF7F00", "#984EA3")

ggplot(BDP_long, aes(x = year, y = Balance, color = Pais)) +
  geom_line(linewidth = 1) +
  xlab("Año") +
  ylab("Balanza comercial (en miles de USD)") +
  ggtitle("Balanza comercial de los países a lo largo de los años") +
  scale_color_manual(values = colores) +
  labs(color = "Países") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 1),
        legend.position = "right") +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10))


