# Paqueterias -------------------------------------------------------------
#Ciencia de datos
library(tidyverse)

#serie de datos
library(tsibble)
library(lubridate) #Fechas

#importación y lectura
library(readxl)
library(dplyr)
library(tidyr)

#summarise data
library(skimr)

#Mapas
library(leaflet)
library(ggmap) # -> para obtener lon y lat de los municipios
library(raster)
library(spData)
library(tmap)
library(RJSONIO)
library(tmaptools)
library(Hmisc)
library(mxmaps)
library(sf)
library(scales) # needed for comma


# Importación de datos ----------------------------------------------------

#Datosmex2502 <- read_csv("210225COVID19MEXICO.csv")
# Descarga de datos desde la página web
fecha <- "210414"
options(timeout = 600)
temp <- tempfile()
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", temp)


Datosmex2502 <- vroom::vroom(unz(temp, paste0(fecha,"COVID19MEXICO.csv")))
unlink(temp)


Entidades <- read_xlsx("Datos nacionales abiertos/201128 Catalogos.xlsx",sheet="Catálogo de ENTIDADES")

# Clasificación de datos  -------------------------------------------------

#datos necesarios para la prueba
datosimportates <- dplyr::select(Datosmex2502,`FECHA_INGRESO`,`ENTIDAD_RES`,
                          `TOMA_MUESTRA_LAB`,`RESULTADO_LAB`, `TOMA_MUESTRA_ANTIGENO`,
                          `RESULTADO_ANTIGENO`, `CLASIFICACION_FINAL`)%>%
  left_join(Entidades, by=c("ENTIDAD_RES"="CLAVE_ENTIDAD"))



#datos confirmados sin realización de pruebas
confirmados <- datosimportates %>% 
  filter(`CLASIFICACION_FINAL`%in% c(1,2,3)) %>% 
  dplyr::select(`FECHA_INGRESO`, `ENTIDAD_RES`, `ENTIDAD_FEDERATIVA`, `ABREVIATURA`) %>% 
  mutate(
    year = lubridate::year(FECHA_INGRESO),
    month = lubridate::month(FECHA_INGRESO),
    day = lubridate::day(FECHA_INGRESO)
  ) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`, `FECHA_INGRESO`) 

#datos de las pruebas realizadas ese día en todo el país
pruebasfiltro <- datosimportates %>% 
  dplyr::filter(`TOMA_MUESTRA_LAB`== 1 | `TOMA_MUESTRA_ANTIGENO`==1 ) %>% 
  dplyr::select(`FECHA_INGRESO`, `ENTIDAD_RES`,`ENTIDAD_FEDERATIVA`,`ABREVIATURA`) %>% 
  mutate(
    year = lubridate::year(FECHA_INGRESO),
    month = lubridate::month(FECHA_INGRESO),
    day = lubridate::day(FECHA_INGRESO)
  ) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`, `FECHA_INGRESO`) 


#Separación de datos por fechas para mapas
pruebas2020 <- dplyr::filter(pruebasfiltro, year==2020)
pruebEstado2020 <- pruebas2020 %>% 
  group_by(`ENTIDAD_RES`) %>% 
  summarise(
    count=n()
  )

pruebas2021 <- dplyr::filter(pruebasfiltro, year==2021)
pruebEstado2021 <- pruebas2021 %>% 
  group_by(`ENTIDAD_RES`) %>% 
  summarise(
    count=n()
  )
#confirmados por año para mapas
confirm2020 <- confirmados %>% 
  dplyr::filter( year==2020) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`)
confirmEstado2020 <- confirm2020 %>% 
  group_by(`ENTIDAD_RES`) %>% 
  summarise(
    count=n()
  )

confirm2021 <- confirmados %>% 
  dplyr::filter( year==2021) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`)
confirmEstado2021 <- confirm2021 %>% 
  group_by(`ENTIDAD_RES`) %>% 
  summarise(
    count=n()
    
  )


#Numero de pruebas por estado totales hasta la fecha de datos
pruebasXEstado <- pruebasfiltro %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(PRUEBAS=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  drop_na() 


# #Numero de pruebas por estado según el día 
# pruebasxEstadoxDia <- pruebasfiltro %>%
#   group_by(`ENTIDAD_RES`,`FECHA_INGRESO`) %>%
#   mutate(count=n()) %>%
#   distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>% 
#   arrange(`ENTIDAD_RES`) %>% 
#   drop_na() 
#   
# 
#  prubeasXEstadotsbl <- pruebasxEstadoxDia %>%
#    as_tsibble( key = `ENTIDAD_RES`,
#               index = `FECHA_INGRESO`
#    )

# group_split(pruebasxEstadoxDia)
# group_keys(pruebasxEstadoxDia)

#Positivos por estado totales hasta la fecha de datos
positivoxEstado <- confirmados %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(CONFIRMADOS=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  dplyr::select(ENTIDAD_RES, ENTIDAD_FEDERATIVA, CONFIRMADOS )

# #Positivos por estado según el día 
# positivoxEstadoxDia <- confirmados %>%
#   group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
#   mutate(count=n()) %>%
#   distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>% 
#   arrange(`ENTIDAD_RES`) %>% 
#   drop_na()
# 
# positivoXDiatsbl <- positivoxEstadoxDia %>%
#   as_tsibble( key = ENTIDAD_RES,
#              index = FECHA_INGRESO
#              
#   )


#Selección de nombre estados, por orden de codigo
nombreEstado <- Entidades %>%
  dplyr::select(`ENTIDAD_FEDERATIVA`) %>%
  slice( 1:32)
  


# Agrupación de datos totales -----------------------------------------------------

# #suma total de las pruebas realizadas
# totalpruebas <- pruebasXEstado$PRUEBAS %>%
#   sum(na.rm = TRUE)

#suma total de las pruebas que salieron positivas
totalpositivas <- positivoxEstado$CONFIRMADOS %>%
  sum(na.rm = TRUE)

#Porcentaje por estado de las pruebas positivas a el total de pruebas realizadas en los estados
positividadPais <- (totalpositivas/totalpruebas)*100
positividadPais

positividad <- ((positivoxEstado$CONFIRMADOS/pruebasXEstado$PRUEBAS)*100)
positividad

#porcentaje total de las pruebas positivas de acuerdo a que estado. 
porcenestado <- (positivoxEstado$CONFIRMADOS/totalpositivas)*100
porcenestado <- as.numeric(porcenestado) 
porcenestado

#Porcentaje total de pruebas positvas
porcen <- sum(positividad, na.rm = TRUE) 

#verificación de suma de porcentaje de pruebas positivas (porcenestado)
sumporcentaje <- sum(porcenestado, na.rm = TRUE)

# creamos tibble con datos de codigo de entidad y casos positivos
nueva <- positivoxEstado %>% 
  #agregamos porcentajes de acuerdo al total de pruebas positivas
  add_column(porcenestado)%>% 
  #agregamos porcentajes del total de pruebas
  add_column(positividad) %>% 
  add_column(pruebasXEstado$PRUEBAS)
  # #Agregamos el nombre de los estados por orden de codigo
  # add_column(nombreEstado) 

# Agrupación de datos 2020 ------------------------------------------------

# #suma total de las pruebas realizadas
# totalpruebas2020 <- pruebEstado2020$count %>%
#   sum(na.rm = TRUE)

#suma total de las pruebas que salieron positivas
totalpositivas2020 <- confirmEstado2020$count %>%
  sum(na.rm = TRUE)

#Porcentaje por estado de las pruebas positivas a el total de pruebas realizadas en los estados
positividad2020 <- (confirmEstado2020$count/pruebEstado2020$count)*100
positividad2020

#porcentaje total de las pruebas positivas de acuerdo a que estado. 
porcenestado2020 <- (confirmEstado2020$count/totalpositivas2020)*100
porcenestado2020 <- as.numeric(porcenestado) 
porcenestado2020

#Porcentaje total de pruebas positvas
porcen2020 <- sum(positividad2020, na.rm = TRUE) 

#verificación de suma de porcentaje de pruebas positivas (porcenestado)
sumporcentaje2020 <- sum(porcenestado2020, na.rm = TRUE)

# creamos tibble con datos de codigo de entidad y casos positivos
nueva2020 <- confirmEstado2020 %>% 
  #agregamos porcentajes de acuerdo al total de pruebas positivas
  add_column(porcenestado2020)%>% 
  #agregamos porcentajes del total de pruebas
  add_column(positividad2020) %>% 
  #Agregamos el nombre de los estados por orden de codigo
  add_column(nombreEstado)

# Agrupación de datos 2021 ------------------------------------------------
# #suma total de las pruebas realizadas
# totalpruebas2021 <- pruebEstado2021$count %>%
#   sum(na.rm = TRUE)

#suma total de las pruebas que salieron positivas
totalpositivas2021 <- confirmEstado2021$count %>%
  sum(na.rm = TRUE)

#Porcentaje por estado de las pruebas positivas a el total de pruebas realizadas en los estados
positividad2021 <- (confirmEstado2021$count/pruebEstado2021$count)*100
positividad2021

#porcentaje total de las pruebas positivas de acuerdo a que estado. 
porcenestado2021 <- (confirmEstado2021$count/totalpositivas2021)*100
porcenestado2021 <- as.numeric(porcenestado2021) 
porcenestado2021

#Porcentaje total de pruebas positvas
porcen2021 <- sum(positividad2021, na.rm = TRUE) 

#verificación de suma de porcentaje de pruebas positivas (porcenestado)
sumporcentaje2021 <- sum(porcenestado2021, na.rm = TRUE)

# creamos tibble con datos de codigo de entidad y casos positivos
nueva2021 <- confirmEstado2021 %>% 
  #agregamos porcentajes de acuerdo al total de pruebas positivas
  add_column(porcenestado2021)%>% 
  #agregamos porcentajes del total de pruebas
  add_column(positividad2021) %>% 
  #Agregamos el nombre de los estados por orden de codigo
  add_column(nombreEstado)

# Mapa de positividad total --------------------------------------------------------------------

# de acuerdo al número de pruebas realizadas se calcula el porcentaje de las 
#pruebas que fueron seleccionadas como positivas. (por estado)
data(nueva)
nueva$value <- nueva$positividad
nueva$region <- nueva$ENTIDAD_RES
# mxstate_choropleth(nueva,
#                    num_colors = 1,
#                    title = "Porcentaje de casos positivos",
#                    legend = "%",
# )

#Mapa interactivo
bins = c(15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72)
pal <- colorBin("viridis", domain = nueva$value, bins=bins)
mxstate_leaflet(nueva,
                pal,
                ~ pal(value),
                ~ sprintf("Estado: %s<br/>Porcentaje de positividad : %s",
                         ENTIDAD_FEDERATIVA , comma(value)  )) %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            values = nueva$value,
            title = "Percentaje<br>Positividad",
            labFormat = labelFormat(suffix = "%",
                                    )) %>%
  addTiles() %>%
  addMarkers(50, 50) %>%
  addControl("Mapa positividad de las pruebas totales", position = "bottomleft") %>%
  addProviderTiles("CartoDB.Positron")


 # Mapa 2020 ---------------------------------------------------------------
# de acuerdo al número de pruebas realizadas se calcula el porcentaje de las 
#pruebas que fueron seleccionadas como positivas. (por estado del año 2020)
data(nueva2020)
nueva2020$value <- nueva2020$positividad2020
nueva2020$region <- nueva2020$ENTIDAD_RES
# mxstate_choropleth(nueva,
#                    num_colors = 1,
#                    title = "Porcentaje de casos positivos",
#                    legend = "%",
# )

#Mapa interactivo
bins=c(15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72)
pal <- colorBin("viridis", domain = nueva2020$value, bins=bins)
mxstate_leaflet(nueva2020,
                pal,
                ~ pal(value),
                ~ sprintf("Estado: %s<br/>Porcentaje de positividad : %s",
                          ENTIDAD_FEDERATIVA , comma(value)  )) %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            values = nueva2020$value,
            title = "Percentaje<br>Positividad",
            labFormat = labelFormat(suffix = "%",
            )) %>%
  addTiles() %>%
  addMarkers(50, 50) %>%
  addControl("Mapa positividad de las pruebas en 2020", position = "bottomleft") %>% 
  addProviderTiles("CartoDB.Positron")


# Mapa 2021 ---------------------------------------------------------------
# de acuerdo al número de pruebas realizadas se calcula el porcentaje de las 
#pruebas que fueron seleccionadas como positivas. (por estado del año 2021)
data(nueva2021)
nueva2021$value <- nueva2021$positividad2021
nueva2021$region <- nueva2021$ENTIDAD_RES
# mxstate_choropleth(nueva2021,
#                    num_colors = 1,
#                    title = "Porcentaje de casos positivos",
#                    legend = "%",
# )

#Mapa interactivo
bins = c(15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72)
pal <- colorBin("viridis", domain = nueva2021$value, bins=bins)
mxstate_leaflet(nueva2021,
                pal,
                ~ pal(value),
                ~ sprintf("Estado: %s<br/>Porcentaje de positividad : %s",
                          ENTIDAD_FEDERATIVA , comma(value)  )) %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            values = nueva2021$value,
            title = "Percentaje<br>Positividad",
            labFormat = labelFormat(suffix = "%",
            )) %>%
  addTiles() %>%
  addMarkers(50, 50) %>%
  addControl("Mapa positividad de pruebas en 2021", position = "bottomleft") %>% 
  addProviderTiles("CartoDB.Positron")

