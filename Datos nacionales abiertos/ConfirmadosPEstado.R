# Paqueterias -------------------------------------------------------------
#Ciencia de datos
library(tidyverse)

#importación y lectura
library(readxl)
library(dplyr)
library(tidyr)
library(vroom)

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
library(rgeos)
library(maptools)
library(leaflet)


library("geojsonio")
library("jsonlite")




# Importación de datos ----------------------------------------------------

# Datosmex2502 <- read_csv("210225COVID19MEXICO.csv")
# Descarga de datos desde la página web
fecha <- "210412"
options(timeout = 240)
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

# Agrupación de datos  ----------------------------------------------------
#Numero de positivos por estado
positivosestado <- confirmados %>%
  group_by(`ENTIDAD_RES`) %>%
  summarise(
    count=n(),
  )

#Selección de nombre estados, por orden de codigo
nombreEstado <- Entidades %>%
  dplyr::select(`ENTIDAD_FEDERATIVA`) %>%
  slice( 1:32)

mapaPositivos <- positivosestado %>%
  add_column(nombreEstado)

# Gráfica -----------------------------------------------------------------

ggplot(data=confirmados) +
  geom_bar(mapping = aes(x=`ABREVIATURA`))

# Mapa  -------------------------------------------------------------------

# data(mapaPositivos)
# mapaPositivos$rand <- mapaPositivos$count
# mapaPositivos$region <- mapaPositivos$ENTIDAD_RES
# mxstate_choropleth(mapaPositivos,
#                    title = "Casos confirmados de COVID por estado.",
#                    legend = "Número de casos.",
# )


# Convert the topoJSON to spatial object
tmpdir <- tempdir()
# have to use RJSONIO or else the topojson isn't valid
write(RJSONIO::toJSON(mxstate.topoJSON), file.path(tmpdir, "sta.topojson"))
mxstate <- topojson_read(file.path(tmpdir, "sta.topojson")) 


#ordenamos los datos del topoJSON en orden numérico
mxstate <- mxstate[order(mxstate$id),]


mxstate <- as_Spatial(mxstate)

mxstate$rand <- mapaPositivos$count

bins <- c(5000,20000 , 30000, 35000, 50000, 60000, 115000,300000, Inf)
pal <- colorBin("YlOrRd", domain = mxstate$rand, bins=bins)


etiqueta <- paste(
  "Estado: ", mapaPositivos$ENTIDAD_FEDERATIVA, "<br/>",
  "Número de casos: ", mapaPositivos$count
) %>%
  lapply(htmltools::HTML)

leaflet(mxstate) %>%
  addPolygons(
              fillColor = ~pal(mxstate$rand),
              fillOpacity = 1,
              stroke = TRUE,
              color = "White",
              weight = 1.5, 
              dashArray = "3",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = etiqueta,
  )%>%
addLegend(pal = pal, values = ~mapaPositivos$rand, opacity = 0.7, title = "Casos<br>positivos<br>contagios",
          position = "bottomright")%>%
  addTiles() %>%
  addMarkers(50, 50) %>%
  addControl("Positivos totales COVID19 México", position = "bottomleft") %>% 
addProviderTiles("CartoDB.Positron")

