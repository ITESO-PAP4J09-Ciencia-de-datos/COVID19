
# Paqueterias -------------------------------------------------------------
#Ciencia de datos
library(tidyverse)
library(lubridate) #Fechas

#importación y lectura
library(readxl)
library(dplyr)
library(tidyr)

#Tablas
library(tables)
library(data.table)
library(formattable)


# Importación de datos ----------------------------------------------------


# Datosmex2502 <- read_csv("210225COVID19MEXICO.csv")
# Descarga de datos desde la página web
fecha <- "210412"
options(timeout = 600)
temp <- tempfile()
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", temp)


Datosmex2502 <- vroom::vroom(unz(temp, paste0(fecha,"COVID19MEXICO.csv")))
unlink(temp)


Entidades <- read_xlsx("201128 Catalogos.xlsx",sheet="Catálogo de ENTIDADES")
# Clásificación  ----------------------------------------------------------

#datos necesarios para la prueba
datosimportates <- dplyr::select(Datosmex2502,`FECHA_INGRESO`,`ENTIDAD_RES`,
                                 `TOMA_MUESTRA_LAB`,`RESULTADO_LAB`, `TOMA_MUESTRA_ANTIGENO`,
                                 `RESULTADO_ANTIGENO`, `CLASIFICACION_FINAL`)%>%
  left_join(Entidades, by=c("ENTIDAD_RES"="CLAVE_ENTIDAD"))


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
  


# Agrupación de datos  ----------------------------------------------------

#Numero de pruebas por estado totales hasta la fecha de datos
pruebasXEstado <- pruebasfiltro %>%
  group_by(`ENTIDAD_FEDERATIVA`) %>%
  mutate(`Numero de pruebas`=n()) %>%
  distinct(`ENTIDAD_FEDERATIVA`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_FEDERATIVA`) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`) 

pruebasXEstado <- pruebasXEstado %>% 
  dplyr::select(
    `ENTIDAD_FEDERATIVA`,
    `Numero de pruebas`
  )

pruebasfiltro$FECHA_INGRESO <- format(pruebasfiltro$FECHA_INGRESO, "%Y-%m")


#Numero de pruebas por estado según el día 
pruebasxEstadoxDia <- pruebasfiltro %>%
  group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
  mutate(count=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>% 
  arrange(`ENTIDAD_RES`) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`) 

  
  
  
  

# Gráfica  ----------------------------------------------------------------


ggplot(data = pruebasfiltro) +
  geom_bar(mapping = aes(y = FECHA_INGRESO, fill = ABREVIATURA), position = "dodge")

# Tabla  ------------------------------------------------------------------

#Tabla que muestra el número de pruebas que se hacen por día en los estados
formattable(pruebasXEstado, #llamo datos
            align =c("l","c"), #Para alinear los datos de la tabla cada "" es una columna
            list(`ENTIDAD_FEDERATIVA` = formatter( #datos específicos
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `Numero de pruebas` = color_bar("Red") # me crea una barra roja con proporción a los datos
            )
)


