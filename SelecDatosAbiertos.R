
# Librerias ---------------------------------------------------------------
#Ciencia de datos
library(tidyverse)
library(tsibble)

#importación y lectura
library(readxl)
library(dplyr)
library(tidyr)

library(mxmaps)#para población de cada estado según la inegi 2020

# Importación de datos  ---------------------------------------------------

options(timeout = 700)
temp <- tempfile()
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", temp)


Datosmex2502 <- vroom::vroom(unz(temp, unzip(temp, list = TRUE) %>% pull(Name)))
unlink(temp)

Entidades <- read_xlsx("Datos nacionales abiertos/201128 Catalogos.xlsx",sheet="Catálogo de ENTIDADES")
# Limpieza de datos  ------------------------------------------------------
#datos necesarios para la prueba
FiltImpoData <- dplyr::select(Datosmex2502,
                              `FECHA_INGRESO`,
                              `ENTIDAD_RES`,
                              `TOMA_MUESTRA_LAB`,
                              `RESULTADO_LAB`, 
                              `TOMA_MUESTRA_ANTIGENO`,
                              `RESULTADO_ANTIGENO`, 
                              `CLASIFICACION_FINAL`,
                              `FECHA_DEF`,
)%>%
  left_join(Entidades, by=c("ENTIDAD_RES"="CLAVE_ENTIDAD"))

#Población en cada estado del país, con datos a 2020
poblacionEstado <- dplyr::select(df_mxstate_2020,
                                 `region`,
                                 `state_name`,
                                 `pop`,
)

# Tibbles generales ------------------------------------------------------------------

#datos confirmados 
confirm <- FiltImpoData %>% 
  filter(`CLASIFICACION_FINAL`%in% c(1,2,3)) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`, `FECHA_INGRESO`) %>% #borramos los datos NA que generan más filas(son pocos)
  arrange(`FECHA_INGRESO`)

#Casos terminados en muerte
muertesConfirm <- FiltImpoData %>% 
  filter(!is.na(`FECHA_DEF`)) %>% 
  drop_na(`ENTIDAD_FEDERATIVA`, `FECHA_INGRESO`) #quitamos datos NA (no interfiere)

#datos de las pruebas realizadas ese día en todo el país
filtroPrueba <- FiltImpoData %>% 
  dplyr::filter(`TOMA_MUESTRA_LAB`== 1 | `TOMA_MUESTRA_ANTIGENO`==1 ) %>% #seleccuón de datos con pruebas
  drop_na(`ENTIDAD_FEDERATIVA`, `FECHA_INGRESO`) #borrar datos NA (no afecta)



# Tibbles por estado por día ----------------------------------------------


positivosXEstaXDia <- confirm %>% 
  dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
  mutate(POSITIVOS=n()) %>%
  distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
  arrange(`FECHA_INGRESO`) %>% 
  select(`FECHA_INGRESO`,
         `ENTIDAD_RES`,
         `ENTIDAD_FEDERATIVA`,
         `FECHA_DEF`,
         `POSITIVOS`,
  )

muertesXEstaXDia <- muertesConfirm %>% 
  dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
  mutate(MUERTES=n()) %>%
  distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
  arrange(`FECHA_INGRESO`) %>% 
  select(`FECHA_INGRESO`,
         `ENTIDAD_RES`,
         `ENTIDAD_FEDERATIVA`,
         `FECHA_DEF`,
         `MUERTES`
  )

PruePosiXEstaXDia <- filtroPrueba %>% 
  dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
  mutate(PRUEBAS=n()) %>%
  distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
  arrange(`FECHA_INGRESO`) %>%
  select(`FECHA_INGRESO`,
         `ENTIDAD_RES`,
         `ENTIDAD_FEDERATIVA`,
         `FECHA_DEF`,
         `PRUEBAS`) %>% 
  left_join(positivosXEstaXDia, 
            positivosXEstaXDia, 
            by= c("ENTIDAD_RES", "FECHA_INGRESO", "ENTIDAD_FEDERATIVA")) %>% 
  PruePosiXEstaXDia$POSITIVIDAD <- (PruePosiXEstaXDia$POSITIVOS/PruePosiXEstaXDia$PRUEBAS)*100


# Tibbles por estado  -----------------------------------------------------



positivosXEstados <- confirm %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(Positivos=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  dplyr::select(
    `ENTIDAD_RES`,
    `ENTIDAD_FEDERATIVA`,
    `ABREVIATURA`,
    `Positivos`) 

muertesXEstado <- muertesConfirm %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(Muertes=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  dplyr::select(`ENTIDAD_RES`,
                `ENTIDAD_FEDERATIVA`,
                `ABREVIATURA`,
                `Muertes`) 

pruebasXEstado <- filtroPrueba  %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(Pruebas=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  dplyr::select(`ENTIDAD_RES`,  # selección de datos necesarios 
                `ENTIDAD_FEDERATIVA`,
                `ABREVIATURA`,
                `Pruebas`) 