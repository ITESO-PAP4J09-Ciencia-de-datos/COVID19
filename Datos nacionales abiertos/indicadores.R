
# Librerias  --------------------------------------------------------------

#Ciencia de datos
library(tidyverse)
library(tsibble)

#importación y lectura
library(readxl)
library(dplyr)
 library(tidyr)
# library(lubridate) #manejo de fechas


#summarise data
# library(skimr)

#Tablas
# library(data.table)
# library(formattable)
# library(tables)
library(mxmaps) #datos de méxico, INEGI y ubicación para mapa

# Importación de datos ----------------------------------------------------

# Descarga de datos desde la página web
# fecha <- "210415"
options(timeout = 700)
temp <- tempfile()
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", temp)


Datosmex2502 <- vroom::vroom(unz(temp, unzip(temp, list = TRUE) %>% pull(Name)))
unlink(temp)

Entidades <- read_xlsx("Datos nacionales abiertos/201128 Catalogos.xlsx",sheet="Catálogo de ENTIDADES")


# Selección de datos ------------------------------------------------------

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

# Filtro de datos en tibbles ---------------------------------------------------------

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
  


# Medias moviles de los estados casos positivos -----------------------------------------------------

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

#promedio de los últimos catorce días
positivosXEstaXDia %>% 
  group_by(ENTIDAD_FEDERATIVA) %>% 
  slice_tail(n = 14) %>% 
  summarise(Promedio = mean(POSITIVOS))

#media movil de 14 días
positivos_tsbl <- positivosXEstaXDia %>% 
  ungroup() %>% 
  as_tsibble(index = FECHA_INGRESO, key = ENTIDAD_FEDERATIVA) %>% 
  mutate(
    `14-MA` = slider::slide_dbl(POSITIVOS, mean,
                               .before = 14, .complete = TRUE)
  )
#gráfica de los positivos con la medi movil
positivos_tsbl %>% 
  feasts::autoplot(POSITIVOS) +
  geom_line(aes(y = `14-MA`), color = "black") +
  facet_wrap(~ ENTIDAD_FEDERATIVA, scales = "free_y") + 
  theme(legend.position = "none")


# Medias moviles de los estados casos negativos ---------------------------

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


#promedio de los últimos catorce días
muertesXEstaXDia %>% 
  group_by(ENTIDAD_FEDERATIVA) %>% 
  slice_tail(n = 14) %>% 
  summarise(Promedio = mean(MUERTES))

#media movil de 14 días
muertes_tsbl <- muertesXEstaXDia %>% 
  ungroup() %>% 
  as_tsibble(index = FECHA_INGRESO, key = ENTIDAD_FEDERATIVA) %>% 
  mutate(
    `14-MA` = slider::slide_dbl(MUERTES, mean,
                                .before = 14, .complete = TRUE)
  )
#gráfica de los positivos con la medi movil
muertes_tsbl %>% 
  feasts::autoplot(MUERTES) +
  geom_line(aes(y = `14-MA`), color = "black") +
  facet_wrap(~ ENTIDAD_FEDERATIVA, scales = "free_y") + 
  theme(legend.position = "none")


# medias movil positivos por millon de habitantes -------------------------

positivosXEstaXDiaXmillon <- positivosXEstaXDia %>% 
  left_join(poblacionEstado, by=c("ENTIDAD_RES"="region"))
positivosXEstaXDiaXmillon$POSITIVOS <- (positivosXEstaXDiaXmillon$POSITIVOS*1000000)/positivosXEstaXDiaXmillon$pop
  
#media movil de 14 días
positivosmillon_tsbl <- positivosXEstaXDiaXmillon %>% 
  ungroup() %>% 
  as_tsibble(index = FECHA_INGRESO, key = ENTIDAD_FEDERATIVA) %>% 
  mutate(
    `14-MA` = slider::slide_dbl(POSITIVOS, mean,
                                .before = 14, .complete = TRUE)
  )

# media movil muertes por millon de habitantes ----------------------------

muertesXEstaXDiaXmillon <- muertesXEstaXDia %>% 
  left_join(poblacionEstado, by=c("ENTIDAD_RES"="region"))
muertesXEstaXDiaXmillon$MUERTES <- (muertesXEstaXDiaXmillon$MUERTES*1000000)/muertesXEstaXDiaXmillon$pop

#media movil de 14 días
muertesmillon_tsbl <- muertesXEstaXDiaXmillon %>% 
  ungroup() %>% 
  as_tsibble(index = FECHA_INGRESO, key = ENTIDAD_FEDERATIVA) %>% 
  mutate(
    `14-MA` = slider::slide_dbl(MUERTES, mean,
                                .before = 14, .complete = TRUE)
  )


# media movil de la positividad -------------------------------------------
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
   left_join(positivosXEstaXDia, positivosXEstaXDia, by= c("ENTIDAD_RES", "FECHA_INGRESO", "ENTIDAD_FEDERATIVA"))

PruePosiXEstaXDia$POSITIVIDAD <- (PruePosiXEstaXDia$POSITIVOS/PruePosiXEstaXDia$PRUEBAS)*100

#media movil de 14 días
positivdad_tsbl <- PruePosiXEstaXDia %>% 
  ungroup() %>% 
  as_tsibble(index = FECHA_INGRESO, key = ENTIDAD_FEDERATIVA) %>% 
  mutate(
    `14-MA` = slider::slide_dbl(POSITIVIDAD, mean,
                                .before = 14, .complete = TRUE)
  )

# Media movil de pruebas por cada 1000 habitantes --------------------------

pruebasXEstaXDia<- filtroPrueba %>% 
  dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
  mutate(PRUEBAS=n()) %>%
  distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
  arrange(`FECHA_INGRESO`) %>%
  select(`FECHA_INGRESO`,
         `ENTIDAD_RES`,
         `ENTIDAD_FEDERATIVA`,
         `FECHA_DEF`,
         `PRUEBAS`) %>% 
  left_join(poblacionEstado, by=c("ENTIDAD_RES"="region"))

pruebasXEstaXDia$XMILHAB <- ((1000*pruebasXEstaXDia$PRUEBAS)/pruebasXEstaXDia$pop)

#media movil de 14 días
pruebas_tsbl <- pruebasXEstaXDia %>% 
  ungroup() %>% 
  as_tsibble(index = FECHA_INGRESO, key = ENTIDAD_FEDERATIVA) %>% 
  mutate(
    `14-MA` = slider::slide_dbl(XMILHAB, mean,
                                .before = 14, .complete = TRUE)
  ) 

# Indicadores por día en cada estado  -------------------------------------

# #Por día hacemos un conteo de los casos que se confirmaron en cada estado
# positivosXEstaXDia <- confirm %>%
#   dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
#   mutate(POSITIVOS=n()) %>%
#   distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
#   arrange(`FECHA_INGRESO`) %>%
#   select(`FECHA_INGRESO`,
#          `ENTIDAD_RES`,
#          `ENTIDAD_FEDERATIVA`,
#          `FECHA_DEF`,
#          `POSITIVOS`,
#          )# %>%
#   # add_column(SUMS=NA)
# 
# #Para generar las tablas de cada uno de los estados con su conteo
# for(i in unique(positivosXEstaXDia$`ENTIDAD_RES`)) {
#   nam <- paste0("positivoE.", i )
#   assign(nam, positivosXEstaXDia[positivosXEstaXDia$`ENTIDAD_RES`==i,])
# 
# }
# 
# muertesXEstaXDia <- muertesConfirm %>% 
#   dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
#   mutate(MUERTES=n()) %>%
#   distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
#   arrange(`FECHA_INGRESO`) %>% 
#   select(`FECHA_INGRESO`,
#          `ENTIDAD_RES`,
#          `ENTIDAD_FEDERATIVA`,
#          `FECHA_DEF`,
#          `MUERTES`
#          )
# for(i in unique(muertesXEstaXDia$`ENTIDAD_RES`)) {
#   nam <- paste("muertesE", i, sep = ".")
#   assign(nam, muertesXEstaXDia[muertesXEstaXDia$ENTIDAD_RES==i,])
# }
# pruebasXEstaXDia <- filtroPrueba %>% 
#   dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
#   mutate(PRUEBAS=n()) %>%
#   distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
#   arrange(`FECHA_INGRESO`) %>% 
#   select(`FECHA_INGRESO`,
#          `ENTIDAD_RES`,
#          `ENTIDAD_FEDERATIVA`,
#          `FECHA_DEF`,
#          `PRUEBAS`)
# for(i in unique(pruebasXEstaXDia$`ENTIDAD_RES`)) {
#   nam <- paste("pruebasE", i, sep = ".")
#   assign(nam, pruebasXEstaXDia[pruebasXEstaXDia$ENTIDAD_RES==i,]) 
#   # add_column(rollsumr("pruebasE".i$PRUEBAS, k = 14, fill = NA))
#   # pruebasE.i$promedio <- rollmean(`PRUEBAS`, k = 14, fill = NA, aling="rigth")
# }
# for (i in tibble("pruebasE", i,sep="·")){
#   tibble("pruebasE", i,sep="·")$sums <-rollsumr(PRUEBAS, k = 14, fill = NA) %>% 
#   tibble("pruebasE", i,sep="·")$promedio <- rollmean(PRUEBAS, k = 14, fill = NA, aling="rigth")
# }
# Promedio al día indicadores por estados -------------------------------------------------------------

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

# #promedios positivos al día en cada estado
# positivosXEstaXDia <- positivosXEstaXDia %>% 
#   ungroup() %>% 
#   group_by(`ENTIDAD_RES`) %>% 
#   mutate(
#     PROM=mean(POSITIVOS)
#     
#   ) 


muertesXEstado <- muertesConfirm %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(Muertes=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  dplyr::select(`ENTIDAD_RES`,
                `ENTIDAD_FEDERATIVA`,
                `ABREVIATURA`,
                `Muertes`) 

# #promedios de muertes al día en cada estado
# muertesXEstaXDia <- muertesXEstaXDia %>% 
#   ungroup() %>% 
#   group_by(`ENTIDAD_RES`) %>% 
#   mutate(
#     PROM=mean(MUERTES)
#     
#   ) 


pruebasXEstado <- filtroPrueba  %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(Pruebas=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  dplyr::select(`ENTIDAD_RES`,  # selección de datos necesarios 
                `ENTIDAD_FEDERATIVA`,
                `ABREVIATURA`,
                `Pruebas`) 
  


# Por millon de habitantes ------------------------------------------------

posiXEstaXMillon <- ((1000000*positivosXEstados$Positivos)/poblacionEstado$pop) 


muerteXEstaXMillon <- ((1000000*muertesXEstado$Muertes)/poblacionEstado$pop)


# Positividad  ------------------------------------------------------------

PositividadIndica <- (positivosXEstados$Positivos/pruebasXEstado$Pruebas)*100
# Pruebas por mil habitantes  ---------------------------------------------

pruebasXEstaXMilhab <- ((1000*pruebasXEstado$Pruebas)/poblacionEstado$pop)


# Tabla con datos finales xEstado -------------------------------------------------

indicadoresFinal <- positivosXEstados %>% 
  add_column(muertesXEstado$Muertes) %>% 
  add_column(pruebasXEstado$Pruebas) %>% 
  add_column(posiXEstaXMillon) %>% 
  add_column(muerteXEstaXMillon) %>% 
  add_column(PositividadIndica) %>% 
  add_column(pruebasXEstaXMilhab)

indicadoresFinal <- indicadoresFinal %>% 
  ungroup() %>% 
  group_by(`ENTIDAD_FEDERATIVA`) %>% 
  mutate(
    SUM= sum(`Positivos`,
             `muertesXEstado$Muertes`,
             posiXEstaXMillon,
             muerteXEstaXMillon,
             PositividadIndica,
             pruebasXEstaXMilhab,
             na.rm = TRUE),
    PROM = (SUM/6)
      ) 

PromIndica <- indicadoresFinal %>% 
  dplyr::select(`ENTIDAD_RES`,
                `ENTIDAD_FEDERATIVA`,
                `PROM`)
summary(PromIndica)

# Normalización  ----------------------------------------------------------

library(caret)


preproc2 <- preProcess(PromIndica[,c(1:3)], method=c("range"))

norm2 <- predict(preproc2, PromIndica[,c(1:3)])

summary(norm2)


normalize <- function(x) {
  return (((x - min(x))*(100) / (max(x) - min(x))))
}
PromIndica$PROM_NORM <- normalize(PromIndica$PROM)

