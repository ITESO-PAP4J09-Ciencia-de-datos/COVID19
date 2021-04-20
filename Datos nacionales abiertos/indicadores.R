
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
  

# Indicadores por día en cada estado  -------------------------------------

#Por día hacemos un conteo de los casos que se confirmaron en cada estado
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
         )# %>%
  # add_column(SUMS=NA)
 
#Para generar las tablas de cada uno de los estados con su conteo 
for(i in unique(positivosXEstaXDia$`ENTIDAD_RES`)) {
  nam <- paste0("positivoE.", i )
  assign(nam, positivosXEstaXDia[positivosXEstaXDia$`ENTIDAD_RES`==i,])
  
}

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
for(i in unique(muertesXEstaXDia$`ENTIDAD_RES`)) {
  nam <- paste("muertesE", i, sep = ".")
  assign(nam, muertesXEstaXDia[muertesXEstaXDia$ENTIDAD_RES==i,])
}
pruebasXEstaXDia <- filtroPrueba %>% 
  dplyr::group_by(`ENTIDAD_RES`, `FECHA_INGRESO`) %>%
  mutate(PRUEBAS=n()) %>%
  distinct(`FECHA_INGRESO`, .keep_all = TRUE) %>%
  arrange(`FECHA_INGRESO`) %>% 
  select(`FECHA_INGRESO`,
         `ENTIDAD_RES`,
         `ENTIDAD_FEDERATIVA`,
         `FECHA_DEF`,
         `PRUEBAS`)
for(i in unique(pruebasXEstaXDia$`ENTIDAD_RES`)) {
  nam <- paste("pruebasE", i, sep = ".")
  assign(nam, pruebasXEstaXDia[pruebasXEstaXDia$ENTIDAD_RES==i,]) 
  # add_column(rollsumr("pruebasE".i$PRUEBAS, k = 14, fill = NA))
  # pruebasE.i$promedio <- rollmean(`PRUEBAS`, k = 14, fill = NA, aling="rigth")
}
for (i in tibble("pruebasE", i,sep="·")){
  tibble("pruebasE", i,sep="·")$sums <-rollsumr(PRUEBAS, k = 14, fill = NA) %>% 
  tibble("pruebasE", i,sep="·")$promedio <- rollmean(PRUEBAS, k = 14, fill = NA, aling="rigth")
}
# Indicadores por estados -------------------------------------------------------------

positivosXEstados <- confirm %>%
  group_by(`ENTIDAD_RES`) %>%
  mutate(Positivos=n()) %>%
  distinct(`ENTIDAD_RES`, .keep_all = TRUE) %>%
  arrange(`ENTIDAD_RES`) %>% 
  dplyr::select(`ENTIDAD_RES`,
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
  


# Por millon de habitantes ------------------------------------------------

posiXEstaXMillon <- ((1000000*positivosXEstados$Positivos)/poblacionEstado$pop) 


muerteXEstaXMillon <- ((1000000*muertesXEstado$Muertes)/poblacionEstado$pop)


# Positividad  ------------------------------------------------------------

PositividadIndica <- (positivosXEstados$Positivos/pruebasXEstado$Pruebas)*100
# Pruebas por mil habitantes  ---------------------------------------------

pruebasXEstaXMilhab <- ((100*pruebasXEstado$Pruebas)/poblacionEstado$pop)


# Tabla con datos finales xEstado -------------------------------------------------

indicadoresFinal <- positivosXEstados %>% 
  add_column(muertesXEstado$Muertes) %>% 
  add_column(pruebasXEstado$Pruebas) %>% 
  add_column(posiXEstaXMillon) %>% 
  add_column(muerteXEstaXMillon) %>% 
  add_column(PositividadIndica) %>% 
  add_column(pruebasXEstaXMilhab)


# Promedio movil por estado -----------------------------------------------

#se genera un promedio movil de los úlimos 14 días más actualziados para cada estado

library(zoo)
puebasMOVIL <- pruebasE.01

as.numeric(nrow(pruebasE.01)-13):as.numeric(nrow(pruebasE.01))ç

library(zoo)
positivoE.01$sums <- rollsumr(positivoE.01$POSITIVOS, k = 14, fill = NA)
pruebasE.01$promedio <- rollmean(pruebasE.01$PRUEBAS, k = 14, fill = NA, aling="rigth")


pruebasE.02$sums <- rollsumr(pruebasE.02$PRUEBAS, k = 14, fill = NA)
pruebasE.02$promedio <- rollmean(pruebasE.02$PRUEBAS, k = 14, fill = NA, aling="rigth")


pruebasE.13$sums <- rollsumr(pruebasE.13$PRUEBAS, k = 14, fill = NA)
pruebasE.13$promedio <- rollmean(pruebasE.13$PRUEBAS, k = 14, fill = NA, aling="rigth")

for(i in unique(positivosXEstaXDia$`ENTIDAD_RES`)) {
  nam <- paste("positivoE", i, sep = ".")
  data <- get(paste("positivoE",i,sep="."))
  SUM <- 
  assign(nam, positivosXEstaXDia[positivosXEstaXDia$`ENTIDAD_RES`==i,]) %>% 
    pruebasE.i$sums <-rollsumr(pruebasE.i$PRUEBAS, k = 14, fill = NA) %>% 
    pruebasE.i$promedio <- rollmean(pruebasE.i$PRUEBAS, k = 14, fill = NA, aling="rigth")
  
}

pruebasE.01 %>% 
  add_colum(rollsumr(pruebasE.01$PRUEBAS, k = 14, fill = NA)) %>% 
 add_colum(rollmean(pruebasE.01$PRUEBAS, k = 14, fill = NA, aling="rigth"))

datalist <- list.files("muertesE", i, sep = ".")

for (i in 1:32){
  datalist <- paste("positivoE", i, sep = ".")
  datalist[datalist$sums,] <-rollsumr(datalist[datalist$PRUEBAS,], k = 14, fill = NA) %>% 
    datalist[datalist$promedio,] <- rollmean(datalist[datalist$PRUEBAS,], k = 14, fill = NA, aling="rigth")
}

for (i in 1:32){
  datalist <- paste0("positivoE", i, sep = ".")
  as.list(datalist)
  datalist[["SUMS"]] <-rollsumr(datalist["POSITIVOS"], k = 14, fill = NA) 
  datalist["promedio"] <- rollmean(datalist["POSITIVOS"], k = 14, fill = NA, aling="rigth")
}

for (i in 1:32){
  datalist <- paste("positivoE", i, sep = ".")
  datalist[datalist$SUMS] <-rollsumr(datalist[datalist$POSITIVOS], k = 14, fill = NA) 
  datalist[datalist$promedio] <- rollmean(datalist[datalist$POSITIVOS], k = 14, fill = NA, aling="rigth")
}

i=01
for (i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  dataname <- paste("positivoE", i, sep = ".")
  datalist <- get(paste("positivoE", i, sep = "."))
  assign(dataname, datalist[rollsumr(datalist$POSITIVOS, k = 14, fill = NA)])
}

# datalist <- list(positivoE.01,positivoE.02)
for (i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  datalist <- paste0("positivoE.", i)
  list()
  add_column(datalist,
         SUMS = rollsumr(datalist[POSITIVOS], k = 14, fill = NA),
          # promedio <- rollmean(datalist$POSITIVOS, k = 14, fill = NA, aling="rigth")
         )
  # datalist$SUMS <-rollsumr(datalist$POSITIVOS, k = 14, fill = NA)
  # datalist$promedio <- rollmean(datalist$POSITIVOS, k = 14, fill = NA, aling="rigth")
}

for (i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  datalist <- paste0("positivoE.", i)
  datalist <- get(paste("positivoE", i, sep = "."))
  assign(datalist["SUMS"],
             rollsumr(datalist["POSITIVOS"], k = 14, fill = NA)
             # promedio <- rollmean(datalist$POSITIVOS, k = 14, fill = NA, aling="rigth")
  )
  # datalist$SUMS <-rollsumr(datalist$POSITIVOS, k = 14, fill = NA)
  # datalist$promedio <- rollmean(datalist$POSITIVOS, k = 14, fill = NA, aling="rigth")
}


for (i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  get(paste0("positivoE.", i))
  (as.name(paste0("positivoE.", i)))$SUMS <-as.name(paste0("positivoE.", i)[rollsumr((as.name(paste0("positivoE.", i)))$POSITIVOS, k = 14, fill = NA)]
  (as.name(paste0("positivoE.", i)))$promedio <- rollmean((as.name(paste0("positivoE.", i)))$POSITIVOS, k = 14, fill = NA, aling="rigth")
}

for (i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  get(paste0("positivoE.", i)) %>% 
  SUMS <-rollsumr(get(paste0("positivoE.", i))$POSITIVOS, k = 14, fill = NA)
  promedio <- rollmean(POSITIVOS, k = 14, fill = NA, aling="rigth")
}

i=0

for (i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  datalist = list(get(paste0("positivoE.", i)))
  # as.name(paste0("positivoE.", i)) <- get(paste0("positivoE.", i))
  # names <- tibble(paste0("positivoE.", i))
  datalist$SUMS <-rollsumr(datalist$POSITIVOS, k = 14, fill = NA)
  datalist$promedio <- [rollmean(datalist$POSITIVOS, k = 14, fill = NA, aling="rigth")]
  datalist=datalist+1
}


datalist=list()
for (i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  name = (get(paste0("positivoE.", i)))
  datalist <- append(datalist,name)
  # datalist$SUMS <-rollsumr(datalist$POSITIVOS, k = 14, fill = NA)
  # datalist$promedio <- rollmean(datalist$POSITIVOS, k = 14, fill = NA, aling="rigth")
  # positivoE.
}

mybiglist <- vector('list', 5)
names(mybiglist) <- paste0('item:', seq_along(mybiglist))

datalist <- list(1:32)

for(i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
  names(datalist) <- paste0('positivoE.', i)
  data <- get(paste0("positivoE.",i))
  datalist[[i]] <- data
}

#para hacer una lista de las bases de datos
datalist <- list()
for(i in unique(positivosXEstaXDia$`ENTIDAD_RES`)) {
  name <- paste0('positivoE.', i)
  data <- get(paste0('positivoE.', i))
  datalist[[name]] <- data
}

for (j in datalist){
  i <- j
  na <- paste0(j)
  i$SUMS <-rollsumr(i$POSITIVOS, k = 14, fill = NA)
  i$promedio <- rollmean(i$POSITIVOS, k = 14, fill = NA, aling="rigth")
  datalist[[na]] <- i
}

df1 <- data.frame(x = rep(3, 5), y = seq(1, 5, 1), ID = letters[1:5])
df2 <- data.frame(x = rep(5, 5), y = seq(2, 6, 1), ID = letters[6:10])

z=list(df1,df2)
df=NULL
for (i in z) {
  # i$Avg=(i$x+i$y)/2
  # df<-rbind(df,i)
  # print (df)
}

# for(i in unique(positivosXEstaXDia$`ENTIDAD_RES`)){
#   dat <- get(paste0('positivoE.', i))
#   name <- paste0("promedio14E.",i)
#   
#   sum(dat$POSITIVOS, na.rm = TRUE))
#   assign(name,dat[dat$POSITIVOS] )
#   (nam, positivosXEstaXDia[positivosXEstaXDia$`ENTIDAD_RES`==i,])
# }


length()
for(i in unique(positivosXEstaXDia$`ENTIDAD_RES`)) {
  nam <- (paste("positivoE", i, sep = "."))
  data <- get(paste0("positivoE.", i))
  SUM <- rollsumr(data$POSITIVOS, k = 14, fill = NA)
  PRO <- rollmean(data$POSITIVOS, k = 14, fill = NA, aling="rigth")
  data[nam] <- add_column(SUM)
  
}
