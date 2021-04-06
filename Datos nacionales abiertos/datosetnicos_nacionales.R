# Librerias ---------------------------------------------------------------

library(tidyverse) #To Wrangle data 
library(patchwork) #Para poner unir gráficas en una sola visualización
library(readxl) #Leer los excels de la base de datos abierta
                #Del gobierno de México 


#library(data.table)
#library(formattable)
#library(tables)


library(reactable) # Paquete para crear tablas reactivas [1]
library(htmltools) #  [1]
library(lubridate) #Para gestionar tiempos y dias [1] 
library(hrbrthemes) #fuentes de una publicación que hizo el economista poniendo datos de covid en una tabla interactiva y estética [1]
# [1] https://towardsdatascience.com/recreate-publication-quality-interactive-tables-in-r-using-reactable-187407bc9702



# Importación de datos tibble ----------------------------------------------

data_raw <- read_csv("Datos nacionales abiertos/210131COVID19MEXICO.csv")
etiqueta_SINO <- read_xlsx("Datos nacionales abiertos/201128 Catalogos.xlsx", sheet = "Catalogo SI_NO")
etiqueta_NACIONALIDAD <- read_xlsx("Datos nacionales abiertos/201128 Catalogos.xlsx", sheet = "Catalogo NACIONALIDAD")
etiqueta_TPACIENTE <- read_xlsx("Datos nacionales abiertos/201128 Catalogos.xlsx", sheet = "Catalogo TIPO_PACIENTE")
etiqueta_SEXO <- read_xlsx("Datos nacionales abiertos/201128 Catalogos.xlsx", sheet = "Catalogo SEXO")

# Wrangled Data -------------------------------------------------------------
data_raw <- data_raw %>% 
  mutate(PAIS_ORIGEN = as.double(PAIS_ORIGEN))

data_raw %>% glimpse()

data_etnica <- data_raw %>% 
  select('SEXO', 'INTUBADO',
         'HABLA_LENGUA_INDIG', 'TIPO_PACIENTE',
         'NACIONALIDAD','INDIGENA','EDAD',
         'PAIS_ORIGEN', 'MIGRANTE', 'UCI') %>%
  pivot_longer(
    cols = c(
      'HABLA_LENGUA_INDIG',
      'INDIGENA',
      'PAIS_ORIGEN'),
    names_to = 'ETNICOS',
    values_to = 'CONTEOx'
  ) %>%
pivot_longer(
  cols = c(
    'INTUBADO',
    'UCI'),
  names_to =  'RESPUESTA_ETNICOS',
  values_to = 'CONTEOy'
  ) %>%
  left_join(etiqueta_SINO, by=c('CONTEOx' = 'CLAVE')) %>%
  left_join(etiqueta_SINO, by=c('CONTEOy' = 'CLAVE')) %>%
  left_join(etiqueta_TPACIENTE, by=c('TIPO_PACIENTE' = 'CLAVE')) %>%
  left_join(etiqueta_NACIONALIDAD, by=c('NACIONALIDAD' = 'CLAVE'))



#data_etnica <- rename(data_etnica, HLI_Exp = DESCRIPCIÓN.x , INTUBADO_Exp = DESCRIPCIÓN.y )

data_edad_Rtable_raw <- filter(data_raw, is.na(FECHA_DEF)) 
data_edad_Rtable_raw <- filter(data_raw, TIPO_PACIENTE %in% c(2)) 
data_edad_Rtable_raw <- filter(data_raw, CLASIFICACION_FINAL %in% c(1,2,3))

data_edad_Rtable_tidy <- select(data_edad_Rtable_raw, 'EDAD',
                                'CLASIFICACION_FINAL',
                                'TIPO_PACIENTE',
                                'UCI', 'INTUBADO',
                                'FECHA_DEF')

MuertesTotales <- length(which(!is.na(data_edad_Rtable_raw$FECHA_DEF)))

#data_edad_Rtable_final <- data_edad_Rtable_raw %>%
 # group_by(EDAD) %>%
  #summarise(
  #SintomaticosHospitalizacion =  ,
  #HospitalizadosUCI = ,
  #RiesgoMortalidadInfeccion = 
   #  )
 
         
data_edad <- select(data_raw,'CLASIFICACION_FINAL' , 
                         'EDAD','INTUBADO', 'UCI', 
                         'SEXO', 'TIPO_PACIENTE',
                    'FECHA_DEF') %>%
  left_join(etiqueta_SINO, by=c('INTUBADO' = 'CLAVE')) %>%
  left_join(etiqueta_SINO, by = c('UCI' = 'CLAVE')) %>%
  left_join(etiqueta_SEXO, by=c('SEXO' = 'CLAVE')) %>%
  left_join(etiqueta_TPACIENTE, by=c('TIPO_PACIENTE' = 'CLAVE')) %>%
  
  pivot_longer(
    cols = c(
    'UCI',
    'INTUBADO'),
    names_to = 'RESPUESTA_EDAD',
    values_to = 'CONTEOy'
  )

#data_edad <- rename(data_edad, INTUBADO_Exp = DESCRIPCIÓN)

data_edad$EDAD <- cut(data_edad$EDAD , breaks = seq(0,100,by = 10), right = TRUE)
data_edad %>%
  group_by(EDAD) %>%
tally()


#Visualización y gráficas ---------------------------------------------------------

#Para los datos étnicos 

data_etnica %>% glimpse()
g1 <- data_etnica %>% 
  mutate_at(
    .vars = c("ETNICOS", "DESCRIPCIÓN.y", "DESCRIPCIÓN.x.x", "DESCRIPCIÓN.y.y"),
    .funs = as_factor
  ) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = ETNICOS, fill = DESCRIPCIÓN.y)) +
  facet_grid(DESCRIPCIÓN.x.x ~ DESCRIPCIÓN.y.y)

#Para edad y sexo

 data_edad %>% glimpse()
g2 <- data_edad %>% mutate_at(
    .vars = c("EDAD" , "DESCRIPCIÓN.y", "DESCRIPCIÓN.x.x", "DESCRIPCIÓN.y.y"),
    .funs = as_factor
  ) %>%
  ggplot() +
  geom_bar(mapping = aes( x = EDAD, fill = DESCRIPCIÓN.y)) +
  facet_grid(DESCRIPCIÓN.x.x ~ DESCRIPCIÓN.y.y)

#Tabla interactiva 


#integración de la visualización 

g3 = g1 + g2
 

