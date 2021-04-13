
# Paqueterías -------------------------------------------------------------

#to Wrangle data
library(dplyr)
library(tidyverse) 

# Para series de tiempo y forecasting. 
library(fpp3)
library(feasts)
library(fable) 
library(tsibble)
library(lubridate) 

#integrar visualización
library(patchwork)

# Carga de datos ----------------------------------------------------------

#Se importan los datos como un tibble
Vacunastotales <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")


# Wrangle data ------------------------------------------------------------


#Se quiere trabajar con series de tiempo, entonces convertimos
# a tsibble un objeto que tiene orientación a este tiempo de 
#procesamiento 
Vacunastotales_tsibble <- Vacunastotales %>%
  mutate(Daily = as.Date(date)) %>%
  select(-date) %>%
  as_tsibble(key = location,
             index = Daily)

#se hace una variable con los nombres de los paises de
#LATAM para asi poder llamar la variable a buscar en
#la base de datos si se requiere, esto esta pensado
#en que la instrucción podría hacerse varias veces 
#entonces en teoría debería simplificar el código
  latam <- c("Mexico", "Argentina", 
           "Colombia", "Chile", 
           "Brazil", "Bolivia", 
           "Costa Rica", "Ecuador",
           "Guatemala", "Panama", 
           "Paraguay", "Peru", 
           "Puerto Rico", "Dominican Republic")
  
  #Se encontro que era particularmente complicado mostrar 
  #todos los datos en una sola gráfica, por lo tanto,
  #graficar por secciones y pegar con patchwork es una
  #opción viable, por lo que la variable length(latam) = 14
  #entonces dividimos en 2 grupos para tener símetria.
  
  latam1 <- latam[1:7]
  latam2 <- latam[8:14]
#latam == latam1 + latam2
  
  
#hacemos otro dafa frame que solo sea para los de 
#LATAM y asi trabajamos con un tsibble más pequeña
Vacunas_latam_tsibble <- Vacunastotales_tsibble %>%
  select( Daily, location, total_vaccinations, 
          total_vaccinations_per_hundred, 
          daily_vaccinations_per_million) %>%
  filter(location %in% latam)


# Visualización -----------------------------------------------------------
# Visualización general ---------------------------------------------------


#Gráfica que representa el escenario general para los paises 
#de latam en el tiempo vacunados por cada 100 
EscenarioLatam <- ggplot(data = Vacunas_latam_tsibble) + 
  geom_line(mapping = aes(x = Daily, y = total_vaccinations_per_hundred, color = location)) +
  labs(x = 'meses',
         y = 'Vacunas aplicadas por cada 100')

plotly::ggplotly(EscenarioLatam) 

#Notas de el gráifco EscenarioLatam 
#muestra una tendencia creciente
#con temporalidad variable
#No hay evidencia de comportmaiento ciclico 



# #Visualización por periocidad -------------------------------------------

#Utilizando la función gg_season para hacer graficas
#de la vacunación (2 gráficas por pais correspondiente a los
# 2 años de los que se tienen datos) por mes. 

Vacunas_latam_tsibble %>%
  filter(location %in% latam1) %>%
  gg_season(total_vaccinations_per_hundred, labels = "both") +
  labs(y = "Vacunas aplicadas por cada 100",
       x = "Meses",
       title = "Vacunación por meses en los diferentes paises de LATAM") +
  expand_limits(x = ymd(c("2021-02","2021-04"))) -> g1


#se repite el codigo para hacer lo mismo y luego juntarlos 
#con el apoyo de patch work
Vacunas_latam_tsibble %>%
  filter(location %in% latam2) %>%
  gg_season(total_vaccinations_per_hundred, labels = "both") +
  labs(y = "Vacunas aplicadas por cada 100",
       x = "Meses",
       title = "Vacunación por meses en los diferentes paises de LATAM") +
  expand_limits(x = ymd(c("2021-02","2021-04"))) -> g2
#No se estiliza que la asignación vaya hasta el final
#pues transgrede con el estilo del código, pero se recomienda
#en el libro de forescasting para darle "fluidez" a la lectura
#del código

#Se encuentra interesante que en marzo la mayoría de los paises
#tienen una linea constante
#Méxio y chile empezaron la vacunación en las últimas semanas
#de diciembre 



#Aquí vemos las gráficas anteriores más a detalle, pues podemos
#ver en que semanas de cada mes hay crecimiento 
Vacunas_latam_tsibble %>% 
  filter(location %in% latam1) %>%
  gg_season(total_vaccinations_per_hundred, period = "month") +
  labs(y = "Vacunas aplicadas por cada 100",
       x = "Periodicidad de las semanas del mes",
       title = " Vacunación por semanana de los diferentes meses en los paises de LATAM") +
expand_limits(x = ymd(c("2021-02","2021-04"))) -> g3


#repetimos el código para la sección 2
Vacunas_latam_tsibble %>% 
  filter(location %in% latam2) %>%
  gg_season(total_vaccinations_per_hundred, period = "month") +
  labs(y = "Vacunas aplicadas por cada 100",
       x = "Periodicidad de las semanas del mes",
       title = " Vacunación por semanana de los diferentes meses en los paises de LATAM") + 
expand_limits(x = ymd(c("2021-02","2021-04"))) -> g4


# Visualización: Integración de los gráficos con PATCHWORK  -----------------------------

#Establecemos un layout, que es basicamente un # para los espacios en blanco, y letras 
#para los lugares que deseamos que ocupe la letra
layout <- '
##AAAA##
BBCCDDEE
BBCCDDEE
'
#cambiamos el lugar de las letras en el layout por nuestrras gráficas
wrap_plots(A = EscenarioLatam,
           B = g1,
           C = g2, 
           D = g3, 
           E = g4, 
           design = layout)
