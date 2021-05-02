
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
  dplyr::mutate(Daily = as.Date(date)) %>%
  dplyr::select(-date) %>%
  tsibble::as_tsibble(key = location,
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
  
  
#hacemos otro data frame que solo sea para los de 
#LATAM y asi trabajamos con un tsibble más pequeña
Vacunas_latam_tsibble <- Vacunastotales_tsibble %>%
  dplyr::select( Daily, location, total_vaccinations, 
          total_vaccinations_per_hundred, 
          daily_vaccinations_per_million) %>%
  filter(location %in% latam)

#Tratando los valores faltantes y los que estan fuera de rango

#VLT = contracción para Vacunas_latam_tsibble
VLT_miss <- Vacunas_latam_tsibble %>% 
  #filter(location %in% latam1) %>%
  #anti_join(outliers) %>%
  fill_gaps() #aqui se remplazan por valores faltantes
  #fill(direction = "down")

#A continuacion hacemos un modelo ARIMA que se ajuste
#a los datos que cotienen "valores faltantes"

VLT_fill <- VLT_miss %>% 
  model(ARIMA(total_vaccinations_per_hundred)) %>%
  interpolate(VLT_miss)


### Visualización -----------------------------------------------------------
# Visualización general ---------------------------------------------------


#Gráfica que representa el escenario general para los paises 
#de latam en el tiempo vacunados por cada 100 

EscenarioLatam <- ggplot(data = Vacunas_latam_tsibble) + 
  geom_line(mapping = aes(x = Daily, y = total_vaccinations_per_hundred, color = location)) +
  labs(title = 'Escenario general de vacunación en LATAM ',
       x = 'meses',
       y = 'Vacunas aplicadas por cada 100')

#Gráfica que representa el escenario general para los paises 
#de latam en el tiempo vacunados por cada 100 (rellenado)

EscenarioLatam_fill <- ggplot(data = VLT_fill) +
  geom_line(mapping = aes(x = Daily, y = total_vaccinations_per_hundred, color = location)) +
  labs(title = 'Escenario general de vacunación en LATAM (sin valores faltantes)',
       x = 'meses',
       y = 'Vacunas aplicadas por cada 100')

EscenarioLatam_Comparacion = EscenarioLatam + EscenarioLatam_fill
EscenarioLatam_Comparacion

plotly::ggplotly(EscenarioLatam_Comparacion) #Este comando solo se utiliza para ver más a detalle aquellos países que dan problema
#tenemos problema con Costa Rica (reporta cada semana), 
#Bolivia (pico negativo a finales de febrero) 
#Republica dominicana, principios de marzo
#Guatemala y ecuador algunos valores negativos

#Notas de el gráifco EscenarioLatam 
#muestra una tendencia creciente
#con temporalidad variable
#No hay evidencia de comportmaiento ciclico 



# #Visualización por periocidad (estacionalidad) -------------------------------------------

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





### Pronósticos y modelaje  -------------------------------------------------


# Modelo TSLM -------------------------------------------------------------
#https://www.rdocumentation.org/packages/forecast/versions/8.14/topics/tslm

#Descripción
#Fit a linear model with time series components
#tslm is used to fit linear models to time series including trend and seasonality components.


# Definición del modelo 

TSLM(total_vaccinations_per_hundred ~ trend())


# Entrenamiento del modelo (Estimación) 

fit_TSLM <- Vacunas_latam_tsibble %>%
  model(Modelo_tendencia = 
          TSLM(total_vaccinations_per_hundred ~ trend()))
fit_TSLM


#Para datos rellenados 

fit_TSLM_fill <- VLT_fill %>%
  model(Modelo_tendencia = 
          TSLM(total_vaccinations_per_hundred ~ trend()))
  
fit_TSLM_fill

# Revisar el desempeño del modelo (evaluación) 


# Producir pronósticos 

#Se genera la tabla de pronósticos, el cual va ser
#una tabla de tipo fable (objeto) es decir
#forecasting table
fcst_TSLM <- fit_TSLM %>% forecast(h = 3) #se hace para los siguientes 3 meses
                                #pues los datos que se tienen hasta el momento
                                # son de 4 - 5 meses
fcst_TSLM

#tabla de pronósticos, datos rellenados

fcst_TSLM_fill <- fit_TSLM_fill %>%  forecast(h = 3)

fcst_TSLM_fill

# Visualización de la forecasting table

#para grupo 1 latama

fcst_TSLM %>%
  filter(location %in% latam1) %>%
  autoplot(Vacunas_latam_tsibble) +
  ggtitle('Vacunas en LATAM') + 
  ylab('Vacunas aplicadas por cada 100') -> fcst_TSLM_g1

#para grupo 1 latam (rellenado)

fcst_TSLM_fill %>%
  filter(location %in% latam1) %>%
  autoplot(VLT_fill) +
  ggtitle('Vacunas en LATAM') + 
  ylab('Vacunas aplicadas por cada 100') -> fcst_TSLM_fill_g1

#para grupo 2 latam

fcst_TSLM %>%
  filter(location %in% latam2) %>%
  autoplot(Vacunas_latam_tsibble) +
  ggtitle('Vacunas en LATAM') + 
  ylab('Vacunas aplicadas por cada 100') -> fcst_TSLM_g2

#para grupo 2 latam (rellenado)

fcst_TSLM_fill %>%
  filter(location %in% latam2) %>%
  autoplot(VLT_fill) +
  ggtitle('Vacunas en LATAM') + 
  ylab('Vacunas aplicadas por cada 100') -> fcst_TSLM_fill_g2

#integración de las visualizaciones

fcst_TSLM_g3 = fcst_TSLM_g1 + fcst_TSLM_fill_g1 + fcst_TSLM_g2 + fcst_TSLM_fill_g2 
fcst_TSLM_g3



# Modelo ETS (suavización exponencial con tendencia) ----------------------------
#https://www.rdocumentation.org/packages/forecast/versions/8.14/topics/ets

#ETS = Exponential smoothing state space model

#Description
# Returns ETS model applied to "y"


#Parámetros estimados

#Estimamos alfa (entre 0 y 1, la tasa a la que disminuye "el peso" de los datos en el modelo, tambien conocida como el parametro de suavizacion)
#L0 o Lt (nivel, o valor suavizado)
#Beta (entre 0 y 1, es el coefficiente que representa la pendiente de la "tendencia" )

# 'A' es para 'aditivo' , 'M' para multiplicativo y 'N' para ninguno
# Como nuestros datos tienen una tendencia marcada, seleccionmos que tanto
#el error como la tendencia sean "aditivos" 

fit_ETS_trend <- VLT_fill %>%
  model(ETS(total_vaccinations_per_hundred ~ error('A') + trend('A') + season('N')))
 
#Generamos el pronóstico para 5 pasos después 

fcst_ETS_trend <- fit_ETS_trend %>%
  forecast(h = 3) %>%
  autoplot(VLT_fill, level = NULL) + 
  labs(title = 'Pronóstico de vacunas latam con ETS',  
       x = 'meses',
       y = 'Vacunas aplicadas por cada 100') -> fcst_ETS_trend_g1

#El método de Holt es el que nos permite hacer suavizacion
#exponencial para datos con tendencia 



#Holt tiene un problema, que la tendencia solo se establece
#como creciente o decreciente. Por lo que se desarrollo
#una funcion que hace este metodo pero amortiguado

# phi es el factor de "amortiguamiento", donde phi 
# con un valor igual a 1, es identico al metodo de Holt sin
# amortiguamiento

#Ad -> aditive damped

fit_ETS_trendDamped <- VLT_fill %>%
  model(ETS(total_vaccinations_per_hundred ~ error('A') + trend('Ad') + season('N')))

fcst_ETS_trendDamped <- fit_ETS_trendDamped %>%
  forecast(h = 3) %>%
  autoplot(VLT_fill, level = NULL) + 
  labs(title = 'Pronóstico de vacunas latam con ETS amortiguado',  
       x = 'meses',
       y = 'Vacunas aplicadas por cada 100') -> fcst_ETS_trendDamped_g1

fcst_ETS_comparacion = fcst_ETS_trend + fcst_ETS_trendDamped_g1
fcst_ETS_comparacion

fit_ETS_trendDamped %>%
  forecast(h = '1 month') %>%
  autoplot(VLT_fill) +
  facet_wrap(~location, ncol = 3, scales = 'free_y') +
  labs(x = 'meses',
       y = 'Vacunas aplicadas por cada 100')

# fit_ETS_trend %>%
#   forecast(h = 3) %>%
#   autoplot(VLT_fill) +
#   labs(x = 'meses',
#        y = 'Vacunas aplicadas por cada 100')

# Modelo ARIMA ------------------------------------------------------------


