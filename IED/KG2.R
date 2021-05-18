#KEVgraf
library(tidyverse)
library(readxl)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)


theme_set(theme_minimal())
IED <-  read_excel("C:/Users/jimenezb/Desktop/R-U/BD_INDICADORES_MACROECONOMICOS.xlsx",
                   sheet = "IED1")
IED <- IED %>% 
  mutate(Periodo = dmy(Periodo),
         Periodo = yearquarter(Periodo))
colnames(IED) <- c("Fecha","Tipo","Cifras Preliminares","Cifras Revisadas","C")

#IED %>% glimpse

IED$C <- NULL

IED_tsbl <- IED %>% 
  pivot_longer(
    cols      = c(`Cifras Preliminares`, `Cifras Revisadas`),
    names_to  = "Tipo_de_cifra",
    values_to = "Cifras"
  ) %>% 
  as_tsibble(
    index = Fecha,
    key   = c(Tipo, Tipo_de_cifra)
  )
#IED_tsbl %>% glimpse()

IED_tsbl %>%
  filter(Tipo_de_cifra != "C") %>% 
  autoplot() +
  facet_wrap(~ Tipo, ncol = 1)

IED_tsbl <- IED_tsbl %>%
  filter(Tipo_de_cifra != "C")

#IED_tsbl$Cifras


prediccion <- IED_tsbl %>%
  model(arima=ARIMA(Cifras)) %>% 
  forecast(h=8)

prediccion %>%autoplot(IED_tsbl)
#IED_tsbl$Cifras


