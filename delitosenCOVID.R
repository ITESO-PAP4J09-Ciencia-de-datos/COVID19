library(tidyverse)

delitos <- read_csv("delitos2015-2021.csv")

#quedarse solo con las columnas necesarias
feminicidios <- delitos %>%
  select( Ano, Entidad, Tipo_de_delito, Enero:Diciembre ) %>%
  filter( Tipo_de_delito == "Feminicidio") %>% 
  pivot_longer(
  cols = Enero:Diciembre ,
  names_to = "Meses",
  values_to = "count"
)

#Poner los valores de cero como NA para quitarlos
feminicidios[feminicidios==0] <- NA

#separlos por año

#2015
fem2015 <- feminicidios %>%
  na.omit() %>%
  filter( Ano == 2015)

total2015 <- sum(fem2015$count)

#2016
fem2016 <- feminicidios %>%
  na.omit() %>%
  filter( Ano == 2016)

total2016 <- sum(fem2016$count)

#2017
fem2017 <- feminicidios %>%
  na.omit() %>%
  filter( Ano == 2017)

total2017 <- sum(fem2017$count)

#2018
fem2018 <- feminicidios %>%
  na.omit() %>%
  filter( Ano == 2018)

total2018 <- sum(fem2018$count)

#2019
fem2019 <- feminicidios %>%
  na.omit() %>%
  filter( Ano == 2019)

total2019 <- sum(fem2019$count)

#2020
fem2020 <- feminicidios %>%
  na.omit() %>%
  filter( Ano == 2020)

total2020 <- sum(fem2020$count)

byyear <- c(total2015, 
            total2016, 
            total2017,
            total2018,
            total2019,
            total2020)

fembyyear <- data.frame( "Ano" = c(2015, 2016, 2017, 2018, 2019, 2020), 
                         num_total = byyear)

ggplot(fembyyear
       )