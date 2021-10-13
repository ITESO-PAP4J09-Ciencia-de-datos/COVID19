library(tidyverse)
library(dplyr)
library(ggplot2)


DF1 <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

DFManufacturer <- readr::read_csv("./O21/country_vaccinations_by_manufacturer.csv") %>% 
  rename(region = location)

totalVaccCountry <- DF1 %>% 
  group_by(location) %>% 
  summarise(total_vaccinations = sum(total_vaccinations[!is.na(total_vaccinations)]))

print(totalVaccCountry)

VaccManufacturer  <- DFManufacturer %>% 
  group_by(region,vaccine) %>% 
  summarise(total_vaccinations = sum(total_vaccinations[!is.na(total_vaccinations)]))

VaccManufacturerCount  <- DFManufacturer %>% 
  group_by(vaccine) %>% 
  summarise(total_vaccinations = sum(total_vaccinations[!is.na(total_vaccinations)]))

print(VaccManufacturerCount)

barplot(height=VaccManufacturerCount$total_vaccinations, names=VaccManufacturerCount$vaccine, 
        main = "Gráfica de vacunas por marca", xlab = "Marca de vacuna", ylab = "Cantidad")


print(VaccManufacturer)


VaccManufacturerSplited <- VaccManufacturer %>%
  pivot_wider(names_from = vaccine, values_from = total_vaccinations, values_fill = 0)

VaccManufacturer %>% 
  group_by(region) %>% 
  slice_max(n = 1, order_by = total_vaccinations)

#VaccTemp = subset(VaccManufacturerSplited, select = -c(region))


VaccManufacturerFinal <- VaccManufacturerSplited %>% 
  #tidying
  #mutate(id = row_number()) %>% 
  pivot_longer(-region, values_to = "amount") %>% 
  group_by(region) %>%  
  arrange(region, desc(amount)) %>% 
  #workhorse
  mutate(popularVaccine = head(name, 1) ) %>% 
  #Pivot
  pivot_wider(names_from = name, values_from = amount) %>% 
  rename(J_J         = `Johnson&Johnson`, # renombré algunas columnas
         AstraZeneca = `Oxford/AstraZeneca`,
         Pfizer      = `Pfizer/BioNTech`,
         Sinopharm    = `Sinopharm/Beijing`,
         Sputnik     = `Sputnik V`) %>%  
  mutate(Total = sum(across(Pfizer:Sputnik))) %>% # agregué una columna con el total de vacunas por país
  # mutate(across(Pfizer:Sputnik, ~ ./Total * 100)) # agregué columnas con el % de cada vacuna por país
  mutate(across(Pfizer:Sputnik, .fns = list(pct = ~ ./Total * 100)))
#clean_names() # intento de quitar los puntos de los nombres de las columnas
  


#creating map


mapData <- map_data("world")
view(mapData)
mapData <- left_join(mapData, VaccManufacturerFinal, by="region")
view(mapData)
#mapData1 <- mapData %>% filter(!is.na(mapData$popularVaccine))
#view(mapData1)


mapData %>% 
  glimpse()
 # select(Pct.Moderna)

mapa <- ggplot(mapData, aes(x=long, y=lat, group=group)) + 
  geom_polygon(
    aes(
      label = region, 
      fill = popularVaccine, 
      label1 = `Moderna_pct`,
      label2 = `CanSino_pct`,
      label3 = `J_J_pct`,
      label4 = `AstraZeneca_pct`,
      label5 = `Pfizer_pct`,
      label6 = `Sinopharm_pct`,
      label7 = `Sinovac_pct`,
      label8 = `Sputnik_pct`
      #label1 = text
        ), 
    color = "black") + 
  theme_void()
mapa

plotly::ggplotly(mapa)


