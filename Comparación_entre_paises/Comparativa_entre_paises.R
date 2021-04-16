
library(tidyverse) #librerias que se necesitan
library(lubridate)
library(tsibble)

nuevos_casos_mundiales <- read_csv("https://raw.github.com/owid/covid-19-data/master/public/data/jhu/full_data.csv")

#vector para la selección de paises con población similar
poblacion_similiar <- c("Mexico",
                        "Japan", 
                        "Russia", 
                        "Bangladesh", 
                        "Philippines")
#Paises de LATAM
latam <- c("Mexico", "Argentina", 
           "Colombia", "Chile", 
           "Brazil", "Bolivia", 
           "Costa Rica", "Ecuador",
           "Guatemala", "Panama", 
           "Paraguay", "Peru", 
           "Puerto Rico", "Dominican Republic")

Comparativa_nuevos_casos <- nuevos_casos_mundiales %>%
  ggplot(aes(x = date, y = new_cases, group = location)) +
  geom_line(color = "grey") +
  geom_line(data = nuevos_casos_mundiales %>% filter(location %in% poblacion_similiar),
            aes(color = location), size = 1) +
  scale_y_log10() 

casos_por_millon <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases_per_million.csv")

Comparativa_casos_latam <- casos_por_millon %>%
  select(date, matches(latam)) %>%
  pivot_longer(
    cols = 'Mexico':'Dominican Republic',
    names_to = "Paises",
    values_to = "Casos_por_millon"
  ) %>%
  filter( Paises != "Ecuador")

Comparativa_casos_latam_tsbl<- Comparativa_casos_latam %>%
  as_tsibble(
    index = date,
    key = Paises
  )

GraphLatam <- Comparativa_casos_latam_tsbl %>%
  filter(Paises != "Ecuador") %>% #Se elimina ecuador de la lista de paises por datos críticos negativos
  as_tsibble(
    index = date )%>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = Casos_por_millon, color = Paises))
  

  
