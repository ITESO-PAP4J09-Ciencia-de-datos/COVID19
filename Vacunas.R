library(tidyverse)
Vacunastotales <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")




latam <- c("Mexico", "Argentina", 
           "Colombia", "Chile", 
           "Brazil", "Bolivia", 
           "Costa Rica", "Ecuador",
           "Guatemala", "Panama", 
           "Paraguay", "Peru", 
           "Puerto Rico", "Dominican Republic")


Vacunas_latam <- Vacunastotales %>%
  select( date, location, total_vaccinations, 
          total_vaccinations_per_hundred, 
          daily_vaccinations_per_million) %>%
  filter(location %in% latam)

p <- ggplot(data = Vacunas_latam) + 
  geom_line(mapping = aes(x = date, y = total_vaccinations_per_hundred, color = location))

plotly::ggplotly(p)