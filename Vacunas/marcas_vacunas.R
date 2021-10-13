## Librerias

library(tidyverse) 
library(tsibble)
library(feasts)

`%nin%` <- Negate(`%in%`)

## Descarga de base de datos con marcas de vacuna

marcas <- c("Pfizer/BioNTech", "Moderna", "CanSino", "Oxford/AstraZeneca")

vacunas_totales <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv")

p1 <- vacunas_totales %>% 
  group_by(location, vaccine) %>% 
  slice_tail(n = 1) %>% 
  filter(vaccine %in% marcas) %>% 
  ggplot(aes(x = location, y = total_vaccinations, fill = vaccine)) +
  geom_col() +
  facet_wrap(~ vaccine, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_y_log10()

plotly::ggplotly(p1)

vacunas_totales %>% 
  as_tsibble(index = date, key = c(location, vaccine)) %>% 
  ggplot(aes(x = date, y = total_vaccinations, color = location)) +
  geom_line(size = 2) +
  facet_wrap(~ vaccine, scales = "free_y") +
  scale_y_log10()
  
  

## filtrar ultima información de vacunas

vacunas <- Vacunastotales %>% mutate(date = as.Date(date,"%m/%d/%Y")) %>% 
  filter(date >= max(date-4) & date <= max(date-1)) %>%
vacunas <- vacunas[!(vacunas$location=="United States" | vacunas$location=="European Union"),]
  
## Total de vacuna Johnson&johnson

vacuna_jj <- vacunas %>% filter(vaccine == "Johnson&Johnson") %>% 
  distinct(location,.keep_all = TRUE)
  
## Total de vacuna Moderna

vacuna_m <- vacunas %>% filter(vaccine == "Moderna") %>% 
  distinct(location,.keep_all = TRUE)

## Total de vacuna Oxford/AstraZeneca

vacuna_az <- vacunas %>% filter(vaccine == "Oxford/AstraZeneca") %>% 
  distinct(location,.keep_all = TRUE)

## Total de vacuna Pfizer/BioNTech

vacuna_p <- vacunas %>% filter(vaccine == "Pfizer/BioNTech") %>% 
  distinct(location,.keep_all = TRUE)

## Gráficas

barplot(vacuna_jj$total_vaccinations,
        main = "Johnson&Johnson",
        xlab = "Pais",
        ylab = "Vacunas",
        names.arg = list(vacuna_jj$location),
        col = "darkred",
        las = 2,
        cex.names = 1)

barplot(vacuna_m$total_vaccinations,
        main = "Moderna",
        xlab = "Pais",
        ylab = "Vacunas",
        names.arg = list(vacuna_jj$location),
        col = "blue",
        las = 1,
        cex.names = 1)

barplot(vacuna_az$total_vaccinations,
        main = "AztraZeneca",
        xlab = "Pais",
        ylab = "Vacunas",
        names.arg = list(vacuna_jj$location),
        col = "green",
        las = 1,
        cex.names = 1)

barplot(vacuna_p$total_vaccinations,
        main = "Pfizer",
        xlab = "Pais",
        ylab = "Vacunas",
        names.arg = list(vacuna_jj$location),
        col = "orange",
        las = 1,
        cex.names = 1)

