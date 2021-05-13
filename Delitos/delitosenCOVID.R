library(tidyverse)
library(lubridate)
library(formattable)
library(tsibble)

####Importación de datos####
delitos <- read_csv("Delitos/delitos2015-2021.csv",
                    locale(encoding = "latin1"),
                    col_names = TRUE, 
                    col_types = NULL
                    )

#######Quedarse solo con las columnas y filas necesarias#######

delitos_a_comparar <- c("Feminicidio", "Abuso sexual", 
                        "Acoso sexual", "Hostigamiento sexual",
                        "Otros delitos que atentan contra la libertad y la seguridad sexual",
                        "Violación simple", "Violación equiparada", "Trata de personas",
                        "Tráfico de menores", "Secuestro", "Violencia familiar")

delitos_tidy <- delitos %>%
  filter( Tipo_de_delito %in% delitos_a_comparar | 
          Subtipo_de_delito == "Homicidio doloso" |
          Subtipo_de_delito == "Lesiones dolosas" ) %>% 
  pivot_longer(
  cols = Enero:Diciembre ,
  names_to = "Meses",
  values_to = "Cuenta"
) %>% 
  group_by(Ano, Meses, Tipo_de_delito, Subtipo_de_delito) %>% 
  summarise(Cuenta = sum(Cuenta), .groups = "drop")

glimpse(delitos_tidy)

delitos_tidy <- delitos_tidy %>% 
  mutate(
    Meses = str_trunc(Meses, width = 3, ellipsis = ""),
    Meses = case_when(
      Meses == "Ene" ~ "Jan",
      Meses == "Abr" ~ "Apr",
      Meses == "Ago" ~ "Aug",
      Meses == "Dic" ~ "Dec",
      TRUE           ~ Meses
    )
  ) %>% 
  unite(col = "Fecha", c(Ano,Meses), sep = " ") %>% 
  mutate(Fecha = yearmonth(Fecha))

delitos_tidy_tsbl <- delitos_tidy %>% 
  as_tsibble(
    index = Fecha,
    key   = c(Tipo_de_delito, Subtipo_de_delito)
  )

#####Graficas#### 

#Delitos generales

sexuales_y_genero = c("Abuso sexual", 
                      "Acoso sexual",
                      "Feminicidio", 
                      "Violación simple", 
                      "Violación equiparada", 
                      "Hostigamiento sexual", 
                      "Otros delitos que atentan contra la libertad y la seguridad sexual")

delitos_sexuales_y_genero_gg <- delitos_tidy_tsbl %>%
  filter (Tipo_de_delito %in% sexuales_y_genero) %>%
  ggplot(aes(x = Fecha, y = Cuenta, color = Tipo_de_delito)) +
  geom_line(size = 1)+
  facet_wrap(~ Tipo_de_delito, scales = "free_y") +
  theme(legend.position = "none")

delitos_contra_libertad_gg <- delitos_tidy_tsbl %>%
  filter (Tipo_de_delito %in% c("Trata de personas", "Tráfico de menores", "Secuestro") ) %>%
  ggplot() + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Tipo_de_delito))

delitos_dolosos_gg <- delitos_tidy_tsbl %>%
  filter(Subtipo_de_delito %in% c("Lesiones dolosas", "Homicidio doloso")) %>%
  ggplot() + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Tipo_de_delito))

delitos_violencia_familiar_gg <- delitos_tidy_tsbl %>%
  filter (Tipo_de_delito == "Violencia familiar") %>%
  ggplot() +
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Tipo_de_delito))


####Tabla de comparación#### 

#Tabla de incidencia

incidencias <- delitos_tidy_tsbl %>% 
  group_by_key() %>% 
  index_by(Año = year(Fecha)) %>% 
  summarise(Cuenta = sum(Cuenta)) %>% 
  as_tibble(incidencias) %>% 
  mutate(cambio = (Cuenta / lag(Cuenta) - 1)*100) %>% 
  filter(Año != 2021)

Todos_delitos_gg <- incidencias %>% 
  ggplot(aes(x = Año, y = Cuenta, color = Tipo_de_delito)) +
  geom_line(size = 1)+
  facet_wrap(~ Tipo_de_delito, scales = "free_y") +
  theme(legend.position = "none")

perc_cambio_incidencias <- incidencias %>%
  ggplot(aes(x = Año, y = cambio, color = Subtipo_de_delito)) +
  geom_line() +
  geom_line(size = 1)+
  facet_wrap(~ Subtipo_de_delito, scales = "free_y") +
  theme(legend.position = "none")
plotly::ggplotly(perc_cambio_incidencias)

incidencias <- incidencias %>% 
  pivot_wider(names_from = Año, values_from = Cuenta:cambio)

Tabla <- incidencias %>%
  select( Subtipo_de_delito, Cuenta_2019, Cuenta_2020, cambio_2020) %>%
  arrange(-cambio_2020) %>%
  transmute('Tipo de delito' = Subtipo_de_delito,
            'Incidencia en 2019' = Cuenta_2019,
            'Incidencia en 2020' = Cuenta_2020,
            'Porcentaje de cambio' =  round(cambio_2020, digits = 2))

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

cambio_format <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold",
              color = ifelse(x < 0, customGreen, ifelse(x > 0, customRed, "black"))),
            x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
  ) 

formattable(Tabla, 
            align = c("l", rep("r", NCOL(Tabla) - 1)),
            list('Tipo de delito' = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 'Porcentaje de cambio' = cambio_format
            ))
 