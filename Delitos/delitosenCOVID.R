library(tidyverse)
library(lubridate)
library(kableExtra)
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
delitos_tidy_tsbl %>%
  autoplot()

sexuales_y_genero = c("Abuso sexual", 
                      "Acoso sexual",
                      "Feminicidio", 
                      "Violación simple", 
                      "Violación equiparada", 
                      "Hostigamiento sexual", 
                      "Otros delitos que atentan contra la libertad y la seguridad sexual")

delitos_sexuales_y_genero_gg <- delitos_tidy_tsbl %>%
  filter (Tipo_de_delito %in% sexuales_y_genero) %>%
  ggplot() + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Tipo_de_delito))

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
  mutate(cambio = (Cuenta / lag(Cuenta) - 1) * 100) %>% 
  filter(Año != 2021)

incidencias %>% 
  ggplot(aes(x = Año, y = Cuenta, color = Tipo_de_delito)) +
  geom_line(size = 1)+
  facet_wrap(~ Tipo_de_delito, scales = "free_y") +
  theme(legend.position = "none")

incidencias %>% 
  pivot_wider(names_from = Año, values_from = Cuenta:cambio)

Incidencia_2019 <- delitos_tidy_tsbl %>% 
  group_by_key() %>% 
  index_by(Año = year(Fecha)) %>% 
  summarise(Cuenta = sum(Cuenta)) %>% 
  filter(Año %in% 2019) %>%
  as_tibble(Incidencia_2019) %>%
  transmute( Delito = Tipo_de_delito, 
                    Incidencia_2019 = Cuenta) 

Incidencia_2020 <- delitos_tidy_tsbl %>% 
  group_by_key() %>% 
  index_by(Año = year(Fecha)) %>% 
  summarise(Cuenta = sum(Cuenta)) %>% 
  filter(Año %in% 2020) %>%
  as_tibble(Incidencia_2020) %>%
  mutate( Delito = Tipo_de_delito, 
              Incidencia_2020 = Cuenta) %>%
  select(Delito, Incidencia_2020)

Incidencia <- Incidencia_2020 %>%
  add_column(Incidencia_2019$Incidencia_2019) %>%
  mutate(
    Porcentaje_de_cambio = round((
      (Incidencia_2020 - Incidencia_2019$Incidencia_2019)/Incidencia_2020), digits = 5),
    Incidencia_2019 = Incidencia_2019$Incidencia_2019) %>%
  select(Delito, Incidencia_2019, Incidencia_2020, Porcentaje_de_cambio)%>%
  arrange(desc(Porcentaje_de_cambio)) 
 
Tabla <- Incidencia %>%
  mutate(Porcentaje_de_cambio =  percent(Porcentaje_de_cambio, 2)) %>%
  kbl(fortmat = "htlm", col.names = c("Delitos",
                                      "Incidencia en 2019",
                                      "Incidencia en 2020",
                                      "Porcentaje de cambio"
  )) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "left",
                font_size = 14) %>%
  column_spec(4,
              color = ifelse( Incidencia$Porcentaje_de_cambio > 0, "red", "green"))
Tabla