library(tidyverse)
library(lubridate)
library(tibbletime)
library(reactable)
library(htmltools)

####Importación de datos####
delitos <- read_csv("delitos2015-2021.csv",
                    locale(encoding = "latin1"),
                    col_names = TRUE, 
                    col_types = NULL
                    )

#######Quedarse solo con las columnas y filas necesarias#######

delitos_a_comparar <- c("Feminicidio", "Abuso sexual", 
                        "Acoso sexual", "Hostigamiento sexual", "Incesto",
                        "Otros delitos que atentan contra la libertad y la seguridad sexual",
                        "Violación simple", "Violación equiparada", "Trata de personas",
                        "Tráfico de menores", "Secuestro", "Violencia familiar")

delitos_tidy <- delitos %>%
  select( Ano, Entidad, Tipo_de_delito, Subtipo_de_delito, Enero:Diciembre ) %>%
  filter( Tipo_de_delito %in% delitos_a_comparar | 
          Subtipo_de_delito == "Homicidio doloso" |
          Subtipo_de_delito == "Lesiones dolosas" ) %>% 
  pivot_longer(
  cols = Enero:Diciembre ,
  names_to = "Meses",
  values_to = "Cuenta"
)

#Poner los valores de cero como NA para quitarlos
delitos_tidy[delitos_tidy==0] <- NA

#Hacer una columna para los meses y años en serie de tiempo

delitos_tidy$Meses <- gsub("Enero", 1, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Febrero", 2, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Marzo", 3, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Abril", 4, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Mayo", 5, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Junio", 6, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Julio", 7, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Agosto", 8, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Septiembre", 9, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Octubre", 10, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Noviembre", 11, delitos_tidy$Meses)
delitos_tidy$Meses <- gsub("Diciembre", 12, delitos_tidy$Meses)

delitos_tidy <- delitos_tidy %>%
  mutate( Fecha = make_date(Ano, Meses)) %>%
  na.omit() %>%
  select( Fecha, Tipo_de_delito, Cuenta)
  


####Acumulados por delito en la misma fecha####

#Feminicidio
Acum_feminicidio <- delitos_tidy %>%
  filter( Tipo_de_delito == "Feminicidio")

numfem <- as_tibble(
  aggregate(Acum_feminicidio$Cuenta, by=list(Acum_feminicidio$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Feminicidio")

#Homicidio doloso

Acum_homicidio <- delitos_tidy %>%
  filter(Tipo_de_delito == "Homicidio")

numhom <- as_tibble(
  aggregate(Acum_homicidio$Cuenta, by=list(Acum_homicidio$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Homicidio doloso")

#Lesiones dolosas

Acum_lesiones <- delitos_tidy %>%
  filter(Tipo_de_delito == "Lesiones")

numles <- as_tibble(
  aggregate(Acum_lesiones$Cuenta, by=list(Acum_lesiones$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Lesiones dolosas")

#Abuso sexual
Acum_abuso <- delitos_tidy %>%
  filter( Tipo_de_delito == "Abuso sexual")

numabuso <- as_tibble(
  aggregate(Acum_abuso$Cuenta, by=list(Acum_abuso$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Abuso sexual")

#Acoso sexual
Acum_acoso <- delitos_tidy %>%
  filter( Tipo_de_delito == "Acoso sexual")

numacoso <- as_tibble(
  aggregate(Acum_acoso$Cuenta, by=list(Acum_acoso$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Acoso sexual")
  
#Hostigamiento sexual
Acum_hosti <- delitos_tidy %>%
  filter( Tipo_de_delito == "Hostigamiento sexual")

numhosti <- as_tibble(
  aggregate(Acum_hosti$Cuenta, by=list(Acum_hosti$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Hostigamiento sexual")

#Incesto

Acum_incesto <- delitos_tidy %>%
  filter( Tipo_de_delito == "Incesto")

numincesto <- as_tibble(
  aggregate(Acum_incesto$Cuenta, by=list(Acum_incesto$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Incesto")

#Otros delitos que atentan contra la libertad y la seguridad sexual

Acum_otros <- delitos_tidy %>%
  filter( 
    Tipo_de_delito == "Otros delitos que atentan contra la libertad y la seguridad sexual")

numotros <- as_tibble(
  aggregate(Acum_otros$Cuenta, by=list(Acum_otros$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Otros delitos que atentan contra la libertad y la seguridad sexual")

#Violación simple
Acum_vs <- delitos_tidy %>%
  filter( Tipo_de_delito == "Violación simple")

numvs <- as_tibble(
  aggregate(Acum_vs$Cuenta, by=list(Acum_vs$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Violación simple")

#violación equiparada
Acum_ve <- delitos_tidy %>%
  filter( Tipo_de_delito == "Violación equiparada")

numve <- as_tibble(
  aggregate(Acum_ve$Cuenta, by=list(Acum_ve$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Violación equiparada")

#Trata de personas
Acum_trata <- delitos_tidy %>%
  filter( Tipo_de_delito == "Trata de personas")

numtrata <- as_tibble(
  aggregate(Acum_trata$Cuenta, by=list(Acum_trata$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Trata de personas")

#Tráfico de menores
Acum_trafi <- delitos_tidy %>%
  filter( Tipo_de_delito == "Tráfico de menores")

numtrafi <- as_tibble(
  aggregate(Acum_trafi$Cuenta, by=list(Acum_trafi$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Tráfico de menores")

#Secuestro
Acum_secu <- delitos_tidy %>%
  filter( Tipo_de_delito == "Secuestro")

numsecu <- as_tibble(
  aggregate(Acum_secu$Cuenta, by=list(Acum_secu$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Secuestro")

#Violencia familiar
Acum_viofam <- delitos_tidy %>%
  filter( Tipo_de_delito == "Violencia familiar")

numviofam <- as_tibble(
  aggregate(Acum_viofam$Cuenta, by=list(Acum_viofam$Fecha), sum)) %>%
  transmute(Fecha = Group.1, Cuenta = x) %>%
  mutate(Delito =  "Violencia familiar")

#####Todos los delitos acumulados en un df####
#Violencia familiar y lesiones dolosas tienen un rango mayor y mueven todo
delitos_acumulados <- rbind(
  numabuso,
  numacoso,
  numfem,
  numhosti,
  numincesto,
  numotros,
  numsecu,
  numtrafi,
  numtrata,
  numve,
  numvs,
  numhom,
  numles,
  numviofam
)

delitos_sexuales_y_de_genero <- rbind(
  numabuso,
  numacoso,
  numhosti,
  numotros,
  numvs,
  numve,
  numfem
)

delitos_libertad <- rbind (
  numtrata,
  numtrafi,
  numsecu
)

delitos_dolosos <- rbind (
  numhom,
  numles
)

#####Graficas#### 

#gráfica de delitos violentos contra la mujer

r <- ggplot(data = delitos_sexuales_y_de_genero) + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Delito))

#gráfica de los delitos contra la libertad

s <- ggplot(data = delitos_libertad) + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Delito))

#gráfica de los delitos detras de la vida y la integridad corporal

t <- ggplot(data = delitos_dolosos) + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Delito))

#gráfica sobre la incidencia de delitos relacionados a la violencia familiar
u <- ggplot(data = numviofam) + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Delito))

#gráfica de delitos generales
v <- ggplot(data = delitos_acumulados) + 
  geom_line(mapping = aes(x = Fecha, y = Cuenta, color = Delito))

plotly::ggplotly(s)

####Tabla de comparación#### 

#Abuso
numabuso <- as_tbl_time(numabuso, index = Fecha)

abuso2019 <- filter_time(numabuso, time_formula = '2019-01-01' ~ '2019-12-31')
abuso2020 <- filter_time(numabuso, time_formula = '2020-01-01' ~ '2020-12-31')

i_abuso <- numabuso %>%
  transmute(Incidencia_2019 = sum(abuso2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(abuso2020$Cuenta))  %>%
  mutate ( Delito = "Abuso sexual")

#Acoso
numacoso <- as_tbl_time(numacoso, index = Fecha)

acoso2019 <- filter_time(numacoso, time_formula = '2019-01-01' ~ '2019-12-31')
acoso2020 <- filter_time(numacoso, time_formula = '2020-01-01' ~ '2020-12-31')

i_acoso <- numacoso %>%
  transmute(Incidencia_2019 = sum(acoso2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(acoso2020$Cuenta))  %>%
  mutate ( Delito = "Acoso sexual")

#Feminicidio

numfem <- as_tbl_time(numfem, index = Fecha)

fem2019 <- filter_time(numfem, time_formula = '2019-01-01' ~ '2019-12-31')
fem2020 <- filter_time(numfem, time_formula = '2020-01-01' ~ '2020-12-31')

i_fem <- numfem %>%
  transmute(Incidencia_2019 = sum(fem2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(fem2020$Cuenta))  %>%
  mutate ( Delito = "Feminicidio")

#Homicidio doloso

numhom <- as_tbl_time(numhom, index = Fecha)

hom2019 <- filter_time(numhom, time_formula = '2019-01-01' ~ '2019-12-31')
hom2020 <- filter_time(numhom, time_formula = '2020-01-01' ~ '2020-12-31')

i_hom <- numhom %>%
  transmute(Incidencia_2019 = sum(hom2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(hom2020$Cuenta))  %>%
  mutate ( Delito = "Homicidio")

#Hostigamiento sexual

numhosti <- as_tbl_time(numhosti, index = Fecha)

hosti2019 <- filter_time(numhosti, time_formula = '2019-01-01' ~ '2019-12-31')
hosti2020 <- filter_time(numhosti, time_formula = '2020-01-01' ~ '2020-12-31')

i_hosti <- numhom %>%
  transmute(Incidencia_2019 = sum(hosti2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(hosti2020$Cuenta))  %>%
  mutate (Delito = "Hostigamiento sexual")

#Incesto

numincesto <- as_tbl_time(numhosti, index = Fecha)

incesto2019 <- filter_time(numincesto, time_formula = '2019-01-01' ~ '2019-12-31')
incesto2020 <- filter_time(numincesto, time_formula = '2020-01-01' ~ '2020-12-31')

i_inces <- numincesto %>%
  transmute(Incidencia_2019 = sum(incesto2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(incesto2020$Cuenta))  %>%
  mutate (Delito = "Incesto")

#Lesiones dolosas

numles <- as_tbl_time(numles, index = Fecha)

lesiones2019 <- filter_time(numles, time_formula = '2019-01-01' ~ '2019-12-31')
lesiones2020 <- filter_time(numles, time_formula = '2020-01-01' ~ '2020-12-31')

i_lesiones <- numles %>%
  transmute(Incidencia_2019 = sum(lesiones2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(lesiones2020$Cuenta))  %>%
  mutate (Delito = "Lesiones dolosas")

#Otros delitos que atentan contra la libertad y la seguridad sexual

numotros <- as_tbl_time(numotros, index = Fecha)

otros2019 <- filter_time(numotros, time_formula = '2019-01-01' ~ '2019-12-31')
otros2020 <- filter_time(numotros, time_formula = '2020-01-01' ~ '2020-12-31')

i_otros <- numotros %>%
  transmute(Incidencia_2019 = sum(otros2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(otros2020$Cuenta))  %>%
  mutate (Delito = "Otros delitos que atentan contra la libertad y la seguridad sexual")

#Secuestro

numsecu <- as_tbl_time(numsecu, index = Fecha)

secu2019 <- filter_time(numsecu, time_formula = '2019-01-01' ~ '2019-12-31')
secu2020 <- filter_time(numsecu, time_formula = '2020-01-01' ~ '2020-12-31')

i_secu <- numsecu %>%
  transmute(Incidencia_2019 = sum(secu2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(secu2020$Cuenta))  %>%
  mutate (Delito = "Secuestro")

#Tráfico de menores

numtrafi <- as_tbl_time(numtrafi, index = Fecha)

trafi2019 <- filter_time(numtrafi, time_formula = '2019-01-01' ~ '2019-12-31')
trafi2020 <- filter_time(numtrafi, time_formula = '2020-01-01' ~ '2020-12-31')

i_trafi <- numsecu %>%
  transmute(Incidencia_2019 = sum(trafi2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(trafi2020$Cuenta))  %>%
  mutate (Delito = "Tráfico de menores")

#Trata de personas

numtrata <- as_tbl_time(numtrata, index = Fecha)

trata2019 <- filter_time(numtrata, time_formula = '2019-01-01' ~ '2019-12-31')
trata2020 <- filter_time(numtrata, time_formula = '2020-01-01' ~ '2020-12-31')

i_trata <- numtrata %>%
  transmute(Incidencia_2019 = sum(trata2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(trata2020$Cuenta))  %>%
  mutate (Delito = "Trata de personas")

#Violación equiparada

numve <- as_tbl_time(numve, index = Fecha)

ve2019 <- filter_time(numve, time_formula = '2019-01-01' ~ '2019-12-31')
ve2020 <- filter_time(numve, time_formula = '2020-01-01' ~ '2020-12-31')

i_ve <- numve %>%
  transmute(Incidencia_2019 = sum(ve2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(ve2020$Cuenta))  %>%
  mutate (Delito = "Violación equiparada")

#Violación simple

numvs <- as_tbl_time(numvs, index = Fecha)

vs2019 <- filter_time(numvs, time_formula = '2019-01-01' ~ '2019-12-31')
vs2020 <- filter_time(numvs, time_formula = '2020-01-01' ~ '2020-12-31')

i_vs <- numvs %>%
  transmute(Incidencia_2019 = sum(vs2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(vs2020$Cuenta))  %>%
  mutate (Delito = "Violación simple")

#Violencia familiar

numviofam <- as_tbl_time(numviofam, index = Fecha)

viofam2019 <- filter_time(numviofam, time_formula = '2019-01-01' ~ '2019-12-31')
viofam2020 <- filter_time(numviofam, time_formula = '2020-01-01' ~ '2020-12-31')

i_viofam <- numviofam %>%
  transmute(Incidencia_2019 = sum(viofam2019$Cuenta)) %>%
  group_by(Incidencia_2019) %>%
  summarise(
  ) %>%
  mutate(Incidencia_2020 = sum(viofam2020$Cuenta))  %>%
  mutate (Delito = "Violencia familiar")

#Tabla de incidencia

font_instal

Incidencia <- rbind(
  i_abuso,
  i_acoso,
  i_fem,
  i_hom,
  i_inces,
  i_lesiones,
  i_otros,
  i_secu,
  i_trafi,
  i_trata,
  i_ve,
  i_viofam,
  i_vs
) 

Incidencia <- Incidencia %>%
  mutate( 
    Porcentaje_de_cambio = (((((Incidencia_2020/Incidencia_2019)*100)-100))/100)
    ) %>%
  mutate( Porcentaje_de_cambio = round(Porcentaje_de_cambio, digits = 2)) %>%
  select(Delito, Incidencia_2019, Incidencia_2020, Porcentaje_de_cambio)

