#Graficas IMSS

# pkgs --------------------------------------------------------------------

library(tidyverse)
library(reshape)
library(readxl)
library(ggpubr)
library(tsibble)

# data --------------------------------------------------------------------

#Leer del excel los datos y pasarlos al dataframe IMMSS
IMSS <- read_excel("INDICADORES_MACROECONOMICOS/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS")

#Cambiar el nombre de las columnas para facilitar los calculos
colnames(IMSS) <- c("Division","Grupo","Fracción","Fecha","Trabajadores_Asegurados","Trabajadores_Permanentes","Trabajadores_Eventuales")

IMSS_tbl <- IMSS %>% 
  mutate(Fecha = yearmonth(Fecha))

IMSS_tbl %>% 
  as_tsibble(
    index = Fecha,
    key   = c(Division, Grupo, Fracción) 
  )

IMSS_tbl %>% 
  # filter(Division == "Agricultura, ganadería, silvicultura, pesca y caza") %>% 
  ggplot(aes(x = Fecha, y = Trabajadores_Asegurados, color = Fracción)) + 
  geom_line() + 
  # facet_wrap(~ Grupo, scales = "free_y") +
  facet_grid(Division ~ Grupo, scales = "free_y") +
  theme(legend.position = "none")

# filter01 <- IMSS$Division == "Agricultura, ganader?a, silvicultura, pesca y caza"
# filter02 <- IMSS$Division == "Transportes y comunicaciones"
# filter03 <- IMSS$Division == "Servicios para empresas, personas y el hogar"
# filter04 <- IMSS$Division == "Servicios sociales y comunales"
# filter05 <- IMSS$Division == "Industrias extractivas"
# filter06 <- IMSS$Division == "Industrias de transformaci?n"
# filter07 <- IMSS$Division == "Industria el?ctrica, y captaci?n y suministro de agua potable"
# filter08 <- IMSS$Division == "Servicios para empresas, personas y el hogar"
# filter09 <- IMSS$Division == "Comercio"
# filter10 <- IMSS$Division == "Industria de la construcci?n"
# 
# filter01.01 <- filter01 & IMSS$Fracci?n == "Acuicultura"
# filter01.02 <- filter01 & IMSS$Fracci?n == "Explotaci?n de bosques madereros; extracci?n de productos forestales no maderables y servicios de explotaci?n forestal"
# filter01.03 <- filter01 & IMSS$Fracci?n == "Pesca en aguas interiores"
# filter01.04 <- filter01 & IMSS$Fracci?n == "Trabajos de buceo"
# 
# g01 <- ggplot(data = IMSS[filter01,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g02 <- ggplot(data = IMSS[filter02,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g03 <- ggplot(data = IMSS[filter03,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g04 <- ggplot(data = IMSS[filter04,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g05 <- ggplot(data = IMSS[filter05,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g06 <- ggplot(data = IMSS[filter06,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g07 <- ggplot(data = IMSS[filter07,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g08 <- ggplot(data = IMSS[filter08,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g09 <- ggplot(data = IMSS[filter09,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g10 <- ggplot(data = IMSS[filter10,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# 
# 
# g01 <- ggplot(data = IMSS[filter01.01,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracci?n)) + geom_line(size=0.01)
# g01

?trendline()

filter01.03

ggarrange(g01, g02, g03, g04, nrow = 2, ncol = 2)
ggarrange(g05, g06, g07, g08, nrow = 2, ncol = 2)
ggarrange(g09, g10, nrow = 2, ncol = 2)

g05

?facet_grid




#Filtrar las fechas para que solo nos apareca el a?o (eliminar mes y d?a)

#IMSS$Fecha <- substring(IMSS$Fecha,1,4)
#IMSS$Fecha

#Mdificar la tabla para poder graficar los datos m?s sencillo
IMSS_Filtro <- IMSS %>% gather(Trabajador, Asegurados, c("Trabajadores_Asegurados",
                                                         "Trabajadores_Permanentes","Trabajadores Eventuales"))
#Definir los par?metros de la gr?fica
Graf1 <- ggplot(data=IMSS_Filtro[IMSS_Filtro$Fecha!=2021,],aes(x=Fecha,y=Asegurados))

#Gr?ficar


#Graf1 + geom_col(aes(fill=Trabajador)) + 
#  ggtitle("Gr?fica de trabajadores asegurados por IMSS, se?alando el tipo de trabajador")


#Patrones
Patrones<- read_excel("C:/Users/Osso/Downloads/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "Patrones")
#Filtrar la fecha para que solo lea por a?o
Patrones$Fecha <- substring(Patrones$Fecha,1,4)

#Definir los par?metros de la gr?fica
Graf2 <- ggplot(data=Patrones[Patrones$Fecha!=2021,],aes(x=Fecha,y=Patrones))
#Gr?ficar
Graf2 + geom_point(aes(color=cve2)) +
  coord_cartesian(ylim=c(0,40000)) +
  theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("Gr?fica de Patrones asegurados en IMMS por a?o, separado por tipo de trabajo")
