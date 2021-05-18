


#Graficas IMSS

# pkgs --------------------------------------------------------------------

library(tidyverse)
library(reshape)
library(readxl)
library(ggpubr)
library(tsibble)
library(forecast)
library(fable)

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

IMSS$Division
# `git config` --list
# 
# git config --global user.email nt701802@iteso.mx
#IMSS_tbl %>% ARIMA(log() ~ 0, pdq(3,0,1) + PDQ(0,1,2))

#wind <- window()
# ctrl + shift + c
# 
# IMSS %>% glimpse()
# IMSS_tbl <- IMSS %>%
#   pivot_longer(
#     cols = c(`Trabajadores_Asegurados`, `Trabajadores_Permanentes`, `Trabajadores_Eventuales`),
#     names_to = "Tipo_de_cifra" ,
#     values_to = "Cifras"
#   ) %>%
#   as_tsibble(
#     index = Fecha,
#     key = c(Fracción, Tipo_de_cifra)
#   )
# IMSS_tbl %>% glimpse()
# 





# IMSS %>% glimpse() 
# IMSS_tbl %>% 
#   # filter(Division == "Agricultura, ganadería, silvicultura, pesca y caza") %>% 
#   ggplot(aes(x = Fecha, y = Trabajadores_Asegurados, color = Fracción)) + 
#   geom_line() + 
#   # facet_wrap(~ Grupo, scales = "free_y") +
#   #facet_grid(Division ~ Grupo, scales = "free_y") +
#   facet_grid(Division ~ Grupo, scales = "free_y") +
#   theme(legend.position = "none") #+ ggthemes::theme_economist()



# ?facet_grid
# unique(IMSS$Grupo)
# unique(IMSS$Fracción)
# typeof(IMSS_tbl)

# length(IMSS$Grupo[IMSS$Grupo=="Pesca"])
# IMSS$Grupo
#filter01 <- IMSS$Division == "Agricultura, ganadería, silvicultura, pesca y caza"
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
#g01 <- ggplot(data = IMSS[filter01,],aes(x = Fecha, y = Trabajadores_Asegurados, colour=Fracción)) + geom_line(size=0.01) #+ #(legend.position = "none")
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
# 
# tstest <- data.frame(var2, IMSS$Trabajadores_Asegurados)
# tstest
# Fecha <- IMSS$Fecha
# #var1 <- ts(tstest$X2, start = 1, frequency = 1)
# 
# IMSS$Fecha
# var2 <- IMSS$Fecha
# head(tstest)
# IMSS$Division
# var3
# 
# var5 <- ts(tstest$var2, start = 1, frequency = 12)
# 
# var4 <- ts(tstest, start = 1, frequency = 12)
# var4
# 
# plot(var5, ylab=IMSS$Trabajadores_Asegurados, xlab = IMSS$Fecha)
# 
# ?trendline()
# 
# typeof(tstest)
# typeof(var4)
# 
# filter01
# IMSS[filter01,]
# 
# IMSS$Trabajadores_Asegurados
# IMSS$Fecha
# 
# Fecha <- as.POSIXct
# Fecha
# ggarrange(g01, g02, g03, g04, nrow = 2, ncol = 2)
# ggarrange(g05, g06, g07, g08, nrow = 2, ncol = 2)
# ggarrange(g09, g10, nrow = 2, ncol = 2)
# 
# g05
# 
# ?facet_grid




#Filtrar las fechas para que solo nos apareca el a?o (eliminar mes y d?a)

#IMSS$Fecha <- substring(IMSS$Fecha,1,4)
#IMSS$Fecha

#Mdificar la tabla para poder graficar los datos m?s sencillo
# IMSS_Filtro <- IMSS %>% gather(Trabajador, Asegurados, c("Trabajadores_Asegurados",
#                                                          "Trabajadores_Permanentes","Trabajadores Eventuales"))
# #Definir los par?metros de la gr?fica
# Graf1 <- ggplot(data=IMSS_Filtro[IMSS_Filtro$Fecha!=2021,],aes(x=Fecha,y=Asegurados))
# 
# #Gr?ficar


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