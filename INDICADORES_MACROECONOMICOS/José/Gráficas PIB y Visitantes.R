library(readxl)
library(tidyverse)
library(tsibble)
library(feasts)

##Gr?fica PIB
PIB <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "PIB")
#Filtrar del dataset solo los datos que sean de Jalisco
PIB <- PIB[PIB$`Nombre entidad`=="Jalisco",]
#Renombrar las columnas para facilitar los calculos
colnames(PIB) <- c("Clave","Estado","Year","Clave_Sector","Nombre_Sector","Actividad","PIB")
#Cambiar el tipo de dato de la columna "Year" para poder filtrar 
###PIB$Year <- factor(PIB$Year)
#Definir los datos que vamos a necesitar para filtar
a <- ggplot(data=PIB,aes(x=Year,y=PIB))
#Gr?fico boxplot con Jitter
a + geom_jitter(color=I(3)) + geom_boxplot(alpha=I(0.5)) +
  facet_grid(Actividad~.,scales="free")+
  ggtitle("Gr?ficas de PIB por a?o, separadas por tipo de actividad")
#Graf?co de barras dividiendo por actividad
a + geom_col(aes(fill=Actividad)) +
  ggtitle("Gr?fica de PIB por a?o, se?alando tipo de actividad")

a + geom_line()


PIB_Tsibble <- PIB %>% as_tsibble(index=Year,key=c(Estado,Nombre_Sector,Actividad))

PIB_Tsibble1 <- PIB_Tsibble %>% filter(Estado=="Jalisco") %>% distinct(Nombre_Sector)


p4 <- PIB_Test %>% group_by(Estado) %>% 
  summarize(PIB1=sum(PIB)) %>% 
  ggplot(aes(x=Year,y=PIB1)) +
  geom_line(aes(color=Estado),size=1.3) +
  scale_y_log10() 

p4 + transition_reveal(PIB1)

anim_save(file_name= "C:/Users/JoseGallardo/Documents/Ajolotec/PAP/GIF_TEST.gif")




PIB_Tsibble1 %>% ?autoplot()


y <- tsibble(Year = 2015:2019,
             Observation = c(123,39,78,52,110),
             index = Year)
autoplot(y)
?autoplot
?gafa_stock
mycolors <- c("blue", "#FFC125", "darkgreen", "darkorange")


b <-  PIB_Actividad %>% ggplot(aes(x=Year,y=PIB))
b + geom_line(aes(color=Actividad)) + scale_y_log10()

PIB_Test <- PIB_Tsibble %>% group_by(Estado,Nombre_Sector,Actividad) %>% 
  summarise(PIB=sum(PIB))

p1 <- PIB_Test %>% group_by(Estado) %>% 
  summarize(PIB1=sum(PIB)) %>% 
  ggplot(aes(x=Year,y=PIB1)) +
  geom_line(aes(color=Estado),size=1.3) +
  scale_y_log10() +
  transition_reveal(PIB1)
p1



p2 <- PIB_Test %>% filter(Year==2018) %>%
  group_by(Estado) %>%
  summarise(PIB1 = sum(PIB)) %>%
  filter(PIB1>700000) %>%
  plot_ly(labels = ~Estado,
          values = ~PIB1,
          marker = list(colors = mycolors)) %>%
  add_pie(hole = 0.2) %>%
  layout(xaxis = list(zeroline = F,
                      showline = F,
                      showticklabels = F,
                      showgrid = F),
         yaxis = list(zeroline = F,
                      showline = F,
                      showticklabels=F,
                      showgrid=F))
p2

p3 <- PIB_Test %>% filter(Year==2018) %>%
  group_by(Actividad) %>%
  summarise(PIB1 = sum(PIB))  %>%
  plot_ly(labels = ~Actividad,
          values = ~PIB1,
          marker = list(colors = mycolors)) %>%
  add_pie(hole = 0.2) %>%
  layout(xaxis = list(zeroline = F,
                      showline = F,
                      showticklabels = F,
                      showgrid = F),
         yaxis = list(zeroline = F,
                      showline = F,
                      showticklabels=F,
                      showgrid=F))
p3


?salesdt


.









##Gr?fica de Visitantes extranjeros por entrada a?rea, por nacionalidad
Visitantes<- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "Visitantes")
#Filtrar del dataset solo los datos que sean de Jalisco
Visitantes <- Visitantes[(Visitantes$Aeropuerto=='Puerto Vallarta, Jal.')|(Visitantes$Aeropuerto=='Guadalajara, Jal.'),] 
#Cambiar el tipo de dato de la columna "A?o" para poder filtrar 
Visitantes$A?o <- factor(Visitantes$A?o)
##Definir los datos que vamos a necesitar para filtar
#Filtrando y eliminando ciertas regiones para tener un an?lisis m?s limpio
b <- ggplot(data=Visitantes[(Visitantes$Regiones!="Am?rica del Norte"&
                               Visitantes$Regiones!="Europa"&
                               Visitantes$Regiones!="Ap?trida"&
                               Visitantes$Regiones!="Islas del Caribe"&
                               Visitantes$Regiones!="?frica"),],
            aes(x=Regiones,y=Entradas))
##Grafica de Barras dividiendo entre los dos aeropuertos y separando entre sexo
b + geom_col(aes(fill=Sexo)) + 
  facet_grid(A?o~Aeropuerto,scales="free") +
  ggtitle("Graficas de visitantes extranjeros divididos por area y por aeropuerto")


#Grafica IMSS
library(reshape)
library(tidyr)
#Leer del excel los datos y pasarlos al dataframe IMMSS
IMSS <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS")
#Cambiar el nombre de las columnas para facilitar los calculos
colnames(IMSS) <- c("Divisi?n","Grupo","Fracci?n","Fecha","Trabajadores_Asegurados","Trabajadores_Permanentes","Trabajadores Eventuales")
#Filtrar las fechas para que solo nos apareca el a?o (eliminar mes y d?a)
IMSS$Fecha <- substring(IMSS$Fecha,1,4)
#Mdificar la tabla para poder graficar los datos m?s sencillo
IMSS_Filtro <- IMSS %>% gather(Trabajador, Asegurados, c("Trabajadores_Asegurados",
                                                         "Trabajadores_Permanentes","Trabajadores Eventuales"))
#Definir los par?metros de la gr?fica
Graf1 <- ggplot(data=IMSS_Filtro[IMSS_Filtro$Fecha!=2021,],aes(x=Fecha,y=Asegurados))
#Gr?ficar
Graf1 + geom_col(aes(fill=Trabajador)) + 
  ggtitle("Gr?fica de trabajadores asegurados por IMSS, se?alando el tipo de trabajador")



#Patrones
Patrones<- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx",
                      sheet = "Patrones")
#Filtrar la fecha para que solo lea por a?o
Patrones$Fecha <- substring(Patrones$Fecha,1,4)
#Definir los par?metros de la gr?fica
Graf2 <- ggplot(data=Patrones[Patrones$Fecha!=2021,],aes(x=Fecha,y=Patrones))
#Gr?ficar
Graf2 + geom_jitter(aes(color=cve2)) +
  coord_cartesian(ylim=c(0,40000)) +
  theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("Gr?fica de Paotrones asegurados en IMMS por a?o, separado por tipo de trabajo")



#Inversi?n Extranjera Directa por tipo de inversi?n
#Definir el Dataset
IED <-  read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx",
                   sheet = "IED1")
#Filtrar la fecha para solo tomar los a?os
IED$Periodo <- substring(IED$Periodo,7,10)
#Cambiar nombre de columnas
colnames(IED) <- c("Fecha","Tipo","Cifras Preliminares","Cifras Revisadas","C")
#Reformular el dataset para facilitar la graficaci?n
IED_Filtro <- IED %>% gather( Tipo_de_cifra,Cifras, c("Cifras Preliminares","Cifras Revisadas"))
#Hcarer factor la fecha para facilitar modelado
IED_Filtro$Fecha <- factor(IED_Filtro$Fecha)
#Definir valores de gr?fica
Graf4 <- ggplot(data=IED_Filtro[IED_Filtro$Cifras>0,],aes(x=Fecha,y=Cifras))
#Graficar
Graf4 +  geom_col(aes(fill=Tipo_de_cifra)) + 
  ggtitle("Inversi?n Extranjera Directa por tipo de inversi?n")




#Caracter?sticas de la poblaci?n ocupada seg?n la Encuesta Nacional de Ocupaci?n y Empleo
ENOE <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/BD_INDICADORES_MACROECONOMICOS.xlsx",
                   sheet = "ENOE")
colnames(ENOE)<-c("Entidad","Posicion","Salud","Actividad","Ocupaci?n","Ingreso","Formal","Personas")
ENOE <- ENOE[ENOE$Entidad=="Jalisco",]

Graf3 <- ggplot(data=ENOE[ENOE$Actividad!="NA",],aes(x=Actividad,y=Personas))
Graf3 + geom_col(aes(fill=Formal)) +
  theme(axis.text.x=element_text(angle=45)) + 
  ggtitle("Cantidad de personas en secotres, se?alados por tipo de empleo")




```{r}
Visitantes<- read_excel("BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "Visitantes") %>% as_tsibble(index=c(Año,Mes),key=c(Aeropuerto,Nacionalidad,
                                                                                                                           Regiones,Sexo))
colnames(Visitantes) <-  c("Year","Mes","Aeropuerto","Nacionalidad","Regiones","Sexo","Entradas")
Visitantes$Mes <- NULL
#Visitantes$Date <-  as.Date(paste(Visitantes$Year,Visitantes$Mes,sep="-"),"%Y-%M-%d")
#df$Date <- as.Date(paste(df$year, df$month, sep="-"), "%Y-%M")
Visitantes <-  Visitantes %>% as_tsibble(index=Year,
                                         key=c(Aeropuerto,Nacionalidad,
                                               Regiones,Sexo)) %>% 
  group_by(Aeropuerto,Nacionalidad,
           Regiones,Sexo)
head(Visitantes %>% group_by(Aeropuerto,Nacionalidad,
                             Regiones,Sexo))
```


