library(shiny)
library(shinydashboard)
library(flexdashboard)
library(knitr)
library(DT)
library(rpivotTable)
library(ggplot2)
library(plotly)
library(dplyr)
library(openintro)
library(highcharter)
library(ggvis)
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(readxl)
library(lubridate)
library(gganimate)
library(gifski)
library(av)
library(gapminder)
library(stringr)
library(readr)
library(zoo)
library(forecast)
library(shinythemes)
library(urca)
library(plotly)
library(patchwork)
library(lubridate)
library(fable.prophet)



Visitantes<- read_excel("../BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "Visitantes")
colnames(Visitantes) <-  c("Year","Mes","Aeropuerto","Nacionalidad","Regiones","Sexo","Entradas")


# Visitantes <- Visitantes %>% filter(Aeropuerto=="Guadalajara, Jal."|
#                                       Aeropuerto=="Puerto Vallarta, Jal.")



Visitantes <- Visitantes %>% 
  slice_sample(n = 500) %>% 
  mutate(
    Mes = str_trunc(Mes, side = "right", width = 3, ellipsis = ""),
    Mes = case_when(
      Mes == "Ene" ~ "Jan",
      Mes == "Abr" ~ "Apr",
      Mes == "Ago" ~ "Aug",
      Mes == "Dic" ~ "Dec",
      TRUE         ~ Mes
    )
  ) %>% 
  unite(Periodo, Year:Mes, sep = " ", remove = FALSE)%>%
  mutate(Periodo = yearmonth(Periodo)) %>% 
  as_tsibble(
    index = Periodo,
    key   = c(Aeropuerto, Nacionalidad, Regiones,Sexo)
  ) %>%
  mutate_if(is.character,as_factor) 

models <- c("SNAIVE", "ETS", "ARIMA")





ui <- dashboardPage(
  dashboardHeader(title= "My Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    box(plotlyOutput("Plot1"),width = 8),
    box(
      selectInput("Aeropuerto","Aeropuerto:",
                  levels(Visitantes$Aeropuerto)),
      width = 4
    )
  )
  )


server <- function(input, output){
  visitantes1 <- reactive({
    Visitantes %>% 
      filter(
        Aeropuerto == input$Aeropuerto
      )
  })
  
  
  output$Plot1 <- renderPlotly({
    p <- visitantes1() %>% 
      ggplot(aes(x=Periodo, y = Entradas,
                 color = Sexo)) + 
      geom_line()
    
    p
  })
}

shinyApp(ui,server)























ui <- dashboardPage(
  dashboardHeader(title= "My Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("Correlation_Plot"),width = 8),
    box(
      selectInput("features","Features: ",
                  c("Sepal.Width","Petal.Length","Petal.Width")),
      width = 4
    )
  )
)

server <- function(input, output){
  output$Correlation_Plot <- renderPlot({
    plot(iris$Sepal.Length,iris[[input$features]],
         xlab = "Sepal Lenth", ylab = "Feature")
  })
}

shinyApp(ui,server)







server <- function(input, output, session) {
  observe({
    
  })
  
}

ui <- fluidPage(theme = shinytheme("superhero"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "Aeropuerto",
                                label = "Selecciona el Aeropuerto",
                                choices = levels(Visitantes$Aeropuerto),
                                selected = "Acapulco, Gro."
                                )
                  ),
                  mainPanel(  
                    tabsetPanel(
                    tabPanel("Tab1", "First Tab"),
                    tabPanel("Tab2", "Second Tab"),
                    tabPanel("Tab3", "Third Tab")
                  )
                  )
                  
                )
)


shinyApp(ui, server)


server <- function(input, output, session) {}

ui <- fluidPage(
  
  titlePanel("using Tabsets"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica", "Aquí van gráficas bonitas"),
        tabPanel("Pronosticos", "Aquí van pronosticos bonitos"),
        tabPanel("Blabla", "Aquí va blabla")
      )
    )
  )

shinyApp(ui, server)