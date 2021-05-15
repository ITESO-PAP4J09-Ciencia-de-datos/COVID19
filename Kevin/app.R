library(shiny)
library(tidyverse)
library(fpp3)
library(shinythemes)
library(tsibble)
library(readxl)
library(shiny)
library(shinythemes)
library(urca)
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(plotly)
library(patchwork)
library(lubridate)
library(fable.prophet)




Patrones <- read_excel("BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "Patrones")
colnames(Patrones) <- c("Entidad", "Fecha", "Agrupacion","cve2","Patrones num")

Patrones_tbl <- Patrones %>%
  mutate(Fecha = yearmonth(as.character(Fecha)))



Patrones_tsbl <- Patrones_tbl %>% 
  as_tsibble(
    index = Fecha,
    key   = c(Entidad, Agrupacion, cve2) 
  )

Patrones_tsbl <- Patrones_tsbl %>% mutate_if(is.character,as_factor) 



models <- c("ETS", "ARIMA")

#Patrones$Agrupacion %in% Patrones$Entidad

ui <- fluidPage(theme = shinytheme("spacelab"),
                titlePanel("Trabajadores asegurados Patrones"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "Entidad",
                                label = "Selecciona la Entidad",
                                choices = unique(Patrones$Entidad),
                                selected = "Jalisco"
                    ),
                    uiOutput(outputId = "Agrupacion"),
                    uiOutput(outputId = "cve2")
                  ),
                  mainPanel(
                    plotOutput(outputId = "plot"),
                    plotOutput(outputId= "plot2")
                  )
                )
)

server <- function(input, output, session) {
  
  output$Agrupacion <- renderUI({
    selectInput(inputId = "Agrupacion",
                label = "Selecciona el Agrupacion",
                choices = Patrones %>% filter(Entidad == input$Entidad) %>%
                  distinct(Agrupacion)%>% pull(),
                selected = "Tama?o del patron"
    )
  })
  
  output$cve2 <- renderUI({
    selectInput(inputId = "cve2",
                label = "Selecciona la cve2",
                choices = Patrones %>%  filter(Agrupacion == input$Agrupacion) %>% 
                  distinct(cve2) %>% pull(),
                selected = "Un Puesto De Trabajo"
    )
  })
  
  
  output$plot <- renderPlot({
    Patrones_tsbl %>% 
      filter(Entidad == input$Entidad, Agrupacion == input$Agrupacion, cve2 == input$cve2) %>% 
      ggplot(aes(x = Fecha, y = `Patrones num`,
                 color = cve2)) +
      geom_line() +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 color = "firebrick") +
      annotate("text", label = "L?nea base", 
               x = last(Patrones_tsbl$Fecha)-5, y = 0.05, 
               size = 3, color = "firebrick") + 
      guides(color = guide_legend(title = NULL)) +
      ggtitle(paste0("Aseguramiento ",input$Agrupacion,
                     ", ", input$Entidad))
    
  })
  
  output$plot2 <- renderPlot({ 
    Patrones_tsbl1 <-  Patrones_tsbl %>% 
      filter(Entidad == input$Entidad, Agrupacion == input$Agrupacion, cve2 == input$cve2) 
    Patrones_fore <- Patrones_tsbl1 %>%
      model(arima=ARIMA(`Patrones num`),ets=ETS(`Patrones num`)) %>%
      forecast(h=12)
    
    Patrones_fore %>% autoplot(Patrones_tsbl1,level=NULL)
  })
  
  
}

shinyApp(ui, server)