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


#paises <- global_economy %>% distinct(Country) %>% pull() %>%
#    as.character()

#indicadores <- names(global_economy)[-c(1:3)]

IMSS <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/COVID19/INDICADORES_MACROECONOMICOS/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS")
colnames(IMSS) <- c("Division","Grupo","Fracción","Fecha","Trabajadores_Asegurados","Trabajadores_Permanentes","Trabajadores_Eventuales")

IMSS_tbl <- IMSS %>% 
    mutate(Fecha = yearmonth(Fecha))

IMSS_tsbl <- IMSS_tbl %>% 
    as_tsibble(
        index = Fecha,
        key   = c(Division, Grupo, Fracción) 
    )

IMSS_tsbl <- IMSS_tsbl %>% mutate_if(is.character,as_factor) 

models <- c("ETS", "ARIMA")

#IMSS$Grupo %in% IMSS$Division

ui <- fluidPage(theme = shinytheme("spacelab"),
    titlePanel("Trabajadores asegurados IMSS"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Division",
                        label = "Selecciona la division",
                        choices = unique(IMSS$Division),
                        selected = "Transportes y comunicaciones"
            ),
            uiOutput(outputId = "Grupo"),
            uiOutput(outputId = "Fracción")
        ),
        mainPanel(
             plotOutput(outputId = "plot"),
             plotOutput(outputId= "plot2")
        )
    )
)

server <- function(input, output, session) {
    
    output$Grupo <- renderUI({
    selectInput(inputId = "Grupo",
                label = "Selecciona el grupo",
                choices = IMSS %>% filter(Division == input$Division) %>%
                    distinct(Grupo)%>% pull(),
                selected = "Agricultura"
    )
        })
    
    output$Fracción <- renderUI({
    selectInput(inputId = "Fracción",
                label = "Selecciona la fracción",
                choices = IMSS %>%  filter(Grupo == input$Grupo) %>% 
                    distinct(Fracción) %>% pull(),
                selected = "Agricultura"
    )
        })
    
    output$plot <- renderPlot({
        IMSS_tsbl %>% 
            filter(Division == input$Division, Grupo == input$Grupo, Fracción == input$Fracción) %>% 
        ggplot(aes(x = Fecha, y = Trabajadores_Asegurados,
                   color = Fracción)) +
            geom_line() +
            geom_hline(yintercept = 0,
                       linetype = "dashed",
                       color = "firebrick") +
            annotate("text", label = "Línea base", 
                     x = last(IMSS_tsbl$Fecha)-5, y = 0.05, 
                     size = 3, color = "firebrick") + 
            guides(color = guide_legend(title = NULL)) +
            ggtitle(paste0("Aseguramiento ",input$Grupo,
                           ", ", input$Division))
      
    })
      
      output$plot2 <- renderPlot({ 
        IMSS_tsbl1 <- IMSS_tsbl %>%filter(Division == input$Division, Grupo == input$Grupo, Fracción == input$Fracción)
        IMSS_tsbl_fore <- IMSS_tsbl1 %>%
          model(arima=ARIMA(Trabajadores_Asegurados),ets=ETS(Trabajadores_Asegurados)) %>%
          forecast(h=12)
        
          IMSS_tsbl_fore %>% autoplot(IMSS_tsbl,level=NULL)
      })
        

    
}

shinyApp(ui, server)







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




Patrones <- read_excel("C:/Users/JoseGallardo/Documents/Ajolotec/PAP/COVID19/INDICADORES_MACROECONOMICOS/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "Patrones")
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
                selected = "Tamaño del patron"
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
      annotate("text", label = "Línea base", 
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


