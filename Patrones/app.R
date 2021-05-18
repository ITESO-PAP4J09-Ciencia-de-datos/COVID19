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



IMSS <- read_excel("INDICADORES_MACROECONOMICOS/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS")
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


# patrones <- read_excel("INDICADORES_MACROECONOMICOS/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS", sheet = "IMSS")
# colnames(patrones) <- c("Entidad","Fecha","Agruapacion","cve2","Patrones")
# 
# patrones_tbl <- patrones %>% 
#     mutate(Fecha = yearmonth(Fecha))
# 
# patrones_tsbl <- patrones_tbl %>% 
#     as_tsibble(
#         index = Fecha,
#         key   = c(Entidad, Agruapacion, cve2) 
#     )
# 
# IMSS_tsbl <- IMSS_tsbl %>% mutate_if(is.character,as_factor) 
# 
# models <- c("ETS", "ARIMA")
# 
# 
# ui <- fluidPage(theme = shinytheme("spacelab"),
#                 titlePanel("Patrones"),
#                 
#                 sidebarLayout(
#                     sidebarPanel(
#                         selectInput(inputId = "Agruapacion",
#                                     label = "Selecciona la Agrupación",
#                                     choices = unique(IMSS$Division),
#                                     selected = "Tamaño del patron"
#                         ),
#                         uiOutput(outputId = "Grupo"),
#                         uiOutput(outputId = "Fracción")
#                     ),
#                     mainPanel(
#                         plotOutput(outputId = "plot")
#                     )
#                 )
# )
# 
# server <- function(input, output, session) {
#     
#     output$Grupo <- renderUI({
#         selectInput(inputId = "Grupo",
#                     label = "Selecciona el grupo",
#                     choices = IMSS %>% filter(Division == input$Division) %>%
#                         distinct(Grupo)%>% pull(),
#                     selected = "Agricultura"
#         )
#     })
#     
#     output$Fracción <- renderUI({
#         selectInput(inputId = "Fracción",
#                     label = "Selecciona la fracción",
#                     choices = IMSS %>%  filter(Grupo == input$Grupo) %>% 
#                         distinct(Fracción) %>% pull(),
#                     selected = "Agricultura"
#         )
#     })
#     
#     output$plot <- renderPlot({
#         IMSS_tsbl %>% 
#             filter(Division == input$Division, Grupo == input$Grupo, Fracción == input$Fracción) %>% 
# 
#         
#         ggplot(aes(x = Fecha, y = Trabajadores_Asegurados,
#                    color = Fracción)) +
#             geom_line() +
#             geom_hline(yintercept = 0,
#                        linetype = "dashed",
#                        color = "firebrick") +
#             annotate("text", label = "Línea base", 
#                      x = last(IMSS_tsbl$Fecha)-5, y = 0.05, 
#                      size = 3, color = "firebrick") + 
#             guides(color = guide_legend(title = NULL)) +
#             # scale_y_continuous(labels = scales::percent )+
#             ggtitle(paste0("Aseguramiento ",input$Grupo,
#                            ", ", input$Division))
# 
#         
#     })
#     
# }
# 
# shinyApp(ui, server)