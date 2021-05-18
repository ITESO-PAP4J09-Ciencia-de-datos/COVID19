


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
library(readxl)



IMSS <- read_excel("INDICADORES_MACROECONOMICOS/BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS")

  colnames(IMSS) <- c("Division","Grupo","Fracción","Fecha","Trabajadores_Asegurados","Trabajadores_Permanentes","Trabajadores_Eventuales")

IMSS_tbl <- IMSS %>% 
  
  mutate(Fecha = yearmonth(Fecha), Grupo = if_else(is.na(Grupo), Fracción, Grupo))

#?sprintf()
#group_by(IMSS$Fecha)

IMSS_tsbl <- IMSS_tbl %>% 
  mutate(name_label = sprintf("%.2f", Trabajadores_Asegurados)) %>%
  
         
  as_tsibble(
    index = Fecha,
    key   = c(Division, Grupo, Fracción) 
  )

models <- c("ARIMA")

IMSS_tsbl <- IMSS_tsbl %>% mutate_if(is.character,as_factor) 

#-------------------------------------------------------------------------------
#-----------------------          UI          ----------------------------------
#-------------------------------------------------------------------------------
#?levels()

ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel(tags$div(HTML('</i>
                
    Pronósticos de Empleados Asegurados por el IMSS
    
                                        </i>')),
    windowTitle = "Pronósticos de Empleados Asegurados por el IMSS"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "Division",
                    label = "Selecciona Division",
                    choices = levels(IMSS_tsbl$Division),
                    selected = levels(IMSS_tsbl$Division)
        ),
    
        selectInput(inputId = "Grupo",
                    label = "Selecciona un Grupo",
                    choices = levels(IMSS_tsbl$Grupo),
                    selected = levels(IMSS_tsbl$Grupo)
        ),
    
          selectInput(inputId = "Fracción",
                    label = "Selecciona una Fracción",
                    choices = levels(IMSS_tsbl$Fracción),
                    selected = levels(IMSS_tsbl$Fracción)
        ),
        
        uiOutput(outputId = "Division"),
        
        # checkboxGroupInput(inputId = "Grupo",
        #                    label = "Selecciona el grupo deseado",
        #                    choices = levels(IMSS_tsbl$Grupo),
        #                    selected = levels(IMSS_tsbl$Grupo),
        #                    inline = TRUE
        # # ),
        # # 
        # checkboxGroupInput(inputId = "Fracción",
        #                    label = "Escoge la fraccióm a estimar",
        #                    choices = levels(IMSS_tsbl$Fracción),
        #                    selected = levels(IMSS_tsbl$Fracción),
        #                    inline = TRUE
        # ),
        # checkboxGroupInput(inputId = "modelos",
        #                    label = "Escoge los modelos a estimar",
        #                    choices = models,
        #                    selected = models[1],
        #                    inline = TRUE
        # )
        
      ),
      mainPanel(
        navbarPage(title = "Análisis",
                   # tab 1 - Gráficas ####
                   tabPanel("Gráficas", icon = icon("chart-bar"),
                            plotlyOutput(outputId = "time_plot")
                   ),
                   # tab 2 - Modelos ####
                   tabPanel("Modelos", icon = icon("chess-knight"),
                            wellPanel(
                              uiOutput(outputId = "output_sector"),
                              uiOutput(outputId = "output_radio")
                            ),
                            tabsetPanel(type = "pills",
                                        tabPanel("Reporte", icon = icon("chess-knight"),
                                                 verbatimTextOutput(outputId = "report")
                                        ),
                                        tabPanel("Ajuste vs. entrenamiento", 
                                                 icon = icon("cloud")
                                                 , 
                                                 dataTableOutput("fit_accuracy")
                                        ),
                                        tabPanel("Diagnóstico de residuos", 
                                                 icon = icon("code")
                                                 ,
                                                 plotOutput("resid")
                                        )
                            )
                   ),
                   # tab 3 - Pronósticos ####
                   tabPanel("Pronósticos", icon = icon("cloud"),
                            sliderInput(inputId = "horizonte",
                                        label = "Selecciona el horizonte de pronóstico",
                                        min = 1, max = 60, value = 14
                            ),
                            plotOutput("forecast", width = "100%")
                   )
        ) # navbarPage
        
      ) # main Panel
      
      
    ) # sidebarLayout
) # fluidPage

server <- function(input, output, session) {
  output$region <- renderUI({
    selectInput(inputId = "Division",
                label = "Division",
                choices = IMSS_tsbl %>% filter(Division == input$Division) %>% 
                  distinct(Grupo) %>% pull(),
                selected = IMSS_tsbl %>% filter(Division == input$Division) %>% 
                  distinct(Grupo) %>% pull()
    )
  })
  
  df <- reactive({
    IMSS_tsbl %>% 
      filter(
        Division == input$Division,
        Grupo == input$Grupo,
        Fracción == input$Fracción
        #name %in% c(input$sector)
      )
  })
  
  fit <- reactive({
    df() %>% 
      model(
        SNAIVE = SNAIVE(Trabajadores_Asegurados),
        ETS = ETS(Trabajadores_Asegurados),
        ARIMA = ARIMA(Trabajadores_Asegurados),
        Prophet = prophet(Trabajadores_Asegurados),
        Regression = TSLM(Trabajadores_Asegurados ~ trend() + season()),
        `Harmonic reg.` = ARIMA(Trabajadores_Asegurados ~ fourier(K = 2) + PDQ(0,0,0)),
        `Piecewise reg.` = TSLM(Trabajadores_Asegurados ~ trend(knots = c(ymd("1998-01-01"), 
                                                        ymd("2021-01-01"))) + 
                                  season())
      )
  })
  
  forecast <- reactive({
    fit() %>% 
      fabletools::forecast(h = input$horizonte)
  })
  
  # tab 1- Gráficas ####
  output$time_plot <- renderPlotly({
    p <- df() %>% 
      ggplot(aes(x = Fecha, y = Trabajadores_Asegurados,
                 color = Fracción,
                 label = name_label)) +
      geom_line() +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 color = "firebrick") +
      annotate("text", label = "Línea base", 
               x = last(IMSS_tsbl$Fecha)-5, y = 0.05, 
               size = 3, color = "firebrick") + 
      guides(color = guide_legend(title = NULL)) +
      # scale_y_continuous(labels = scales::percent )+
      ggtitle(paste0("Aseguramiento ",input$Grupo,
                     ", ", input$Division))
    
    ggplotly(p, tooltip = c("x", "label", "color"))
  })
  
  # tab 2 - Modelos ####
  output$output_sector <- renderUI({
    radioButtons(inputId = "radio_series",
                 label = "La serie a reportar",
                 choices = input$Fracción,
                 selected = input$Fracción[1],
                 inline = TRUE
    )
  })
  
  output$output_radio <- renderUI({
    radioButtons(inputId = "radio_models",
                 label = "El modelo a reportar",
                 choices = input$modelos,
                 selected = input$modelos[1],
                 inline = TRUE
    )
  })
  
  output$report <- renderPrint({
    fit() %>% 
      select(input$radio_models) %>%
      filter(Fracción == input$radio_series) %>% 
      report()
  })
  
  output$fit_accuracy <- renderDataTable({
    fit() %>% 
      select(1:3,input$modelos) %>% 
      accuracy()
  })
  
  output$resid <- renderPlot({
    fit() %>% 
      select(input$radio_models) %>% 
      filter(Fracción == input$radio_series) %>% 
      gg_tsresiduals() +
      ggtitle(paste0("Diagnóstico de residuos para el modelo ", input$radio_models, 
                     " de la serie ", input$radio_series, " en ", 
                     input$Grupo, ", ", input$Division))
  })
  
  # tab 3 - Pronósticos ####
  
  output$forecast <- renderPlot({
    forecast() %>% 
      filter(.model %in% input$modelos) %>% 
      autoplot(df(), size = 1,
               level = if (length(input$modelos) == 1) {c(80,95)} else {NULL}
      ) +
      geom_hline(yintercept = 0,
                 linetype = "dashed", 
                 color = "firebrick") +
      annotate("text", label = "Línea base", 
               x = last(IMSS_tsbl$Fecha)-5, y = 0.05, 
               size = 3, color = "firebrick") +
      # scale_y_continuous(labels = scales::percent)+
      labs(x = "Fecha", y = "Cambio %")
    
    
  }, height = function(){300* length(input$sector)})
  
}

shinyApp(ui, server)
