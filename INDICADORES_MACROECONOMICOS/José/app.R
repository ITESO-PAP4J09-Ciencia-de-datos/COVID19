
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

IMSS <- read_excel("../BD_INDICADORES_MACROECONOMICOS.xlsx", sheet = "IMSS")
colnames(IMSS) <- c("Division","Grupo","Fracci�n","Fecha","Trabajadores_Asegurados","Trabajadores_Permanentes","Trabajadores_Eventuales")

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
            plotOutput(outputId = "plot")
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
            # ggplot(aes_string(x = IMSS$Fecha, y = IMSS$Trabajadores_Asegurados)) +
            # geom_line(size = 1, color = "orchid3")
            # model(arima=ARIMA(Trabajadores_Asegurados)) %>%
            # forecast(h=5) %>%
            
            # output$plot <- renderPlot({
            #     IMSS_tsbl %>% 
            #         filter(Division == input$Division, Grupo == input$Grupo, Fracción == input$Fracción) %>% 
            #         # ggplot(aes_string(x = IMSS$Fecha, y = IMSS$Trabajadores_Asegurados)) +
            #         # geom_line(size = 1, color = "orchid3")
            #         # model(arima=ARIMA(Trabajadores_Asegurados)) %>%
        #         # forecast(h=5) %>%
        #         ggplot(aes(x = Fecha, y = Trabajadores_Asegurados,
        #                    color = Fracción)) +
        #         geom_line() +
        #         geom_hline(yintercept = 0,
        #                    linetype = "dashed",
        #                    color = "firebrick") +
        #         annotate("text", label = "Línea base", 
        #                  x = last(IMSS_tsbl$Fecha)-5, y = 0.05, 
        #                  size = 3, color = "firebrick") + 
        #         guides(color = guide_legend(title = NULL)) +
        #         # scale_y_continuous(labels = scales::percent )+
        #         ggtitle(paste0("Aseguramiento ",input$Grupo,
        #                        ", ", input$Division))
        
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
            # scale_y_continuous(labels = scales::percent )+
            ggtitle(paste0("Aseguramiento ",input$Grupo,
                           ", ", input$Division))
        
        # # output$plot <- renderPlot({
        # #     IMSS_tsbl %>%
        # #         filter(Division == input$Division, Grupo == input$Grupo, Fracción == input$Fracción) %>%
        # #         # ggplot(aes_string(x = IMSS$Fecha, y = IMSS$Trabajadores_Asegurados)) +
        # #         # geom_line(size = 1, color = "orchid3")
        # #         # model(arima=ARIMA(Trabajadores_Asegurados)) %>%
        # #         # forecast(h=5) %>%
        # #         ggplot(aes(x = Fecha, y = Trabajadores_Asegurados,
        # #                    color = Fracción)) +
        # #         geom_line() +
        # #         geom_hline(yintercept = 0,
        # #                    linetype = "dashed",
        # #                    color = "firebrick") +
        # #         annotate("text", label = "Línea base",
        # #                  x = last(IMSS_tsbl$Fecha)-5, y = 0.05,
        # #                  size = 3, color = "firebrick") +
        # #         guides(color = guide_legend(title = NULL)) +
        # #         # scale_y_continuous(labels = scales::percent )+
        # #         ggtitle(paste0("Aseguramiento ",input$Grupo,
        # #                        ", ", input$Division))
        # #
        # # ggplot(aes(x = Fecha, y = Trabajadores_Asegurados,
        # #            color = Fracción)) +
        # #     geom_line() +
        # #     geom_hline(yintercept = 0,
        # #                linetype = "dashed",
        # #                color = "firebrick") +
        # #     annotate("text", label = "Línea base",
        # #              x = last(IMSS_tsbl$Fecha)-5, y = 0.05,
        # #              size = 3, color = "firebrick") +
        # #     guides(color = guide_legend(title = NULL)) +
        # #     # scale_y_continuous(labels = scales::percent )+
        # #     ggtitle(paste0("Aseguramiento ",input$Grupo,
        # #                    ", ", input$Division))
        # #
        # # plot_var <- ggplot(aes(x = Fecha, y = Trabajadores_Asegurados,
        # #            color = Fracción)) +
        # #     geom_line() +
        # #     geom_hline(yintercept = 0,
        # #                linetype = "dashed",
        # #                color = "firebrick") +
        # #     annotate("text", label = "Línea base",
        # #              x = last(IMSS_tsbl$Fecha)-5, y = 0.05,
        # #              size = 3, color = "firebrick") +
        # #     guides(color = guide_legend(title = NULL)) +
        # #     # scale_y_continuous(labels = scales::percent )+
        # #     ggtitle(paste0("Aseguramiento ",input$Grupo,
        # #                    ", ", input$Division))
        # #plot_var
        # 
        #     # model(arima=ARIMA(Trabajadores_Asegurados)) %>%
        #     # forecast(h=5)
        # 
        # # test <- test %>% filter(Division == input$Division, Grupo == input$Grupo, Fracción == input$Fracción)
        # # test_arima <- test %>%
        # #     model(arima=ARIMA(Trabajadores_Asegurados)) %>%
        # #     forecast(h=5)
        # #
        # # test_arima %>% autoplot(test)

        
    })
    
}

shinyApp(ui, server)
