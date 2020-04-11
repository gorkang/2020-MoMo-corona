
# Libraries ---------------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(shiny)


# Data preparation --------------------------------------------------------

# DOWNLOAD DATA FROM: https://momo.isciii.es/public/momo/dashboard/momo_dashboard.html#datos
DF_raw = read_csv("data/data.zip", guess_max = 10000)

# Data from: https://github.com/gorkang/2020-corona/
# DF_JHU = read_csv(here::here("outputs/raw_JH.csv")) %>% 
DF_JHU = read_csv(here::here("outputs/raw_JH_spain.csv"))
    # filter(Country == "Spain") %>% 
    # select(Date, Deaths) %>% 
    # rename(fecha_defuncion = Date)
    # write_csv("raw_JH_spain.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MoMo + COVID"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
            sliderInput("days_average",
                        "Promedio ___ dias:",
                        min = 1,
                        max = 10,
                        value = 5),
            
            sliderInput("filter_last_days",
                        "Filtrar ultimos ___ dias:",
                        min = 0,
                        max = 10,
                        value = 3),
            
            sliderInput("lag_COVID",
                        "Retraso reporte COVID:",
                        min = 0,
                        max = 10,
                        value = 3)
            
        ),

        # days_average = 7
        # filter_last_days = 0
        # lag_COVID = 3
        
        # Show a plot of the generated distribution
        mainPanel(
            p(HTML(
                paste0(
                    a("Johns Hopkins Data", href="https://covid19api.com/", target = "_blank"),
                    "<BR>", a("MoMo", href="https://momo.isciii.es/public/momo/dashboard/momo_dashboard.html#datos", target = "_blank")
                )
            )
            ),
            
            hr(),
            
           plotOutput("distPlot", height = "800px", width = "100%"),
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    DF_plot = reactive({
        
        DF_JHU = DF_JHU %>% 
        mutate(fecha_defuncion = as.Date(fecha_defuncion) - input$lag_COVID,
               defunciones_COVID = Deaths - lag(Deaths))
        
        
        DF = DF_raw %>% 
            # Filters
            filter(ambito == "nacional") %>% 
            filter(cod_sexo == "all") %>% 
            filter(cod_gedad == "all") %>% 
            filter(fecha_defuncion < max(fecha_defuncion) - input$filter_last_days) %>%
            
            # mutate(DIFF = defunciones_observadas - defunciones_esperadas) %>% 
            select(fecha_defuncion, defunciones_observadas, defunciones_esperadas) %>% 
            left_join(DF_JHU, by = "fecha_defuncion") %>% 
            rowwise() %>% 
            mutate(defunciones_COVID = sum(defunciones_esperadas, defunciones_COVID, na.rm = TRUE))
        
        
        DF_plot = tibble(
            fecha_defuncion = DF$fecha_defuncion,
            esperadas = frollmean(DF[, "defunciones_esperadas"], input$days_average) %>% unlist(),
            observadas = frollmean(DF[, "defunciones_observadas"], input$days_average) %>% unlist(),
            COVID_mas_esperadas = frollmean(DF[, "defunciones_COVID"], input$days_average) %>% unlist()
            
        ) %>% 
            group_by(fecha_defuncion) %>% 
            summarise(esperadas = sum(esperadas, na.rm = FALSE),
                      observadas = sum(observadas, na.rm = FALSE),
                      COVID_mas_esperadas = sum(COVID_mas_esperadas, na.rm = FALSE)) %>% 
            pivot_longer(esperadas:COVID_mas_esperadas)
        
        DF_plot
        
        
    })
    
    output$distPlot <- renderPlot({

        ggplot(DF_plot(), aes(fecha_defuncion, value, color = name)) +
            geom_line() +
            theme_minimal(base_size = 14) +
            scale_color_hue(l = 50) +
            scale_y_continuous(limits = c(600, max(DF_plot()$value, na.rm = TRUE)), 
                               labels = seq(from = 600, to = max(DF_plot()$value, na.rm = TRUE) + 100, by = 200),
                               breaks = seq(from = 600, to = max(DF_plot()$value, na.rm = TRUE) + 100, by = 200)) +
            theme(legend.position = "bottom", legend.title = element_blank()) +
            labs(title = "Datos MoMo combinados con COVID (JHU)",``
                caption = paste0("Promedio de los ultimos ", input$days_average, " dias\n",
                                  "Filtrando los ultimos ", input$filter_last_days, " dias por el retraso en el reporte\n",
                                  "Fechas COVID se mueven atras ", input$lag_COVID, " dias\n",
                                  "@gorkang"),
                 x = "fecha defuncion",
                 y = "muertes")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
