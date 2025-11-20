library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

# ======================
#   PREPARAR LOS DATOS
# ======================


df <- df %>%
  mutate(
    Date = as.Date(Date),
    year_month = floor_date(Date, "month")
  )

data_monthly <- df %>%
  group_by(Country, City, year_month) %>%
  summarise(
    pm25 = mean(`PM2.5`, na.rm = TRUE),
    pm10 = mean(PM10, na.rm = TRUE),
    no2  = mean(NO2, na.rm = TRUE),
    so2  = mean(SO2, na.rm = TRUE),
    co   = mean(CO, na.rm = TRUE),
    o3   = mean(O3, na.rm = TRUE),
    temp = mean(Temperature, na.rm = TRUE),
    hum  = mean(Humidity, na.rm = TRUE),
    wind = mean(Wind.Speed, na.rm = TRUE),
    .groups = "drop"
  )


# ======================
#         UI
# ======================

ui <- fluidPage(
  titlePanel("Dashboard – Calidad del Aire Global"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "País:", 
                  choices = sort(unique(df$Country))),
      
      uiOutput("city_selector"),
      
      selectInput("pollutant", "Contaminante:",
                  choices = c("PM2.5" = "pm25",
                              "PM10" = "pm10",
                              "NO2"  = "no2",
                              "SO2"  = "so2",
                              "CO"   = "co",
                              "O3"   = "o3"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tendencias", plotlyOutput("time_plot")),
        tabPanel("Correlación", plotlyOutput("corr_plot"))
      )
    )
  )
)


# ======================
#      SERVER
# ======================

server <- function(input, output, session){
  
  # selector dinámico de ciudades
  output$city_selector <- renderUI({
    cities <- df %>% 
      filter(Country == input$country) %>%
      pull(City) %>% unique()
    
    selectInput("city", "Ciudad:", choices = cities)
  })
  
  # datos filtrados 
  filtered <- reactive({
    data_monthly %>%
      filter(Country == input$country,
             City == input$city)
  })
  
  # gráfico temporal
  output$time_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    
    plot_ly(
      data = filtered(),
      x = ~year_month,
      y = ~get(input$pollutant),
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(title = paste("Evolución de", input$pollutant))
  })
  
  # correlación con temperatura
  output$corr_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    
    plot_ly(
      filtered(),
      x = ~temp,
      y = ~get(input$pollutant),
      type = "scatter",
      mode = "markers"
    ) %>%
      layout(title = paste(input$pollutant, "vs Temperatura"))
  })
  
}

# correr app
shinyApp(ui, server)
