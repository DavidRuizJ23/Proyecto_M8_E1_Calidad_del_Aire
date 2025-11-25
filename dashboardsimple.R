# ===============================
# Dashboard – Calidad del Aire Global
# ===============================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
library(shinycssloaders)
library(scales)
library(tibble)
library(DT)
library(leaflet)
library(sf)
library(rnaturalearth)

# ---------- Estilos ----------
col_line <- "#2E86DE"  # azul principal
col_ma   <- "#3C7D22"  # verde media móvil (no usada ahora)
col_ref  <- "#C00000"  # línea de referencia
SHOW_MA3 <- FALSE      # media móvil
SHOW_MED <- TRUE       # mediana

# ---------- Función categoría PM2.5 ----------
pm25_to_aqi_cat <- function(pm25) {
  if (is.na(pm25)) return(NA_character_)
  if (pm25 <= 12) return("Bueno")
  if (pm25 <= 35.4) return("Moderado")
  if (pm25 <= 55.4) return("Dañino para grupos sensibles")
  if (pm25 <= 150.4) return("Dañino")
  if (pm25 <= 250.4) return("Muy dañino")
  return("Peligroso")
}

# ---------- Datos ----------
load("df.RData")

df <- df %>%
  mutate(
    Date       = as.Date(Date),
    year_month = floor_date(Date, "month")
  )

data_monthly <- df %>%
  group_by(Country, City, year_month) %>%
  summarise(
    pm25 = mean(⁠ PM2.5 ⁠, na.rm = TRUE),
    pm10 = mean(PM10,     na.rm = TRUE),
    no2  = mean(NO2,      na.rm = TRUE),
    so2  = mean(SO2,      na.rm = TRUE),
    co   = mean(CO,       na.rm = TRUE),
    o3   = mean(O3,       na.rm = TRUE),
    temp = mean(Temperature, na.rm = TRUE),
    hum  = mean(Humidity,    na.rm = TRUE),
    wind = mean(Wind.Speed,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Country, City, year_month)

# Agregado por país (para comparación entre países)
country_monthly <- data_monthly %>%
  group_by(Country, year_month) %>%
  summarise(
    pm25 = mean(pm25, na.rm = TRUE),
    pm10 = mean(pm10, na.rm = TRUE),
    no2  = mean(no2,  na.rm = TRUE),
    so2  = mean(so2,  na.rm = TRUE),
    co   = mean(co,   na.rm = TRUE),
    o3   = mean(o3,   na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    hum  = mean(hum,  na.rm = TRUE),
    wind = mean(wind, na.rm = TRUE),
    .groups = "drop"
  )

pollutant_labels <- c(
  pm25="PM2.5", pm10="PM10", no2="NO2", so2="SO2", co="CO", o3="O3",
  temp="Temperatura", hum="Humedad", wind="Velocidad del viento"
)

env_choices <- c(
  "Temperatura"          = "temp",
  "Humedad"              = "hum",
  "Velocidad del viento" = "wind"
)

# ===== Datos para mapa =====
country_yearly <- data_monthly %>%
  mutate(year = lubridate::year(year_month)) %>%
  group_by(Country, year) %>%
  summarise(across(pm25:wind, ~ mean(.x, na.rm = TRUE)), .groups = "drop")

country_avg_all <- country_yearly %>%
  group_by(Country) %>%
  summarise(across(pm25:wind, ~ mean(.x, na.rm = TRUE)), .groups = "drop")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

centroids <- st_centroid(world) %>%
  st_transform(crs = 4326) %>%
  select(admin, geometry) %>%
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()

map_data <- country_avg_all %>%
  left_join(centroids, by = c("Country" = "admin")) %>%
  mutate(
    popup = paste0(
      "<b>", Country, "</b><br/>",
      "PM2.5: ", round(pm25,1), " µg/m³<br/>",
      "PM10: ", round(pm10,1), " µg/m³<br/>",
      "NO2: ",  round(no2,1),  " µg/m³<br/>",
      "Viento: ", round(wind,1), " m/s"
    )
  )

# ---------- UI ----------
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  sidebar = sidebar(
    width = 320,
    selectInput("country", "País:",
                choices = sort(unique(df$Country)),
                selected = sort(unique(df$Country))[1]),
    uiOutput("city_selector"),
    selectInput("pollutant", "Contaminante:",
                choices = c("PM2.5"="pm25","PM10"="pm10","NO2"="no2",
                            "SO2"="so2","CO"="co","O3"="o3"),
                selected = "pm25"),
    selectInput("env_var", "Variable ambiental:",
                choices = env_choices,
                selected = "temp"),
    hr(),
    selectInput("country_compare", "Países a comparar:",
                choices = sort(unique(df$Country)),
                selected = c("Mexico", "United States"),
                multiple = TRUE),
    textOutput("compare_status"),
    hr(),
    downloadButton("dl_data", "Descargar datos filtrados")
  ),
  
  # ---------- Encabezado personalizado con icono (sin negritas) ----------
  tags$head(
    tags$style(HTML("
      .custom-title {
        text-align: center;
        font-size: 38px;
        font-weight: 400;
        margin-top: 5px;
        margin-bottom: 25px;
        color: #2C3E50;
        letter-spacing: 0.5px;
      }
      .title-icon {
        font-size: 42px;
        margin-right: 10px;
        vertical-align: middle;
      }
    "))
  ),
  
  div(
    class = "custom-title",
    HTML('<span class="title-icon">🌍</span> Dashboard – Calidad del Aire Global')
  ),
  
  # KPIs
  layout_columns(
    col_widths = c(4,4,4),
    card(
      class = "mb-3",
      card_header("Valor actual"),
      card_body(
        div(class = "fs-3 fw-bold", textOutput("kpi_current")),
        div(class = "text-muted", textOutput("kpi_context"))
      )
    ),
    card(
      class = "mb-3",
      card_header("Cambio vs mes previo"),
      card_body(div(class = "fs-3 fw-bold", textOutput("kpi_delta")))
    ),
    card(
      class = "mb-3",
      card_header("Máximo histórico"),
      card_body(div(class = "fs-3 fw-bold", textOutput("kpi_max")))
    )
  ),
  
  # Tabs
  card(
    card_header(
      navset_pill(
        id = "main_tabs",
        nav("Tendencias", withSpinner(plotlyOutput("time_plot", height = 420))),
        nav("Contaminación vs Ambiente",
            withSpinner(plotlyOutput("corr_city_plot", height = 480))),
        nav("Comparación países",
            withSpinner(plotlyOutput("country_compare_plot", height = 480))),
        nav("Mapa", withSpinner(leafletOutput("map_countries", height = 600))),
        nav("Tabla de datos",
            withSpinner(DTOutput("table_city"))),
        nav("Insights",
            withSpinner(
              card(
                class = "p-4",
                card_header("Análisis automático de insights"),
                card_body(
                  div(class = "fs-5", htmlOutput("auto_insights"))
                )
              )
            )
        )
      )
    )
  ),
  
  footer = div(class="small text-muted mt-3",
               "Fuente: global_air_quality_data_10000.csv (demo).")
)

# ---------- SERVER ----------
server <- function(input, output, session){
  
  # selector dinámico de ciudad
  output$city_selector <- renderUI({
    req(input$country)
    cities <- df %>% filter(Country == input$country) %>%
      pull(City) %>% unique() %>% sort()
    selectInput("city", "Ciudad:", choices = cities,
                selected = if (length(cities)) cities[1] else NULL)
  })
  
  # datos filtrados por país / ciudad
  filtered <- reactive({
    req(input$country, input$city)
    data_monthly %>%
      filter(Country == input$country, City == input$city) %>%
      arrange(year_month)
  })
  
  # ---------- Mensaje dinámico países a comparar ----------
  output$compare_status <- renderText({
    req(input$country_compare)
    paste("Comparando:", paste(input$country_compare, collapse = ", "))
  })
  
  # Cambiar automáticamente a pestaña "Comparación países"
  observeEvent(input$country_compare, {
    updateTabsetPanel(
      session,
      inputId = "main_tabs",
      selected = "Comparación países"
    )
  })
  
  # ---------- KPIs ----------
  output$kpi_current <- renderText({
    d <- filtered(); req(nrow(d) > 0)
    y <- d[[input$pollutant]]
    v <- tail(y, 1)
    if (!is.finite(v)) return("s/d")
    if (input$pollutant == "pm25") {
      cat <- pm25_to_aqi_cat(v)
      paste0(round(v, 1), " µg/m³ — ", cat)
    } else {
      round(v, 1)
    }
  })
  
  output$kpi_context <- renderText({
    paste(input$city, "-", input$country)
  })
  
  output$kpi_delta <- renderText({
    d <- filtered(); req(nrow(d) >= 2)
    y <- d[[input$pollutant]]
    cur <- y[length(y)]; prev <- y[length(y)-1]
    if (is.finite(cur) && is.finite(prev) && prev != 0) {
      pct <- (cur - prev) / prev
      paste0(ifelse(pct >= 0, "+", ""), percent(pct, accuracy = 0.1))
    } else "s/d"
  })
  
  output$kpi_max <- renderText({
    d <- filtered(); req(nrow(d) > 0)
    mx <- suppressWarnings(max(d[[input$pollutant]], na.rm = TRUE))
    if (is.finite(mx)) round(mx, 1) else "s/d"
  })
  
  # ---------- Mapa ----------
  output$map_countries <- renderLeaflet({
    md <- map_data %>% filter(!is.na(lat) & !is.na(lon))
    req(nrow(md) > 0)
    
    pal <- colorNumeric("YlOrRd", domain = md$pm25, na.color = "gray")
    
    leaflet(md) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        radius = ~scales::rescale(pm25, to = c(4, 18),
                                  from = range(md$pm25, na.rm = TRUE)),
        color = ~pal(pm25),
        stroke = TRUE, weight = 1,
        fillOpacity = 0.85,
        popup = ~popup,
        label = ~paste0(Country, ": ", round(pm25,1), " µg/m³")
      ) %>%
      addLegend("bottomright", pal = pal, values = ~pm25,
                title = "PM2.5 (promedio)")
  })
  
  # ---------- Tendencia ----------
  output$time_plot <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    ycol <- input$pollutant; ylab <- pollutant_labels[[ycol]]
    d <- d %>% filter(!is.na(.data[[ycol]]))
    yv <- d[[ycol]]
    
    last_x <- tail(d$year_month, 1); last_y <- tail(yv, 1)
    
    plt <- plot_ly() %>%
      add_trace(data = d, x = ~year_month, y = ~yv,
                type = "scatter", mode = "lines+markers",
                name = ylab,
                line = list(width = 3, color = col_line),
                marker = list(size = 6, symbol = "circle-open"),
                hovertemplate = paste0("%{x|%b %Y}<br>", ylab,
                                       ": %{y:.2f}<extra></extra>")
      )
    
    if (SHOW_MED) {
      med_y <- suppressWarnings(median(yv, na.rm = TRUE))
      plt <- plt %>%
        add_trace(data = data.frame(x = range(d$year_month), y = med_y),
                  x = ~x, y = ~y, type = "scatter", mode = "lines",
                  name = "Mediana",
                  line = list(color = col_ref, dash = "dash", width = 2),
                  hoverinfo = "skip")
    }
    
    plt %>%
      add_annotations(x = last_x, y = last_y,
                      text = paste0(round(last_y, 1)),
                      showarrow = TRUE, arrowhead = 2, ax = 20, ay = -25,
                      bgcolor = "rgba(255,255,255,0.85)",
                      bordercolor = col_line) %>%
      layout(
        template = "plotly_white",
        title = list(text = paste("Evolución de", ylab,
                                  "—", input$city, ",", input$country),
                     x = 0.02),
        xaxis = list(
          title = "Mes",
          rangeselector = list(
            buttons = list(
              list(count = 6, label = "6m", step = "month", stepmode = "backward"),
              list(count = 1, label = "1a", step = "year", stepmode = "backward"),
              list(step = "all", label = "Todo")
            ))),
        yaxis = list(title = ylab, zeroline = TRUE, zerolinewidth = 1),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
        margin = list(l = 60, r = 20, b = 60, t = 60),
        hoverlabel = list(bgcolor = "white")
      )
  })
  
  # ---------- Contaminación vs Ambiente ----------
  output$corr_city_plot <- renderPlotly({
    d <- filtered(); req(nrow(d) > 0)
    ycol <- input$pollutant; ylab <- pollutant_labels[[ycol]]
    xcol <- input$env_var;  xlab <- pollutant_labels[[xcol]]
    
    d <- d %>% filter(!is.na(.data[[xcol]]), !is.na(.data[[ycol]]))
    req(nrow(d) > 1)
    
    df_fit <- tibble(x = d[[xcol]], y = d[[ycol]])
    fit <- lm(y ~ x, data = df_fit)
    r_val  <- suppressWarnings(cor(df_fit$x, df_fit$y, use = "complete.obs"))
    r2_val <- summary(fit)$r.squared
    
    xr   <- seq(min(df_fit$x, na.rm = TRUE), max(df_fit$x, na.rm = TRUE), length.out = 100)
    pred <- predict(fit, newdata = data.frame(x = xr))
    
    plot_ly() %>%
      add_trace(data = df_fit, x = ~x, y = ~y,
                type = "scatter", mode = "markers",
                name = "Observaciones",
                marker = list(size = 8, opacity = 0.75,
                              line = list(width = 0.5, color = "#666")),
                hovertemplate = paste0(xlab, ": %{x:.2f}<br>",
                                       ylab, ": %{y:.2f}<extra></extra>")
      ) %>%
      add_trace(x = xr, y = pred,
                type = "scatter", mode = "lines",
                name = "Ajuste lineal",
                line = list(color = col_line, width = 3)
      ) %>%
      layout(
        template = "plotly_white",
        title = paste0(ylab, " vs ", xlab, " — ",
                       input$city, ", ", input$country,
                       if (is.finite(r_val)) paste0(" | r = ", round(r_val, 2)) else "",
                       if (is.finite(r2_val)) paste0(" • R² = ", round(r2_val, 2)) else ""),
        xaxis = list(title = xlab),
        yaxis = list(title = ylab, zeroline = TRUE, zerolinewidth = 1),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
        margin = list(l = 60, r = 20, b = 60, t = 60),
        hoverlabel = list(bgcolor = "white")
      )
  })
  
  # ---------- Comparación países ----------
  output$country_compare_plot <- renderPlotly({
    req(length(input$country_compare) > 0)
    ycol <- input$pollutant; ylab <- pollutant_labels[[ycol]]
    
    d <- country_monthly %>%
      filter(Country %in% input$country_compare) %>%
      filter(!is.na(.data[[ycol]]))
    req(nrow(d) > 0)
    
    plot_ly(
      data = d,
      x    = ~year_month,
      y    = d[[ycol]],
      color = ~Country,
      type  = "scatter",
      mode  = "lines+markers",
      hovertemplate = paste0("%{x|%b %Y}<br>",
                             ylab, ": %{y:.2f}<br>",
                             "País: %{trace.name}<extra></extra>")
    ) %>%
      layout(
        template = "plotly_white",
        title = paste0("Evolución de ", ylab,
                       " entre: ", paste(input$country_compare, collapse = " vs ")),
        xaxis = list(title = "Mes"),
        yaxis = list(title = ylab, zeroline = TRUE, zerolinewidth = 1),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
        margin = list(l = 60, r = 20, b = 60, t = 60),
        hoverlabel = list(bgcolor = "white")
      )
  })
  
  # ---------- Tabla de datos (2 decimales) ----------
  output$table_city <- renderDT({
    d <- filtered(); req(nrow(d) > 0)
    
    tabla <- d %>%
      mutate(Mes = format(year_month, "%Y-%m")) %>%
      select(Mes, pm25, pm10, no2, so2, co, o3, temp, hum, wind) %>%
      rename(
        ⁠ PM2.5 ⁠   = pm25,
        ⁠ PM10 ⁠    = pm10,
        ⁠ NO2 ⁠     = no2,
        ⁠ SO2 ⁠     = so2,
        ⁠ CO ⁠      = co,
        ⁠ O3 ⁠      = o3,
        ⁠ Temp ⁠    = temp,
        ⁠ Humedad ⁠ = hum,
        ⁠ Viento ⁠  = wind
      )
    
    dt <- datatable(
      tabla,
      rownames = FALSE,
      filter   = "top",
      options  = list(
        pageLength = 10,
        autoWidth  = TRUE
      )
    )
    
    dt %>% formatRound(
      columns = c("PM2.5","PM10","NO2","SO2","CO","O3","Temp","Humedad","Viento"),
      digits = 2
    )
  })
  
  # ---------- INSIGHTS AUTOMÁTICOS ----------
  output$auto_insights <- renderUI({
    d <- filtered()
    req(nrow(d) > 5)
    
    ycol <- input$pollutant
    ylab <- pollutant_labels[[ycol]]
    
    # Tendencia simple
    slope <- coef(lm(d[[ycol]] ~ as.numeric(d$year_month)))[2]
    
    tendencia <- if (slope > 0) {
      "una tendencia creciente en los últimos meses"
    } else if (slope < 0) {
      "una tendencia decreciente recientemente"
    } else {
      "un comportamiento relativamente estable"
    }
    
    # Últimos valores
    last_val  <- tail(d[[ycol]], 1)
    prev_val  <- tail(d[[ycol]], 2)[1]
    dif       <- last_val - prev_val
    
    cambio <- if (dif > 0) {
      paste0("un incremento de ", round(dif, 2))
    } else if (dif < 0) {
      paste0("una disminución de ", round(abs(dif), 2))
    } else {
      "sin cambios relevantes"
    }
    
    HTML(paste0(
      "<h4>Insights para ", input$city, ", ", input$country, "</h4>",
      "<ul>",
      "<li>El contaminante <b>", ylab, "</b> muestra ", tendencia, ".</li>",
      "<li>El valor más reciente es <b>", round(last_val, 2), "</b>.</li>",
      "<li>Esto representa ", cambio, " respecto al mes previo.</li>",
      "</ul>",
      "<p><i>💡 Sugerencia:</i> prueba cambiar de contaminante o variable ambiental ",
      "para explorar otros patrones.</p>"
    ))
  })
  
  # ---------- Descarga ----------
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("air_quality_", input$country, "_", input$city, ".csv")
    },
    content = function(file) {
      write_csv(filtered(), file)
    }
  )
}

# ---------- Ejecutar App ----------
shinyApp(ui, server)
