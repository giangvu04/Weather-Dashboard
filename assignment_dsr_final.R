library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(httr)
library(urltools)
library(lubridate)
library(plotly)
library(dplyr)

Sys.setlocale(locale = "C")

css <- "
body, .content-wrapper, .main-sidebar, .right-side {
  background: linear-gradient(120deg, #f7fbff 0%, #e3f2fd 100%);
  font-family: 'Montserrat', 'Roboto', Arial, sans-serif !important;
}
.box, .box-solid, .box-primary, .box-info {
  border-radius: 22px !important;
  box-shadow: 0 4px 20px rgba(33,44,80,0.07);
  background: #fff !important;
  overflow: hidden;
  border: none;
}
.tab-content, .tab-pane, .main-sidebar {
  background: transparent !important;
}
.avatar-img {
  width: 50px; height: 50px; border-radius: 50%; object-fit: cover; margin-right: 10px;
  border: 2px solid #9ed0fc;
  box-shadow: 0 0 7px #b0e0e6;
}

.weather-detail-flex {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: stretch;
  gap: 30px;
  width: 100%;
  margin-top: 10px;
}
.weather-detail-main {
  flex: 1.2;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  min-width: 220px;
  max-width: 320px;
  margin-right: 10px;
}
.weather-detail-main img {
  margin-bottom: 10px;
}
.weather-detail-main h2 {
  color: #38b000;
  font-weight: bold;
  font-size: 36px;
  margin-bottom: 7px;
}
.weather-detail-main .main-feel {
  color: #666;
  font-size: 18px;
  margin-bottom: 5px;
}
.weather-detail-main .main-condition {
  margin-top: 8px;
  font-size: 21px;
  color: #2980b9;
  font-weight: 600;
}
.weather-detail-extra {
  flex: 2;
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 18px;
  align-items: center;
  justify-items: center;
  padding: 22px 18px 0 18px;
}
.weather-extra-item {
  width: 97%;
  background: #eaf6fd;
  border-radius: 16px;
  padding: 16px 10px 12px 18px;
  box-shadow: 0 1px 5px #dbeafe80;
  color: #22223b;
  font-size: 19px;
  font-weight: 600;
  display: flex;
  align-items: center;
  gap: 12px;
  min-width: 120px;
  transition: box-shadow 0.2s;
}
.weather-extra-item strong {
  margin-left: 5px;
  color: #0b2545;
  font-weight: bold;
  word-break: break-word;
}
.weather-extra-item .fa-tint   { color: #38a3a5; }
.weather-extra-item .fa-wind   { color: #4361ee; }
.weather-extra-item .fa-eye    { color: #ffb703; }
.weather-extra-item .fa-gauge  { color: #6930c3; }

@media (max-width: 900px) {
  .weather-detail-flex { flex-direction: column; gap:10px;}
  .weather-detail-extra { grid-template-columns: 1fr; padding: 10px 0 0 0; }
}
.weather-main-temp {
  color: #38b000;
  font-weight: 700;
  font-size: 36px;
  margin-bottom: 4px;
}
.weather-main-feel {
  font-size: 17px;
  color: #444;
  margin-bottom: 6px;
}

.time-pill {
  background: linear-gradient(90deg,#37b3ed22 0%,#fd7e1422 100%);
  color: #228be6;
  font-weight: 700;
  font-size: 22px;
  border-radius: 18px;
  padding: 5px 26px;
  box-shadow: 0 2px 10px #d0ebff44;
  margin-top: 2px;
}
.sidebar-menu .menu-item > a, .sidebar-menu .menu-item {
  color: #2c3e50 !important;
  font-size: 16.5px !important;
}
.sidebar-menu .active > a, .sidebar-menu .active {
  background: #228be6 !important;
  color: #fff !important;
  border-radius: 13px;
}
h2, h1, h3 {
  color: #212529;
  font-weight: 700;
  letter-spacing: 0.2px;
}
.box-title {
  font-weight: 700 !important;
  font-size: 22px !important;
  color: #fff !important;
  display: flex !important;
  align-items: center;
  justify-content: center;
  gap: 9px;
  letter-spacing: 0.3px;
  min-height: 40px;
}
.box-primary > .box-header, .box-info > .box-header {
  background: linear-gradient(90deg,#0093e9 65%,#80d0c7 100%) !important;
  color: #fff !important;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 9px;
  font-size: 22px !important;
  font-weight: 700 !important;
  border-radius: 22px 22px 0 0 !important;
  min-height: 46px;
}
.weather-chart {
  width: 100%;
  height: 300px;
  margin-top: 20px;
}
"

# --------------------- UI ---------------------------------------
header <- dashboardHeader(title = "Weather Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'Menu',
    menuItem(
      tags$div(
        style = "display: flex; align-items: center;",
        tags$img(src = "GiangNek.jpg", class="avatar-img"),
        "Vu Huong Giang"
      ),
      tabName = "tab1"
    ),
    menuItem('Enter city name', tabName = 'tab2', icon = icon('magnifying-glass'),
             textInput("cityNameInput", "Enter City Name"),
             actionButton("getWeatherButton", "Get Weather")),
    menuItem('Weather', tabName = 'tab3', icon = icon('cloud-sun')),
    menuItem('Forecast', tabName = 'tab4', icon = icon('chart-line'))
  )
)

body <- dashboardBody(
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css?family=Montserrat:400,700|Roboto:400,700&display=swap"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML(css))
  ),
  tabItems(
    tabItem(tabName = 'tab3',
            fluidRow(
              column(
                width = 8,
                box(
                  leafletOutput("weatherMap", width = "100%", height = "410px"),
                  title = tagList(icon("map-location-dot"), "Weather Map"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  class = "animate__animated animate__fadeInLeft"
                )
              ),
              column(
                width = 4,
                box(
                  div(
                    style = "height:410px; display:flex; flex-direction:column; justify-content:center; align-items:center; padding:0 12px;",
                    h2(textOutput("locationDateOnly"), style = "font-weight:700; color:#0b2545; font-size:24px; margin-bottom:22px;"),
                    div(
                      style = "display:flex; flex-direction:column; align-items:center; margin-bottom:22px;",
                      tags$i(class = "fas fa-calendar-day", style="font-size:30px; color:#37b3ed; margin-bottom:6px;"),
                      span(textOutput("currentDate"), style="font-size:20px; color:#454545; font-weight:500;")
                    ),
                    div(
                      style="display:flex; flex-direction:column; align-items:center;",
                      tags$i(class = "fas fa-clock", style="font-size:28px; color:#fd7e14; margin-bottom:5px;"),
                      div(class="time-pill", textOutput("currentTime"))
                    )
                  ),
                  title = tagList(icon("location-dot"), "Location & Date"),
                  status = "info", solidHeader = TRUE, width = 12,
                  class = "animate__animated animate__fadeInUp"
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                box(
                  uiOutput("weatherInfo"),
                  title = tagList(icon("cloud-sun-rain"), "Weather Detail"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  class = "animate__animated animate__fadeInRight"
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                box(
                  div(class="weather-chart", plotlyOutput("tempHumidityPlot", height = "300px")),
                  title = tagList(icon("chart-line"), "Temperature vs Humidity"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  class = "animate__animated animate__fadeInUp"
                )
              )
            )
    ),
    tabItem(tabName = 'tab4',
            h1(textOutput("forecastTitle"), style = "text-align:center; color:#2c3e50; margin-bottom:18px; font-family:'Montserrat',sans-serif;"),
            fluidRow(
              column(4,
                     selectizeInput("weatherParameterSelect", "Select Parameter",
                                    choices = c("Temp" = "temp", "Feels Like" = "feels_like", "Temp Max" = "temp_max", "Temp Min" = "temp_min",
                                                "Pressure" = "pressure", "Sea Level" = "sea_level", "Ground Level" = "grnd_level", "Humidity" = "humidity",
                                                "Wind Speed" = "wind_speed", "Deg" = "deg", "Gust" = "gust"),
                                    selected = "temp", width = "100%")
              ),
              column(8,
                     plotlyOutput('forecastPlot', width = "100%", height = "480px")
              )
            )
    )
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# --------------------- SERVER ---------------------------------------
server <- function(input, output, session) {
  api_key <- "81a20cee159f98997026849048d40888"
  locationName <- reactiveVal()
  clickedLocation <- reactiveVal(NULL)
  
  observe({
    updateTabItems(session, "Menu", "tab3")
  })
  
  forecast_data_for_city <- reactiveVal(NULL)
  selected_city_name <- reactiveVal(NULL)
  
  updateForecastForCity <- function(cityName) {
    city <- url_encode(cityName)
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s", city, api_key)
    response <- GET(API_call)
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      selected_city_name(data$name)
      lat <- data$coord$lat
      lon <- data$coord$lon
      getWeatherForecast(lat, lon)
    }
  }
  
  output$locationDateOnly <- renderText({
    if (!is.null(locationName())) {
      format(Sys.time(), paste(locationName()))
    }
  })
  output$currentDate <- renderText({
    format(Sys.Date(), "%A, %d %B %Y", tz = "Asia/Ho_Chi_Minh")
  })
  autoInvalidate <- reactiveTimer(1000)
  output$currentTime <- renderText({
    autoInvalidate()
    format(Sys.time(), "%H:%M:%S", tz = "Asia/Ho_Chi_Minh")
  })
  
  
  
  getWeatherForCity <- function(cityName) {
    updateForecastForCity(cityName)
    city <- url_encode(cityName)
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s", city, api_key)
    response <- GET(API_call)
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      locationName(data$name)
      current_temperature <- round(data$main$temp - 273.15, 1)
      feels_like <- round(data$main$feels_like - 273.15, 1)
      humidity <- data$main$humidity
      weather_condition <- tools::toTitleCase(data$weather[[1]]$description)
      visibility <- round(data$visibility / 1000, 1)
      wind_speed <- round(data$wind$speed, 1)
      air_pressure <- data$main$pressure
      
      output$weatherInfo <- renderUI({
        tagList(
          div(class="weather-detail-flex",
              div(class="weather-detail-main",
                  tags$img(src="https://cdn-icons-png.flaticon.com/512/1116/1116453.png", height="74px"),
                  div(class="main-condition", weather_condition),
                  h2(HTML(paste0(current_temperature, "°C"))),
                  div(class="main-feel", HTML(paste0("Feels like: ", feels_like, "°C")))
              ),
              div(class="weather-detail-extra",
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-tint"), "Humidity:", strong(paste(humidity, "%"))
                  ),
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-wind"), "Wind:", strong(paste(wind_speed, "km/h"))
                  ),
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-eye"), "Visibility:", strong(paste(visibility, "km"))
                  ),
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-gauge"), "Pressure:", strong(paste(air_pressure, "hPa"))
                  )
              )
          )
        )
      })
    } else {
      output$weatherInfo <- renderText("Unable to fetch weather information.")
    }
  }
  
  observeEvent(input$getWeatherButton, {
    city_name <- input$cityNameInput
    if (city_name != "") {
      getWeatherForCity(city_name)
    }
  })
  
  getWeatherForHanoi <- function() {
    lat <- 21.0277644
    lon <- 105.8341598
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s", lat, lon, api_key)
    response <- GET(API_call)
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      locationName(data$city$name)
      current_temperature <- round(data$list[[1]]$main$temp - 273.15, 1)
      feels_like <- round(data$list[[1]]$main$feels_like - 273.15, 1)
      humidity <- data$list[[1]]$main$humidity
      weather_condition <- tools::toTitleCase(data$list[[1]]$weather[[1]]$description)
      visibility <- round(data$list[[1]]$visibility / 1000, 1)
      wind_speed <- round(data$list[[1]]$wind$speed, 1)
      air_pressure <- data$list[[1]]$main$pressure
      
      output$weatherInfo <- renderUI({
        tagList(
          div(class="weather-detail-flex",
              div(class="weather-detail-main",
                  tags$img(src="https://cdn-icons-png.flaticon.com/512/1116/1116453.png", height="74px"),
                  div(class="main-condition", weather_condition),
                  h2(HTML(paste0(current_temperature, "°C"))),
                  div(class="main-feel", HTML(paste0("Feels like: ", feels_like, "°C")))
              ),
              div(class="weather-detail-extra",
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-tint"), "Humidity:", strong(paste(humidity, "%"))
                  ),
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-wind"), "Wind:", strong(paste(wind_speed, "km/h"))
                  ),
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-eye"), "Visibility:", strong(paste(visibility, "km"))
                  ),
                  div(class="weather-extra-item",
                      tags$i(class="fas fa-gauge"), "Pressure:", strong(paste(air_pressure, "hPa"))
                  )
              )
          )
        )
      })
    } else {
      output$weatherInfo <- renderText("Unable to fetch weather information.")
    }
  }
  
  getWeatherForHanoi()
  
  output$weatherMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 105.8342, lat = 21.0285, zoom = 10)  # Zoom rộng hơn khi mở app
  })
  
  forecast_data <- reactiveVal(NULL)
  
  observeEvent(input$weatherMap_click, {
    click <- input$weatherMap_click
    if (!is.null(click)) {
      lat <- click$lat
      lng <- click$lng
      # Zoom vào vị trí được nhấp
      leafletProxy("weatherMap") %>%
        setView(lng = lng, lat = lat, zoom = 12)
      response <- GET(paste0("https://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lng, "&appid=", api_key))
      if (http_type(response) == "application/json") {
        data <- content(response, "parsed")
        locationName(data$name)
        current_temperature <- round(data$main$temp - 273.15, 1)
        feels_like <- round(data$main$feels_like - 273.15, 1)
        humidity <- data$main$humidity
        weather_condition <- tools::toTitleCase(data$weather[[1]]$description)
        visibility <- round(data$visibility / 1000, 1)
        wind_speed <- round(data$wind$speed, 1)
        air_pressure <- data$main$pressure
        
        output$weatherInfo <- renderUI({
          tagList(
            div(class="weather-detail-flex",
                div(class="weather-detail-main",
                    tags$img(src="https://cdn-icons-png.flaticon.com/512/1116/1116453.png", height="74px"),
                    div(class="main-condition", weather_condition),
                    h2(HTML(paste0(current_temperature, "°C"))),
                    div(class="main-feel", HTML(paste0("Feels like: ", feels_like, "°C")))
                ),
                div(class="weather-detail-extra",
                    div(class="weather-extra-item",
                        tags$i(class="fas fa-tint"), "Humidity:", strong(paste(humidity, "%"))
                    ),
                    div(class="weather-extra-item",
                        tags$i(class="fas fa-wind"), "Wind:", strong(paste(wind_speed, "km/h"))
                    ),
                    div(class="weather-extra-item",
                        tags$i(class="fas fa-eye"), "Visibility:", strong(paste(visibility, "km"))
                    ),
                    div(class="weather-extra-item",
                        tags$i(class="fas fa-gauge"), "Pressure:", strong(paste(air_pressure, "hPa"))
                    )
                )
            )
          )
        })
      } else {
        output$weatherInfo <- renderText("Unable to fetch weather information.")
      }
    }
  })
  
  output$locationName <- renderText({
    locationName()
  })
  
  getWeatherForecast <- function(lat, lon) {
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s", lat, lon, api_key)
    response <- GET(API_call)
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      forecast_data(data$list)
      clickedLocation(data$city$name)
    }
  }
  
  output$forecastTitle <- renderText({
    if (!is.null(clickedLocation())) {
      clickedLocation()
    } else {
      "5-Day Weather Forecast"
    }
  })
  
  output$forecastPlot <- renderPlotly({
    if (!is.null(forecast_data())) {
      selected_parameter <- input$weatherParameterSelect
      df <- lapply(forecast_data(), function(entry) {
        datetime <- as.POSIXct(entry$dt_txt)
        if (selected_parameter %in% names(entry$main)) {
          parameter_value <- entry$main[[selected_parameter]]
          data.frame(datetime = datetime, value = parameter_value)
        } else if (selected_parameter %in% names(entry$wind)) {
          parameter_value <- entry$wind[[selected_parameter]]
          data.frame(datetime = datetime, value = parameter_value)
        } else {
          data.frame(datetime = datetime, value = NA)
        }
      }) %>%
        bind_rows()
      
      plot <- ggplot(df, aes(x = datetime, y = value)) +
        geom_line(size = 1.2, color = "#2d98da") +
        geom_point(color = "#eb2f06", size = 2) +
        labs(x = "Date & Time", y = selected_parameter) +
        theme_minimal(base_family = "Montserrat") +
        theme(
          axis.title = element_text(size = 16, face="bold"),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 18, face="bold", hjust=0.5)
        )
      
      ggplotly(plot)
    }
  })
  
  output$tempHumidityPlot <- renderPlotly({
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/forecast?lat=21.0277644&lon=105.8341598&appid=%s", api_key)
    response <- GET(API_call)
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      forecast <- data$list
      df <- data.frame(
        temperature = sapply(forecast, function(x) round(x$main$temp - 273.15, 1)),
        humidity = sapply(forecast, function(x) x$main$humidity),
        datetime = sapply(forecast, function(x) as.POSIXct(x$dt_txt))
      )
      
      plot <- ggplot(df, aes(x = temperature, y = humidity, color = datetime)) +
        geom_point(size = 3) +
        labs(x = "Temperature (°C)", y = "Humidity (%)", color = "Time") +
        theme_minimal(base_family = "Montserrat") +
        theme(
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12)
        ) +
        scale_color_gradient(low = "#228be6", high = "#fd7e14")
      
      ggplotly(plot)
    }
  })
  
  observeEvent(input$weatherMap_click, {
    click <- input$weatherMap_click
    if (!is.null(click)) {
      lat <- click$lat
      lon <- click$lng
      getWeatherForecast(lat, lon)
    }
  })
  
  getForecastForHanoi <- function() {
    lat <- 21.0277644
    lon <- 105.8341598
    getWeatherForecast(lat, lon)
  }
  getForecastForHanoi()
}

shinyApp(ui = ui, server = server)