library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(lubridate)  # For date handling

# Define your OpenWeatherMap API key
api_key <- "69e797f38d664f89afb1e8d920202d73"

# Define the coordinates for each of the 10 locations
locations <- data.frame(
  name = c("Flemington", "Kensington", "Parkville", "North Melbourne", 
           "Carlton", "Melbourne", "Docklands", "East Melbourne", "South Wharf", "Southbank"),
  lat = c(-37.7833, -37.7946, -37.7847, -37.8000, 
          -37.8000, -37.8136, -37.8150, -37.8167, -37.8200, -37.8230),
  lon = c(144.9333, 144.9289, 144.9631, 144.9500, 
          144.9667, 144.9631, 144.9470, 144.9833, 144.9600, 144.9581)
)

# Shiny UI with custom CSS for improved layout
weather_ui <- function(){
  fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
      }
      .forecast-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
      }
      .forecast-item {
        flex-grow: 1;  /* Adjust item width based on content */
        text-align: center;
        padding: 0 5px;
      }
      .weather-icon img {
        width: 40px;
        height: 40px;
      }
      .sun-moon-icons img {
        width: 20px;
        height: 20px;
        margin-left: 5px;
        margin-right: 5px;
        vertical-align: middle;
      }
      .temperature {
        margin-top: 5px;
        font-size: 14px;
      }
    "))
  ),
  
  fluidRow(
    column(6, 
           h4("Real-time weather information", align = "center"),
           textOutput("currentDateTime", inline = TRUE),
           leafletOutput("weatherMap", height = "90vh")
    ),
    
    column(6, 
           fluidRow(
             uiOutput("weather_info", style = "height: 45vh; overflow-y: scroll;")
           ),
           fluidRow(
             h4("Melbourne Historical Temperature Data", align = "center"),
             tags$div(class = 'tableauPlaceholder', id = 'viz1729490902543', style = 'position: relative; width: 100%; height: auto',
                      tags$noscript(a(href = '#', img(src = 'https://public.tableau.com/static/images/Hi/Historicaltemperature/Dashboard1/1_rss.png', alt = 'Tableau Visualization', style = 'border: none'))),
                      tags$object(class = 'tableauViz', style = 'width: 100%; height: 45vh; border: none;',
                                  tags$param(name = 'host_url', value = 'https%3A%2F%2Fpublic.tableau.com%2F'),
                                  tags$param(name = 'embed_code_version', value = '3'),
                                  tags$param(name = 'site_root', value = ''),
                                  tags$param(name = 'name', value = 'Historicaltemperature/Dashboard1'),
                                  tags$param(name = 'tabs', value = 'no'),
                                  tags$param(name = 'toolbar', value = 'yes'),
                                  tags$param(name = 'static_image', value = 'https://public.tableau.com/static/images/Hi/Historicaltemperature/Dashboard1/1.png'),
                                  tags$param(name = 'animate_transition', value = 'yes'),
                                  tags$param(name = 'display_static_image', value = 'yes'),
                                  tags$param(name = 'display_spinner', value = 'yes'),
                                  tags$param(name = 'display_overlay', value = 'yes'),
                                  tags$param(name = 'display_count', value = 'yes'),
                                  tags$param(name = 'language', value = 'en-US')
                      )
             ),
             tags$script(HTML("
               var divElement = document.getElementById('viz1729490902543');                    
               var vizElement = divElement.getElementsByTagName('object')[0];                    
               vizElement.style.width='100%'; 
               vizElement.style.height='45vh';                    
               var scriptElement = document.createElement('script');                    
               scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    
               vizElement.parentNode.insertBefore(scriptElement, vizElement);
             "))
           )
    )
  )
)
}

weather_server <- function(input, output, session) {
  
    # Function to fetch weather data for all locations
  fetch_weather_data_for_all_locations <- function() {
    lapply(1:nrow(locations), function(i) {
      lat <- locations$lat[i]
      lon <- locations$lon[i]
      weather <- fetch_weather_data(lat, lon)
      if (!is.null(weather)) {
        main_weather <- weather[["weather"]][1, "main"]
        icon_url <- get_weather_icon(main_weather)
        # Store the timestamp with the weather data
        weather$dt <- format(as.POSIXct(weather$dt, origin = "1970-01-01", tz = "Australia/Melbourne"), "%b %d, %Y %I:%M %p")
        return(list(icon_url = icon_url, timestamp = weather$dt))
      } else {
        return(list(icon_url = "https://example.com/icons/default.png", timestamp = "N/A"))
      }
    })
  }
  
   fetch_weather_data_for_all_locations <- function() {
    lapply(1:nrow(locations), function(i) {
      lat <- locations$lat[i]
      lon <- locations$lon[i]
      weather <- fetch_weather_data(lat, lon)
      if (!is.null(weather)) {
        main_weather <- weather[["weather"]][1, "main"]
        icon_url <- get_weather_icon(main_weather)
        return(icon_url)
      } else {
        return("https://example.com/icons/default.png")
      }
    })
  }
   
   output$currentDateTime <- renderText({
     # Fetch weather data for the first location to extract the timestamp
     lat <- locations$lat[1]
     lon <- locations$lon[1]
     weather <- fetch_weather_data(lat, lon)
     
     if (!is.null(weather)) {
       timestamp <- format(as.POSIXct(weather$dt, origin = "1970-01-01", tz = "Australia/Melbourne"), "%b %d, %Y %I:%M %p")
       paste("Last Updated:", timestamp)
     } else {
       paste("Last Updated: N/A")
     }
   })
  
  get_weather_icon <- function(main_weather, for_html = FALSE) {
    current_hour <- as.numeric(format(Sys.time(), "%H"))
    is_daytime <- current_hour >= 7 && current_hour < 20
    
    icon_path <- if (grepl("Clouds", main_weather, ignore.case = TRUE)) {
      "www/clouds.png"
    } else if (grepl("Clear", main_weather, ignore.case = TRUE)) {
      ifelse(is_daytime, "www/sun.png", "www/moon.png")
    } else if (grepl("Rain", main_weather, ignore.case = TRUE)) {
      "www/rain.png"
    } else {
      "https://example.com/default.png"
    }
    
    # If the path is for HTML, remove the 'www/' prefix
    if (for_html) {
      icon_path <- gsub("^www/", "", icon_path)
    }
    
    return(icon_path)
  }
  
  # Function to fetch weather data for a single location
  fetch_weather_data <- function(lat, lon) {
    url <- paste0("http://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lon, "&appid=", api_key, "&units=metric")
    response <- GET(url)
    if (status_code(response) == 200) {
      return(fromJSON(content(response, "text"), flatten = TRUE))
    } else {
      return(NULL)
    }
  }
  
  # Function to fetch forecast data for a specific location
  fetch_forecast_data <- function(lat, lon, days) {
    url <- paste0("https://api.openweathermap.org/data/2.5/forecast/daily?lat=", lat, "&lon=", lon, "&cnt=", days, "&units=metric&appid=", api_key)
    response <- GET(url)
    
    if (status_code(response) == 200) {
      return(fromJSON(content(response, "text"), flatten = TRUE))
    } else {
      return(NULL)
    }
  }
  
  # Reactive value to store the weather forecast
  forecast_data <- reactiveVal()
  
  # Initial map setup with weather icons
  output$weatherMap <- renderLeaflet({
    icons_list <- fetch_weather_data_for_all_locations()
    
    # Create the leaflet map
    leaflet(options = leafletOptions(minZoom=13)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 13) %>%
      # Add markers and store them in a variable
      addMarkers(lng = locations$lon, lat = locations$lat,
                 icon = icons(
                   iconUrl = unlist(icons_list),
                   iconWidth = 40,
                   iconHeight = 40,
                   iconAnchorX = 15,
                   iconAnchorY = 30
                 ))
  })
  
  # Fetch and store forecast data for the first location on startup
  observe({
    initial_location <- locations[1, ]
    forecast <- fetch_forecast_data(initial_location$lat, initial_location$lon, input$days)
    forecast_data(forecast)
  })
  
  
  
  # When a location is clicked, fetch the weather data for the clicked location
  observeEvent(input$weatherMap_marker_click, {
    click <- input$weatherMap_marker_click
    lat <- click$lat
    lon <- click$lng
    
    # Find the index of the clicked marker
    clicked_location <- which(locations$lat == lat & locations$lon == lon)
    
    if (length(clicked_location) == 0) {
      showNotification("Error: Unable to determine location.")
      return()
    }
    
    # Get the name of the location from the locations data frame
    location_name <- locations$name[clicked_location]
    
    # Fetch weather data for the clicked location
    url <- paste0("http://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lon, "&appid=", api_key, "&units=metric")
    response <- GET(url)
    
    if (status_code(response) == 200) {
      weather <- fromJSON(content(response, "text"), flatten = TRUE)
      
      # Safely access the lon, lat, and pressure fields
      coord_lon <- if (!is.null(weather[["coord"]][["lon"]])) weather[["coord"]][["lon"]] else "Unavailable"
      coord_lat <- if (!is.null(weather[["coord"]][["lat"]])) weather[["coord"]][["lat"]] else "Unavailable"
      pressure <- if (!is.null(weather[["main"]][["pressure"]])) weather[["main"]][["pressure"]] else "Unavailable"
      
      # Safely check if 'main' exists and is a list before accessing subfields
      weather_main <- if (!is.null(weather[["main"]]) && is.list(weather[["main"]])) weather[["main"]] else list(temp = NA, feels_like = NA, temp_min = NA, temp_max = NA, humidity = NA)
      
      # Safely check if 'weather' is a data frame and access 'main' and 'description' using proper indexing
      if (!is.null(weather[["weather"]]) && nrow(weather[["weather"]]) > 0) {
        main_weather <- weather[["weather"]][1, "main"]  # Access first row's 'main'
        description <- weather[["weather"]][1, "description"]  # Access first row's 'description'
      } else {
        main_weather <- "N/A"
        description <- "N/A"
      }
      
      # Determine the icon URL based on the weather condition
      icon_url <- get_weather_icon(main_weather)
      
      # Use [[ ]] to access weather data only if they exist, otherwise provide fallback values
      temp <- if (!is.null(weather_main[["temp"]])) weather_main[["temp"]] else "Unavailable"
      feels_like <- if (!is.null(weather_main[["feels_like"]])) weather_main[["feels_like"]] else "Unavailable"
      temp_min <- if (!is.null(weather_main[["temp_min"]])) weather_main[["temp_min"]] else "Unavailable"
      temp_max <- if (!is.null(weather_main[["temp_max"]])) weather_main[["temp_max"]] else "Unavailable"
      humidity <- if (!is.null(weather_main[["humidity"]])) weather_main[["humidity"]] else "Unavailable"
      
      # Create popup content with centered temperature and one decimal point
      popup_content <- paste0(
        "<div class='weather-popup' style='text-align: center;'>",  # Center the entire popup content,
        "<div class='temp' style='font-size: 15px;'>", location_name, "</div>", 
        "<div class='temp' style='font-size: 24px; font-weight: bold;'>", temp, "°C</div>",  # Centered and bold temperature
        "<div class='temp' style='font-size: 14px;'>", main_weather, "</div>",  # Centered and bold temperature
        "<div class='min-max' style='font-size: 14px;'>Min: ", temp_min, "°C | Max: ", temp_max, "°C</div>",
        "</div>"
      )
      
      # Update the marker with the new popup content and custom icon, and center the map on the clicked point
      leafletProxy("weatherMap") %>%
        clearPopups() %>%
        addPopups(lon, lat, popup_content, options = popupOptions(closeButton = TRUE)) %>%
        setView(lng = lon, lat = lat, zoom = input$weatherMap_zoom)  # Center the map and set zoom level 
      
    } else {
      # Display an error message in case the weather data cannot be fetched
      popup_content <- "<div class='weather-popup' style='text-align: center;'>Error: Unable to fetch weather data.</div>"
      leafletProxy("weatherMap") %>%
        clearPopups() %>%
        addPopups(lon, lat, popup_content, options = popupOptions(closeButton = TRUE))
    }
  })
  
  # Define default location (example: Melbourne)
  default_lat <- -37.8136  
  default_lon <- 144.9631  
  default_days <- 7  # Number of forecast days
  
  # Fetch and display the forecast for the default location on app load
  observe( {

    clicked_location <- which(locations$lat == default_lat & locations$lon == default_lon)
    
    if (length(clicked_location) > 0) {
      location_name <- locations$name[clicked_location]
      forecast <- fetch_forecast_data(default_lat, default_lon, default_days)
      update_forecast_ui(forecast, location_name)
    } else {
      showNotification("Error: Unable to determine clicked location.")
    }
  })
  
  update_forecast_ui <- function(forecast, location_name) {
    if (!is.null(forecast)) {
      forecast_data <- ""
      
      for (i in 1:default_days) {
        forecast_list <- forecast[["list"]][i, ]
        
        datetime <- if (!is.null(forecast_list[["dt"]])) {
          forecast_date <- as.POSIXct(forecast_list[["dt"]], origin = "1970-01-01", tz = "Australia/Melbourne")
          day_of_week <- format(forecast_date, "(%a)")
          paste0(format(forecast_date, "%m/%d"), " ", day_of_week)
        } else {
          "Unavailable"
        }
        
        temp_max <- if (!is.null(forecast_list[["temp.max"]])) round(forecast_list[["temp.max"]]) else "Unavailable"
        temp_min <- if (!is.null(forecast_list[["temp.min"]])) round(forecast_list[["temp.min"]]) else "Unavailable"
        main_weather <- if (!is.null(forecast_list[["weather"]][[1]][["main"]])) forecast_list[["weather"]][[1]][["main"]] else "N/A"
        icon_url <- get_weather_icon(main_weather, for_html = TRUE)
        
        sunrise <- if (!is.null(forecast_list[["sunrise"]])) 
          format(as.POSIXct(forecast_list[["sunrise"]], origin = "1970-01-01", tz = "Australia/Melbourne"), "%I:%M %p") else "Unavailable"
        sunset <- if (!is.null(forecast_list[["sunset"]])) 
          format(as.POSIXct(forecast_list[["sunset"]], origin = "1970-01-01", tz = "Australia/Melbourne"), "%I:%M %p") else "Unavailable"
        
        forecast_data <- paste0(forecast_data, 
                                "<div class='forecast-container'>",
                                "<div class='forecast-item'>", datetime, "</div>",
                                "<div class='forecast-item'><div class='weather-icon'><img src='", icon_url, "' /></div></div>",
                                "<div class='forecast-item'>", main_weather, "</div>",
                                "<div class='forecast-item'>", temp_min, " / ", temp_max, "°C</div>",
                                "<div class='forecast-item'>", sunrise, 
                                "<span class='sun-moon-icons'><img src='https://cdn3.iconfinder.com/data/icons/feather-5/24/sunrise-1024.png' /><img src='https://cdn3.iconfinder.com/data/icons/feather-5/24/sunset-1024.png' /></span> ", sunset, "</div>",
                                "</div>")
      }
      
      output$weather_info <- renderUI({
        forecast_html <- paste0("<h3 style='text-align:center;'>", location_name, " 7-day forecast</h3><br>", forecast_data)
        HTML(forecast_html)
      })
      
    } else {
      output$weather_info <- renderUI({
        HTML("<b>Error:</b> Unable to fetch forecast data.")
      })
    }
  }
  
  # Output weather forecast info
  observeEvent(input$weatherMap_marker_click, {
    click <- input$weatherMap_marker_click
    lat <- click$lat
    lon <- click$lng
    
    clicked_location <- which(locations$lat == lat & locations$lon == lon)
    
    if (length(clicked_location) == 0) {
      showNotification("Error: Unable to determine location.")
      return()
    }
    
    location_name <- locations$name[clicked_location]
    
    days <- 7  
    forecast <- fetch_forecast_data(lat, lon, days)
    
    if (!is.null(forecast)) {
      city_name <- if (!is.null(forecast[["city"]][["name"]])) forecast[["city"]][["name"]] else "Unknown"
      forecast_data <- ""
      
      for (i in 1:days) {
        forecast_list <- forecast[["list"]][i, ]
        
        datetime <- if (!is.null(forecast_list[["dt"]])) {
          forecast_date <- as.POSIXct(forecast_list[["dt"]], origin="1970-01-01", tz="Australia/Melbourne")
          day_of_week <- format(forecast_date, "(%a)")
          paste0(format(forecast_date, "%m/%d"), " ", day_of_week)
        } else {
          "Unavailable"
        }
        
        temp_max <- if (!is.null(forecast_list[["temp.max"]])) round(forecast_list[["temp.max"]]) else "Unavailable"
        temp_min <- if (!is.null(forecast_list[["temp.min"]])) round(forecast_list[["temp.min"]]) else "Unavailable"
        main_weather <- if (!is.null(forecast_list[["weather"]][[1]][["main"]])) forecast_list[["weather"]][[1]][["main"]] else "N/A"
        icon_url <- get_weather_icon(main_weather, for_html = TRUE)
        
        sunrise <- if (!is.null(forecast_list[["sunrise"]])) 
          format(as.POSIXct(forecast_list[["sunrise"]], origin = "1970-01-01", tz = "Australia/Melbourne"), "%I:%M %p") else "Unavailable"
        sunset <- if (!is.null(forecast_list[["sunset"]])) 
          format(as.POSIXct(forecast_list[["sunset"]], origin = "1970-01-01", tz = "Australia/Melbourne"), "%I:%M %p") else "Unavailable"
        
        forecast_data <- paste0(forecast_data, 
                                "<div class='forecast-container'>",
                                "<div class='forecast-item'>", datetime, "</div>",
                                "<div class='forecast-item'><div class='weather-icon'><img src='", icon_url, "' /></div></div>",
                                "<div class='forecast-item'>", main_weather, "</div>",
                                "<div class='forecast-item'>", temp_min, " / ", temp_max, "°C</div>",
                                "<div class='forecast-item'>", sunrise, 
                                "<span class='sun-moon-icons'><img src='https://cdn3.iconfinder.com/data/icons/feather-5/24/sunrise-1024.png' /><img src='https://cdn3.iconfinder.com/data/icons/feather-5/24/sunset-1024.png' /></span> ", sunset, "</div>",
                                "</div>")
      }
      
      output$weather_info <- renderUI({
        forecast_html <- paste0("<h4 style='text-align:center;'>", location_name, " 7-day forecast</h4></br>", forecast_data)
        HTML(forecast_html)
      })
      
    } else {
      output$weather_info <- renderUI({
        HTML("<b>Error:</b> Unable to fetch forecast data.")
      })
    }
  })
  


}
