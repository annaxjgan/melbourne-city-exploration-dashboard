library(shiny)
library(leaflet)
library(dplyr)


page2_ui <- function(id) {
  ns <- NS(id)  # Create a namespace for the module
  
  industry_choices <- c(
    "Cafes and Restaurants" = "Cafes and Restaurants",
    "Bars" = "Pubs, Taverns and Bars",
    "Takeaway Food" = "Takeaway Food Services",
    "Bakery" = "Bakery Product Manufacturing (Non-factory based)"
  )
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css") ,
      tags$style(HTML("
      .map-container {
        display: flex;
        flex-direction: column;
        height: 100vh;
      }
     .map-food-panel {
        flex-grow: 1; 
        height: 100%; 
      }

      
      
      .input-with-label {
      display: flex;
       align-items: center;
       max-width:300px;
      
      }

    .input-label {
       font-size:15px;
       color: navy;
       font-weight: bold; 
        margin-right:8px;
        margin-top:5px;
    
      }
      .food-checkbox label{
      font-size:15px;
       color: navy;
        font-weight: bold; 
        margin-top:14px;
      }
      #food-inner-div{
        display: flex;
        flex-direction: row;
        justify-content: space-evenly;
        border-right: 1px solid #e3e3e3; 
        border-left: 1px solid #e3e3e3; 
        border-top: 1px solid #e3e3e3;
        border-radius:6px 6px 0 0;
        z-index:999;
        padding: 0.5rem 9rem 0.5rem 9rem;
      }
     
      #food-sidebar{
        width:100%;
        padding:1.2rem 1.2rem 0 1.2rem;
        background-color: #f5f5f5;
        border-top: 1px solid #e3e3e3; 
        border-right: 1px solid #e3e3e3; 
        border-left: 1px solid #e3e3e3; 
        border-radius:8px 8px 0 0;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      }
    "))
    
    ),
    
    
    div(class = "map-container",
        div(id = "food-sidebar",
            div(id = "food-inner-div",
                div(class = "input-with-label",
                    tags$span(class = "input-label", "Select:"),
                    selectInput(
                      ns("industry_filter"),
                      "",
                      choices = industry_choices,
                      selected = "Cafes and Restaurants",
                      multiple = FALSE
                    )
                
            ), 
            
                div(class = "input-with-label",
                    tags$span(class = "input-label", "Search:"),
                    textInput(
                      inputId = ns("shop_search"),
                      label = "",
                      placeholder = "Enter shop name or address"
                    )
                
            ),
            div(class = "food-checkbox",
            
                checkboxInput(
                  ns("outdoor_seating"),
                  "Outdoor Seating Available",
                  value = FALSE
                )
            
            )
        
        )
        ),
        
        # Map panel remains static without expansion/collapse functionality
        div(class = "map-food-panel",
            leafletOutput(ns("map"), height = "720px")
        )
    )
  )
}


mapbox_token <- "pk.eyJ1IjoibHVjY2FhYSIsImEiOiJjbTJkNDRuenQwbmNoMmtwcHZwYzJlb29oIn0.5L0qOTHrDZQvQD8i1JW_lg"
icon_paths <- list(
  "Cafes and Restaurants" = "img/restaurant-svgrepo-com.svg",
  "Pubs, Taverns and Bars" = "img/cocktail-svgrepo-com.svg",
  "Takeaway Food Services" = "img/takeaway-svgrepo-com.svg",
  "Bakery Product Manufacturing (Non-factory based)" = "img/chocolate-roll-bakery-svgrepo-com.svg"
)

get_icon <- function(industry) {
  icon_path <- icon_paths[[industry]]
  if (!is.null(icon_path)) {
    return(icon_path) 
  } else {
    return("img/location-svgrepo-com.svg") 
  }
}

food_map <- function(data, mean_longitude, mean_latitude) {
  if (nrow(data) == 0) {
    # Return an empty map view centered on the mean coordinates
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 14, maxZoom = 18)) %>%
      setView(lng = mean_longitude, lat = mean_latitude, zoom = 14)
  } else {
    first_instance <- data$industry_anzsic4_description[1]
    
    icons <- icons(
      iconUrl = sapply(data$industry_anzsic4_description, get_icon),  
      iconWidth = 27, 
      iconHeight = 27,
      className = get_icon(first_instance)
    )
    
    leaflet(data, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 14, maxZoom = 18)) %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        icon = icons,
        popup = ~paste0(
          "<div style='border: 1px solid #ccc; border-radius: 6px; padding: 4px; box-shadow: 0px 4px 7px rgba(0, 0, 0, 0.1); max-width: 250px;'>",
          "<h4 style='margin-top: 0; color: #333;'>", trading_name, "</h4>",
          "<p style='margin: 5px 0;'><strong>Address:</strong> ", business_address, "</p>",
          ifelse(grepl("outdoor", seating_type, ignore.case = TRUE),
                 "<p style='color: green; margin: 5px 0;'>🌿 Outdoor seats available</p>", ""),
          "<p style='margin: 5px 0;'><strong>Number of Seats:</strong> ", number_of_seats, "</p>",
          "</div>"
        ),
        clusterOptions = markerClusterOptions(
          iconCreateFunction = JS("
  function(cluster) {
    var childCount = cluster.getChildCount();
    var markers = cluster.getAllChildMarkers();
    var iconPath = markers[0].options.icon.options.className;  // Updated path
    var size = '27'; // Default size

    // Determine the icon size based on the number of markers in the cluster
    if (iconPath === 'img/restaurant-svgrepo-com.svg') {
      if (childCount > 100) {
        size = 50; // Large icon size
      } else if (childCount > 80) {
        size = 45; // Medium-Large icon size
      } else if (childCount > 50) {
        size = 39; // Medium icon size
      } else if (childCount > 14) {
        size = 35; // Small-Medium icon size
      } else if (childCount > 5) {
        size = 32; // Small size
      }else if (childCount > 3) {
        size = 29; // Small size
      }
    } else if (iconPath ===  'img/takeaway-svgrepo-com.svg' ) {
      if (childCount > 35) {
        size = 48; 
      } else if (childCount > 20) {
        size = 44; 
      }  else if (childCount > 10) {
        size = 40; 
      }else if (childCount > 6) {
        size = 36; 
      }else if (childCount > 3) {
        size = 33; 
      }
    } else if (iconPath === 'img/cocktail-svgrepo-com.svg'  ) {
      if (childCount > 15) {
        size = 56; 
      } else if (childCount > 10) {
        size = 52; 
      } else if (childCount > 7) {
        size = 48; 
      }else if (childCount > 4) {
        size = 40; 
      }else if (childCount > 2) {
        size = 37; 
      } else{
        size = 35;
      }
    }
    else if (iconPath === 'img/chocolate-roll-bakery-svgrepo-com.svg'){
      size = 42;
    }

    // Return the image element for the cluster icon
    return new L.DivIcon({
      html: '<img src=\"' + iconPath + '\" width=\"' + size + '\" height=\"' + size + '\" style=\"border:none;\"/>',
      className: '',
      iconSize: new L.Point(size, size) // Set icon size
    });
  
  }
        "),
          showCoverageOnHover = TRUE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          disableClusteringAtZoom = 18,
          minClusterSize = 3
        )
      ) %>%
      setView(
        lng = mean(data$longitude),
        lat = mean(data$latitude),
        zoom = ifelse(nrow(data) == 1, 17, 14)
      )
  }
}


page2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    data <- read.csv("cleaned_cafes_restaurants_melbourne_cbd.csv")
    
    mean_latitude <- mean(data$latitude, na.rm = TRUE)
    mean_longitude <- mean(data$longitude, na.rm = TRUE)
    
    observeEvent(input$industry_filter, {
      updateCheckboxInput(session, "outdoor_seating", value = FALSE)
      updateTextInput(session, "shop_search", value = "") 
    })
    
    # Reactive expression to filter data based on the input industry
    filtered_data <- reactive({
      req(input$industry_filter)  # Ensure the industry filter is available
      
      result <- data %>%
        filter(industry_anzsic4_description == input$industry_filter)
      
      # Apply search filter if search text is not empty
      if (!is.null(input$shop_search) && input$shop_search != "") {
        result <- result %>%
          filter(grepl(input$shop_search, trading_name, ignore.case = TRUE) |
                   grepl(input$shop_search, building_address, ignore.case = TRUE))
      }
      
      if (!is.null(input$outdoor_seating) && 
          input$outdoor_seating) {
        result <- result %>%
          filter(grepl("Outdoor", seating_type, ignore.case = TRUE))
      }
      
      result
    })
    
    output$map <- renderLeaflet({
      data_to_display <- filtered_data()
      
      # Check if filtered data is empty
      if (nrow(data_to_display) == 0) {
        # Call food_map with an empty data frame and use the mean coordinates
        food_map(data.frame(longitude = numeric(0), latitude = numeric(0)), 
                 mean_longitude, mean_latitude)  
      } else {
        # Call food_map with the filtered data
        food_map(data_to_display, mean_longitude, mean_latitude)  
      }
    })
  })
}