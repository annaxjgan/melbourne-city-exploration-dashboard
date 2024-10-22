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
       color: #34495e;
       font-weight: bold; 
        margin-right:4px;
    
      }
      .food-checkbox label{
      font-size:15px;
       color: #34495e;
        font-weight: bold; 
        margin-top:14px;
      }
     
      #food-sidebar{
        width:100%;
        padding:1rem 10rem 1rem 10rem;
        background-color: #f5f5f5;
        border-top: 1px solid #e3e3e3; 
        border-right: 1px solid #e3e3e3; 
        border-left: 1px solid #e3e3e3; 
        display: flex;
        flex-direction: row;
        border-radius:6px 6px 0 0;
        justify-content: space-evenly;
        z-index:999;
      }
    "))
    
    ),
    
    
    div(class = "map-container",
        div(id = "food-sidebar",
            
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
                      placeholder = "Enter shop name"
                    )
                
            ),
            div(class = "food-checkbox",
            
                checkboxInput(
                  ns("outdoor_seating"),
                  "Outdoor Seating Available",
                  value = FALSE
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
      addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 15, maxZoom = 18)) %>%
      setView(lng = mean_longitude, lat = mean_latitude, zoom = 15)
  } else {
    first_instance <- data$industry_anzsic4_description[1]
    
    icons <- icons(
      iconUrl = sapply(data$industry_anzsic4_description, get_icon),  
      iconWidth = 27, 
      iconHeight = 27,
      className = get_icon(first_instance)
    )
    
    leaflet(data, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 15, maxZoom = 18)) %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        icon = icons,
        popup = ~paste0(
          "<strong>", trading_name, "</strong><br>",
          "Address: ", business_address, "<br>",
          ifelse(grepl("outdoor", seating_type, ignore.case = TRUE), "Outdoor seats available", ""),
          "<br>",
          "Number of Seats: ", number_of_seats
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
        zoom = ifelse(nrow(data) == 1, 17, 15)
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
          filter(grepl(input$shop_search, trading_name, ignore.case = TRUE))
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