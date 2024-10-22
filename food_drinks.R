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
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style-page2.css")
    ),
    
    # Main container with fixed layout
    div(class = "food-map-container",
        # Sidebar with fixed width
        div(class = "food-sidebar",
            div(class = "food-input-container",
                selectInput(
                  ns("food_industry_filter"),
                  "What you are looking for:",
                  choices = industry_choices,
                  selected = "Cafes and Restaurants",
                  multiple = FALSE
                )
            ),
            div(class = "food-input-container",
                textInput(
                  inputId = ns("food_shop_search"),
                  label = "Search:",
                  placeholder = "Enter shop name"
                )
            ),
            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'Cafes and Restaurants' || input['%s'] == 'Pubs, Taverns and Bars'",
                ns("food_industry_filter"),
                ns("food_industry_filter")
              ),
              div(class = "food-checkbox-container",
                  checkboxInput(
                    ns("food_outdoor_seating"),
                    "Outdoor Seating Available",
                    value = FALSE
                  )
              )
            )
        ),
        
        div(class = "food-map-panel",
            leafletOutput(ns("page2_food_map"))
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
      addTiles(
        url = paste0("https://api.mapbox.com/styles/v1/mapbox/streets-v12/tiles/{z}/{x}/{y}?access_token=", mapbox_token),
        attribution = 'Tiles &copy; <a href="https://www.mapbox.com/">Mapbox</a>',
        options = tileOptions(minZoom = 15, maxZoom = 18)
      ) %>%
      setView(lng = mean_longitude, lat = mean_latitude, zoom = 15)
  } else {
    first_instance <- data$industry_anzsic4_description[1]
    
    icons <- icons(
      iconUrl = sapply(data$industry_anzsic4_description, get_icon),  
      iconWidth = 25, 
      iconHeight = 25,
      className = get_icon(first_instance)
    )
    
    leaflet(data, options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles(
        url = paste0("https://api.mapbox.com/styles/v1/mapbox/streets-v12/tiles/{z}/{x}/{y}?access_token=", mapbox_token),
        attribution = 'Tiles &copy; <a href="https://www.mapbox.com/">Mapbox</a>',
        options = tileOptions(minZoom = 15,maxZoom = 18)  
      ) %>%
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
    var size = '24'; // Default size

    // Determine the icon size based on the number of markers in the cluster
    if (iconPath === 'img/restaurant-svgrepo-com.svg') {
      if (childCount > 100) {
        size = 45; // Large icon size
      } else if (childCount > 80) {
        size = 40; // Medium-Large icon size
      } else if (childCount > 50) {
        size = 35; // Medium icon size
      } else if (childCount > 14) {
        size = 31; // Small-Medium icon size
      } else if (childCount > 3) {
        size = 29; // Small size
      }
    } else if (iconPath ===  'img/takeaway-svgrepo-com.svg' ) {
      if (childCount > 35) {
        size = 45; // Larger size for this industry
      } else if (childCount > 20) {
        size = 40; // Medium-Large size
      } else if (childCount > 5) {
        size = 32; // Smaller size for fewer counts
      }
    } else if (iconPath === 'img/cocktail-svgrepo-com.svg'  ) {
      if (childCount > 15) {
        size = 52; // Larger size for this industry
      } else if (childCount > 8) {
        size = 48; // Medium-Large size
      } else if (childCount > 3) {
        size = 43; // Smaller size for fewer counts
      } else{
        size = 35;
      }
    }
    else if (iconPath === 'img/chocolate-roll-bakery-svgrepo-com.svg'){
      size = 38;
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
    
    # Load and filter the dataset
    data <- read.csv("cleaned_cafes_restaurants_melbourne_cbd.csv")
    
    mean_latitude <- mean(data$latitude, na.rm = TRUE)
    mean_longitude <- mean(data$longitude, na.rm = TRUE)
    
    observeEvent(input$food_industry_filter, {
      updateCheckboxInput(session, "food_outdoor_seating", value = FALSE)
    })
    
    # Reactive expression to filter data based on the input industry
    filtered_data <- reactive({
      req(input$food_industry_filter)  # Use namespaced ID
      
      result <- data %>%
        filter(industry_anzsic4_description == input$food_industry_filter)
      
      # Apply search filter if search text is not empty
      if (!is.null(input$food_shop_search) && input$food_shop_search != "") {
        result <- result %>%
          filter(grepl(input$food_shop_search, trading_name, ignore.case = TRUE))
      }
      
      # Apply outdoor seating filter if applicable
      if (!is.null(input$food_outdoor_seating) && 
          input$food_outdoor_seating && 
          input$food_industry_filter %in% c("Cafes and Restaurants", "Pubs, Taverns and Bars", "Takeaway Food Services", "Bakery Product Manufacturing (Non-factory based)")) {
        result <- result %>%
          filter(grepl("Outdoor", seating_type, ignore.case = TRUE))
      }
      
      result
    })
    
    output$page2_food_map <- renderLeaflet({
      data_to_display <- filtered_data()
      
      # Check if filtered data is empty
      if (nrow(data_to_display) == 0) {
        food_map(data.frame(longitude = numeric(0), latitude = numeric(0)), 
                 mean_longitude, mean_latitude)  
      } else {
        food_map(data_to_display, mean_longitude, mean_latitude)  
      }
    })
  })
}