library(shiny)
library(shiny)
library(tidyr)
library(leaflet)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(shinyjs)
library(geosphere)


# Initialize necessary vectors outside the loop
site_names <- c()
latitudes <- c()
longitudes <- c()
types <- c()
tourist_trails <- list()

tours_data <- read.csv("self-guided-walks.csv")
artworks_data <- read.csv("outdoor-artworks.csv")

# Ensure the relevant columns are available for use (latitude, longitude, description, etc.)
artworks_data <- artworks_data %>%
  mutate(lat = as.numeric(sapply(strsplit(Geo.Point, ", "), "[", 1)),
         lon = as.numeric(sapply(strsplit(Geo.Point, ", "), "[", 2)))

for (i in 1:nrow(tours_data)) {
  # Extract coordinates from JSON
  coordinates <- fromJSON(txt = tours_data$geo_shape[i])
  
  # Extract the coordinates from the list
  coords_array <- coordinates$coordinates
  
  # Initialize temporary lists for storing longitudes and latitudes
  longitudes_temp <- c()
  latitudes_temp <- c()
  
  # Check if it's a MultiLineString or LineString
  if (length(dim(coords_array)) == 3) {
    # Handle MultiLineString (3D array)
    for (j in 1:dim(coords_array)[1]) {
      longitudes_temp <- c(longitudes_temp, coords_array[j, , 1])
      latitudes_temp <- c(latitudes_temp, coords_array[j, , 2])
    }
  } else {
    # Handle LineString (2D array)
    longitudes_temp <- coords_array[, 1]
    latitudes_temp <- coords_array[, 2]
  }
  
  # Generate site names for the current trail
  site_names_temp <- paste(tours_data$name[i], "Stop", seq_along(latitudes_temp))
  
  # Accumulate information into main vectors
  for (k in seq_along(latitudes_temp)) {
    site_names <- c(site_names, site_names_temp[k])  # Append site names
    latitudes <- c(latitudes, latitudes_temp[k])      # Append latitudes
    longitudes <- c(longitudes, longitudes_temp[k])    # Append longitudes
    types <- c(types, paste(tours_data$name[i], "tour")) 
  }
  # Store trail names in tourist_trails list (if needed)
  tourist_trails[[tours_data$name[i]]] <- site_names_temp
}

# Create a single data frame for cultural sites
cultural_sites <- data.frame(
  name = site_names,
  lat = latitudes,
  lon = longitudes,
  type = types,
  stringsAsFactors = FALSE
)

# Initialize an empty leaderboard as a reactive value
leaderboard_data <- reactiveVal(data.frame(
  user = character(),
  score = integer(),
  challenges_completed = integer(),
  stringsAsFactors = FALSE
))



# UI setup
tours_ui <- function() {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$style(HTML("
        .sidebar {
          background-color: #343a40;
          color: #ffffff;
          padding: 15px; 
          height: 100vh;
          transition: width 0.3s;
          overflow: hidden; /* Prevent overflow when collapsed */
        }
        .sidebar .panel-title {
          color: #ffffff;
        }
        h4 {
          text-align: center;
          color: #343a40;
        }
        .btn {
          background-color: navy; 
          color: white;
        }
        .btn:hover {
          background-color: rgba(70, 130, 180, 065); 
          color: white;
          transition: background-color 0.1s ease-in-out, color 0.1s ease-in-out;
        }
        .btn.disabled {
          background-color: #6c757d;
          color: #ffffff !important;
          cursor: not-allowed; 
        }
        .well {
          margin-bottom: 20px; 
        }
        .input-group {
          display: flex;
          justify-content: space-between; 
          align-items: center; 
        }
        .input-group > input {
          flex-shrink: 1; 
        }
        .title-panel {
          text-align: center;
          color: #1c4e80;
        }
        .description-panel {
          max-height:210px;
          overflow-y:scroll;
        }
        .marker-cluster-small {
          background-color: rgba(173, 216, 230, 0.6) !important; 
          color: #000 !important; /* Text color */
        }
        
        .marker-cluster-small div {
          background-color: rgba(173, 216, 230, 0.6) !important; 
          color: #000 !important; /* Text color */
        }
        
        .marker-cluster-medium {
          background-color: rgba(70, 130, 180, 0.6) !important; 
          color: #fff !important; /* Text color */
        }
        
        .marker-cluster-medium div {
          background-color: rgba(70, 130, 180, 065) !important; 
          color: #fff !important; /* Text color */
        }
        
        .marker-cluster-large {
          background-color: rgba(0, 0, 139, 0.7) !important; 
          color: #fff !important; /* Text color */
        }
        
        .marker-cluster-large div {
          background-color: rgba(0, 0, 139, 0.7) !important;
          color: #fff !important; /* Text color */
        }
      "))
    ),
    sidebarLayout(
      sidebarPanel(
        id = "sidebar",
        wellPanel(
          h4("Walking Tour"),
          div(class = "input-group", 
              style = "display: flex; align-items: center; margin-bottom: 10px;",  # Center items vertically
              div(style = "flex: 1; margin-right: 5px;",  # Allow the selectInput to grow
                  selectInput("trail", "Select Walking Tour:", 
                              choices = names(tourist_trails), 
                              width = "100%")  # Full width for the selectInput
              ),
              actionButton("show_trail", "Show Trail", 
                           style = "flex-shrink: 0;")  # Prevent button from shrinking
          ),
          div(
            sliderInput("distance_filter", "Total journey distance (km)", 
                        min = 1, max = 7, value = 7, step = 1)
          )
        ),
        wellPanel(
          h4("Join Tour Challenges"),
          div(class = "input-group", 
              textInput("username", "User Name:", value = ""),
              actionButton("start", "Start")  # Keeping original sizing
          )
        ),
        wellPanel(
          h4("Challenge Progress"),
          div(style = "text-align: center;",
              textOutput("progress"))
        ),
        wellPanel(
          h4("Leaderboard"),
          div(style = "text-align: center;",  # Center alignment style
              actionButton("show_leaderboard", "Show Leaderboard"), # Button to show leaderboard modal
              actionButton("show_visited", "Show Sites Visited") # Button to show visited sites modal
          )
        ),
      ),
      mainPanel(
        leafletOutput("map", height="100vh"),
        div(style = "position: absolute; top: 10px; right: 20px; z-index: 1000;  height: 100%;",  
            conditionalPanel(
              condition = "input.show_trail > 0 && output.showResetButton", 
              uiOutput("showResetButton") 
            )
        )
      )
    )
  )
}

# Server logic
tours_server <- function(input, output, session) {
  
  
  # Reactive value to track if the map is in its original state
  is_map_original <- reactiveVal(TRUE)
  
  # Output to show the Reset Button
  output$showResetButton <- renderUI({
    # Check the state and return the button if necessary
    if (!is_map_original()) {
      actionButton("reset_map", "Reset Map", class = "reset-button")  # Custom class
    }
  })
  
  outputOptions(output, "showResetButton", suspendWhenHidden = FALSE)
  
  # Leaflet output to show all data at the start
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom=14)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(data = cultural_sites, lng = ~lon, lat = ~lat, label = ~type, layerId = ~name, 
                        icon = awesomeIcons(library='fa', icon='star', markerColor = 'darkblue', iconColor = '#ffffff'),
                        clusterOptions = markerClusterOptions()) %>%
      setView(lng = mean(cultural_sites$lon), lat = mean(cultural_sites$lat), zoom = 14) 
  })
  
  # Reactive value to track user data and visited sites
  user_data <- reactiveVal(NULL)
  # Initialize reactive values
  visited <- reactiveVal(list())  # Tracks the list of visited sites
  completed_challenges <- reactiveVal(list())  # Tracks completed challenges for each user
  
  # Update the leaderboard for the current user
  update_leaderboard <- function(score_increment, challenge_increment = 0) {
    current_user <- user_data()
    if (is.null(current_user)) return()  # Do nothing if no user is set
    
    current_leaderboard <- leaderboard_data()
    
    # Only update the score for the current user
    if (current_user %in% current_leaderboard$user) {
      # Update existing user's score and challenges completed
      current_leaderboard <- current_leaderboard %>%
        mutate(
          score = ifelse(user == current_user, score + score_increment, score),
          challenges_completed = ifelse(user == current_user, challenges_completed + challenge_increment, challenges_completed)
        )
    } else {
      # Add a new user to the leaderboard
      new_entry <- data.frame(user = current_user, score = score_increment, challenges_completed = challenge_increment)
      current_leaderboard <- rbind(current_leaderboard, new_entry)
    }
    
    # Update the reactive leaderboard
    leaderboard_data(current_leaderboard)
  }
  
  # Create a reactive output for the bar plot
  output$visited_plot <- renderPlot({
    req(input$username)  # Ensure a username is provided
    user_visited_sites <- visited()[[input$username]] %||% c()
    
    # Create a data frame of all tour types
    all_types <- distinct(cultural_sites %>% select(type))
    
    # Create a data frame of visited sites for the bar chart
    visited_data <- cultural_sites %>%
      group_by(type) %>%
      summarise(count = sum(name %in% user_visited_sites), .groups = 'drop') %>%
      right_join(all_types, by = "type") %>%
      mutate(count = ifelse(is.na(count), 0, count)) %>%
      arrange(desc(count))
    
    # Create the bar chart
    ggplot(visited_data, aes(x = reorder(type, -count), y = count)) +
      geom_bar(stat = "identity", fill = "navy") +
      coord_flip() +  # Flip the axes for better visibility
      labs(title = paste("Sites Visited by", input$username),
           x = "Types of Walking Tours",
           y = "Count of Sites Visited") +
      scale_y_continuous(breaks = seq(0, max(visited_data$count), by = 1)) +  # Set y-axis to integers
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5))  # Center the title
  })
  
  # Handle the "Show Sites Visited" button click event
  observeEvent(input$show_visited, {
    req(input$username)  # Ensure a username is provided
    visited_sites <- visited()
    
    # Check if there are visited sites for the user
    if (length(visited_sites) == 0) {
      showModal(modalDialog(
        title = "No Sites Visited",
        "You have not visited any sites yet.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Show the modal with the plot
    showModal(modalDialog(
      title = "Sites Visited",
      size = "l",  # Use "l" for large size
      easyClose = TRUE,
      footer = NULL,
      plotOutput("visited_plot")  # Use plotOutput to display the bar plot
    ))
  })
  
  
  # Start button to initialize user data
  observeEvent(input$start, {
    req(input$username)
    
    # Set current user
    user_data(input$username)
    
    # Initialize or retrieve the user's visited sites
    current_visited <- visited()
    if (is.null(current_visited[[input$username]])) {
      current_visited[[input$username]] <- c()
      visited(current_visited)
    }
    
    # Ensure the user is in the leaderboard with 0 points initially
    update_leaderboard(0)
    
    # Initialize completed challenges for this user (if not already set)
    user_completed <- completed_challenges()
    if (is.null(user_completed[[input$username]])) {
      user_completed[[input$username]] <- list()  # Empty list for each tour
      completed_challenges(user_completed)
    }
    
    # Assuming the current tour is selected in input$trail
    current_tour <- input$trail
    user_completed <- completed_challenges()[[input$username]]
    
    # Get how many challenges completed for the current tour
    challenges_for_tour <- user_completed[[current_tour]]
    num_challenges_completed <- length(challenges_for_tour)  # Count of completed challenges
    
    # Output the number of completed challenges in the UI
    output$progress <- renderText({
      req(input$trail)
      total_stops <- sum(cultural_sites$type == paste(input$trail, "tour"))
      
      user_visited_sites <- visited()[[input$username]] %||% c()
      
      visited_cleaned <- trimws(user_visited_sites)
      cultural_names_cleaned <- trimws(cultural_sites$name)
      visited_stops <- sum(cultural_names_cleaned %in% visited_cleaned & cultural_sites$type == paste(input$trail, "tour"))
      paste("You have completed", visited_stops, "out of", total_stops, "stops for the", input$trail, ".")
    })
  })
  
  # Handle the "Mark as visited" button click event
  observeEvent(input$visit_site, {
    req(input$visit_site)
    current_user <- user_data()
    current_visited <- visited()
    user_visited_sites <- current_visited[[current_user]] %||% c()
    
    # Ensure that the site is not already visited
    if (!(input$visit_site %in% user_visited_sites)) {
      # Add site to user's visited sites
      user_visited_sites <- c(user_visited_sites, input$visit_site)
      current_visited[[current_user]] <- user_visited_sites
      visited(current_visited)
      
      update_leaderboard(1)  # Increment score by 1 for the current user
    }
    
    output$progress <- renderText({
      req(input$trail)
      current_user <- user_data()
      if (is.null(current_user) || current_user == "") {
        return("Please enter your username and start the challenge.")
      }
      
      total_stops <- sum(cultural_sites$type == paste(input$trail, "tour"))
      user_visited_sites <- visited()[[current_user]] %||% c()
      
      visited_cleaned <- trimws(user_visited_sites)
      cultural_names_cleaned <- trimws(cultural_sites$name)
      visited_stops <- sum(cultural_names_cleaned %in% visited_cleaned & cultural_sites$type == paste(input$trail, "tour"))
      paste("You have completed", visited_stops, "out of", total_stops, "stops for the", input$trail, ".")
    })
  })
  
  # Show leaderboard modal
  observeEvent(input$show_leaderboard, {
    showModal(modalDialog(
      title = "Leaderboard",
      DT::dataTableOutput("leaderboard"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Render the leaderboard data table
  output$leaderboard <- DT::renderDataTable({
    # Sort the leaderboard data by score in descending order
    sorted_leaderboard <- leaderboard_data() %>%
      arrange(desc(score))
    
    sorted_leaderboard
  })
  
  # Calculate maximum distance from all trails and set it for the distance filter
  max_distance <- reactive({
    max(trail_distances(), na.rm = TRUE)
  })
  
  output$distance_slider <- renderUI({
    sliderInput("distance_filter", "Total journey distance (in km):",
                min = 0, max = max_distance(), value = max_distance())
  })
  
  observeEvent(input$show_trail, {
    is_map_original(FALSE)
    req(input$trail, input$distance_filter)  # Ensure both inputs are available
    
    selected_trail <- input$trail
    selected_distance <- input$distance_filter  # User input distance
    trail_data <- cultural_sites[cultural_sites$name %in% tourist_trails[[selected_trail]], ]
    
    starting_point <- data.frame(lon = trail_data$lon[1], lat = trail_data$lat[1])  # Create a data frame for starting point
    cumulative_distance <- 0
    stops_within_distance <- trail_data[1, ]  # Start with the starting point
    
    for (i in 2:nrow(trail_data)) {
      # Calculate the distance from the last stop to the current stop
      distance_to_next_stop <- distHaversine(c(trail_data$lon[i-1], trail_data$lat[i-1]), 
                                             c(trail_data$lon[i], trail_data$lat[i])) /1000  # Convert to kilometers
      
      # Check if adding this stop exceeds the selected distance
      if (cumulative_distance + distance_to_next_stop <= selected_distance) {
        cumulative_distance <- cumulative_distance + distance_to_next_stop  # Update cumulative distance
        stops_within_distance <- rbind(stops_within_distance, trail_data[i, ])  # Add stop to the list
      } else {
        break  # Stop if the next stop exceeds the selected distance
      }
    }
    
    # Create the endpoint based on the last included stop
    end_point <- data.frame(lon = stops_within_distance$lon[nrow(stops_within_distance)], 
                            lat = stops_within_distance$lat[nrow(stops_within_distance)])  # New endpoint
    
    findNearestArtwork <- function(stop_lat, stop_lon) {
      distances <- geosphere::distHaversine(
        matrix(c(stop_lon, stop_lat), ncol = 2),  # Stop location
        matrix(c(artworks_data$lon, artworks_data$lat), ncol = 2)  # All artworks locations
      )
      
      nearest_idx <- which.min(distances)  # Find the index of the nearest artwork
      nearest_artwork <- artworks_data[nearest_idx, ]  # Get the nearest artwork's data
      
      return(nearest_artwork)  # Return nearest artwork row
    }
    
    # Function to create popup content
    createPopupContent <- function(name, type, stop_lat, stop_lon) {
      # Initialize popup content with stop info
      content <- paste0(
        "<strong>Stop Name:</strong> ", name, "<br>",
        "<strong>Type:</strong> ", type, "<br>"
      )
      
      # Get the nearest artwork's data
      nearest_artwork <- findNearestArtwork(stop_lat, stop_lon)
      
      if(!is.null(nearest_artwork)) {
        # Add artwork information to the popup
        content <- paste0(content,
                          "<strong>Nearest Artwork:</strong> ", nearest_artwork$title, "<br>",
                          "<strong>Artist:</strong> ", nearest_artwork$makers, "<br>",
                          "<div class=\"description-panel\">",
                          "<strong>Description:</strong> ", nearest_artwork$description,
                          "</div>",
                          "<strong>Art Date:</strong> ", nearest_artwork$art_date, "<br>"
        )
      }
      
      
      # Add the "Mark as Visited" button if the user is logged in
      if (!is.null(user_data()) && user_data() != "") {
        content <- paste0(content, 
                          "<br><div style='text-align:center;'>",
                          "<button id='visitBtn' onclick=\"Shiny.setInputValue('visit_site', '", name, "', {priority: 'event'})\" class='btn btn-success'>Mark as visited</button>",
                          "</div>")
      }
      
      return(content)
    }
    
    # Clear previous markers and shapes (polylines) before adding new ones
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearMarkerClusters() %>%  # Clear marker clusters
      addPolylines(data = stops_within_distance, lng = ~lon, lat = ~lat, color = "navy", weight = 4) %>%
      addAwesomeMarkers(data = starting_point, 
                        lng = ~lon, lat = ~lat, 
                        icon = awesomeIcons(library='fa', icon='circle', markerColor='green', iconColor='#ffffff'),
                        label = "Start", 
                        popup = createPopupContent(trail_data$name[1], trail_data$type[1], trail_data$lat[1], trail_data$lon[1])) %>%
      addAwesomeMarkers(data = end_point, 
                        lng = ~lon, lat = ~lat, 
                        icon = awesomeIcons(library='fa', icon='circle', markerColor='red', iconColor='#ffffff'),
                        label = "End", 
                        popup = createPopupContent(stops_within_distance$name[nrow(stops_within_distance)], 
                                                   stops_within_distance$type[nrow(stops_within_distance)],
                                                   stops_within_distance$lat[nrow(stops_within_distance)],
                                                   stops_within_distance$lon[nrow(stops_within_distance)])) %>%
      addCircleMarkers(
        data = stops_within_distance,
        lng = ~lon,
        lat = ~lat,
        color = "blue",
        radius = 6,
        label = ~name,
        popup = ~{
          # For each marker, dynamically find the nearest artwork and build the popup
          mapply(function(lat, lon, name, type) {
            createPopupContent(name, type, lat, lon)
          },
          lat,
          lon,
          name,
          type,
          SIMPLIFY = FALSE)  # Ensure the output is a list of popups
        }
      )
    
    
    # Fit map bounds to show all markers
    bounds <- stops_within_distance %>% 
      summarize(lng_min = min(lon), lng_max = max(lon), lat_min = min(lat), lat_max = max(lat)) %>%
      {list(c(.$lng_min, .$lng_max), c(.$lat_min, .$lat_max))}
    
    leafletProxy("map") %>%
      fitBounds(bounds[[1]][1], bounds[[2]][1], bounds[[1]][2], bounds[[2]][2])
  })
  
  # Render the bar chart for sites visited by type
  output$bar_chart <- renderPlot({
    # Filter the data to show only visited sites
    visited_sites <- cultural_sites %>% filter(name %in% visited())
    
    # Create a bar chart showing the number of visited sites by type
    ggplot(visited_sites, aes(x = type)) +
      geom_bar(fill = "steelblue") +
      labs(title = "Sites Visited by Type", x = "Type", y = "Number of Sites") +
      theme_minimal()
  })
  
  
  #Reset the map
  observeEvent(input$reset_map, {
    is_map_original(TRUE)
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data = cultural_sites, lng = ~lon, lat = ~lat, label = ~type, layerId = ~name,clusterOptions = markerClusterOptions()) %>%
      fitBounds(
        lng1 = min(cultural_sites$lon), lat1 = min(cultural_sites$lat),
        lng2 = max(cultural_sites$lon), lat2 = max(cultural_sites$lat)
      )
  })
  
}

# Run the application
shinyApp(ui = tours_ui, server = tours_server)