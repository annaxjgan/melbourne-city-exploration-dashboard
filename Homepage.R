library(shiny)
source("tourist_visits.R")
source("food_drinks.R")
# Define UI
ui <- fluidPage(
  
  # Add modern styles with the SF Pro font, centered tabs, and updated button styles
  tags$head(
    tags$style(HTML("
        @font-face {
          font-family: 'Montserrat';
          src: url('https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap');
        }
        
        body {
          font-family: 'Montserrat', sans-serif; /* Set the font to Montserrat */
          background-color: #f4f7f6;
          color: #34495e;
          text-align: center;
        }
       .container-homepage {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        height: 100vh;
        padding: 30px 0;
        opacity: 0;
        animation: fadeIn 1.5s ease forwards; /* Fade in animation */
      }
      @keyframes fadeIn {
        from {
          opacity: 0; /* Start fully transparent */
          transform: translateY(10px); /* Start slightly lower */
        }
        to {
          opacity: 1; /* End fully opaque */
          transform: translateY(0); /* Return to original position */
        }
      }
      .welcome-message {
        font-size: 3em;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 30px;
        letter-spacing: 1.2px;
      }
      .welcome-message-2 {
        font-size: 1.6em;
        color: #7f8c8d;
        margin-bottom: 40px;
      }
      .button-container {
        display: flex; /* Use flexbox for horizontal layout */
        justify-content: center; /* Center the buttons */
        gap: 120px; /* Space between buttons */
        margin-top: 20px; /* Space above buttons */
      }
      .image-button:hover {
        box-shadow: 0 0 15px lightblue, 0 0 15px lightblue, 0 0 1px lightblue; /* Glow effect on hover */
        transform: translateY(-7px); /* Move the whole container up on hover */
      }
      .image-button {
        cursor: pointer;
        display: flex;
        flex-direction: column;
        justify-content: space-between; /* Ensure label is at the bottom */
        align-items: center;
        border: 2px solid #bdc3c7;
        border-radius: 8px;
        padding: 10px;
        background-color: white;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
        transition: box-shadow 0.3s ease, transform 0.3s ease;
        width: 200px;
        height: 250px;
      }
    
      .image-button img {
        width: 100px;
        height: auto;
        border-radius: 8px;
        margin-top: auto; /* Add margin to push image towards center */
        margin-bottom: 10px; /* Optional spacing between image and label */
      }
      
      .image-button label {
        justify-self: flex-end; /* Align label at the bottom */
        align-self: center; /* Align label at the bottom */
        font-size: 1em;
        color: #000000;
        margin-top: auto; /* Ensures the label stays at the bottom */
      }
      .tab-content-container {
          flex:1;
          display: flex;
          flex-direction: column;
          height: 100vh;
        }
      .tabset-panel .nav-tabs {
        justify-content: center;
        border-bottom: 2px solid #bdc3c7;
      }
      .nav-tabs {
        display: flex;
        justify-content: center; /* Centering tabs */
        border-bottom: 2px solid #bdc3c7;
        margin-top: 20px;
      }
      .nav-tabs .nav-item {
        margin-right: 20px;
      }
      .nav-tabs .nav-link {
        color: #3498db;
        border-radius: 4px;
        padding: 10px 20px;
        font-size: 1.2em;
        transition: all 0.2s ease;
      }
      .nav-tabs .nav-link:hover {
        color: #2980b9;
      }
      .nav-tabs .nav-link.active {
        background-color: #3498db;
        color: white;
      }
    "))
  ),
  
  # Show homepage initially
  conditionalPanel(
    condition = "output.show_homepage == true", # Conditionally render the homepage
    div(
      class = "container-homepage",
      
      # Welcome message
      div(class = "welcome-message", "Discover Melbourne."),
      div(class = "welcome-message-2", "Your Ultimate Guide to the City"),
      
      # Image buttons to navigate to the tabs
      div(class = "button-container",
          div(class = "image-button", 
              img(src = "destination.png", alt = "Explore Walking Tours", 
                  onclick = "Shiny.setInputValue('go_tours', '1')"),
              tags$label("Explore Walking Tours")
          ),
          div(class = "image-button", 
              img(src = "dinner.png", alt = "Find Food & Drink",
                  onclick = "Shiny.setInputValue('go_food', '1')"),
              tags$label("Find Food & Drink")
          ),
          div(class = "image-button", 
              img(src = "cloudy-day.png", alt = "Check the Weather", 
                  onclick = "Shiny.setInputValue('go_weather', '1')"),
              tags$label("Check the Weather")
          )
      )
    )
  ),
  
  
  conditionalPanel(
    condition = "output.show_homepage == false", # Conditionally render the tabs
    div(
      class = "tab-content-container",
      tabsetPanel(id = "main_tabs", 
                  tabPanel("Walking Tours", h3("Discover Melbourne's Best Walking Tours"), tours_ui() ),
                  tabPanel("Food & Drink", h3("Discover the Best Cafes, Restaurants, Bars, and More in Melbourne CBD"),page2_ui("food_tab")),
                  tabPanel("Weather", h3("Get Real-time Weather Updates for Your Trip"))
      ),
      height = "100vh"
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to track whether the homepage should be shown or not
  homepage_visible <- reactiveVal(TRUE)
  
  # Toggle between homepage and tabset based on button clicks
  observeEvent(input$go_tours, {
    homepage_visible(FALSE)  # Hide the homepage and show the tabs
    updateTabsetPanel(session, "main_tabs", selected = "Walking Tours")
  })
  
  observeEvent(input$go_food, {
    homepage_visible(FALSE)
    updateTabsetPanel(session, "main_tabs", selected = "Food & Drink")
  })
  
  observeEvent(input$go_weather, {
    homepage_visible(FALSE)
    updateTabsetPanel(session, "main_tabs", selected = "Weather")
  })
  
  # Output to control conditionalPanel visibility for homepage
  output$show_homepage <- reactive({
    homepage_visible()
  })
  outputOptions(output, "show_homepage", suspendWhenHidden = FALSE)
  
  tours_server(input, output, session) 
  page2_server("food_tab")
}

# Run the application 
shinyApp(ui = ui, server = server)