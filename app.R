# Load required libraries
library(shiny)
library(tidycensus)
library(mapview)
library(leaflet)
library(dotenv)
# Load environment variables from the .env file
dotenv::load_dot_env()

# Access the API key
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = TRUE, overwrite = TRUE)

# Define UI
ui <- fluidPage(
  # App title
  titlePanel("ACS Median Household Income Interactive Map"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      selectInput("geography", "Geography Level:",
                  choices = c("Tract" = "tract", "County" = "county", "State" = "state", "Block Group" = "block group"), 
                  selected = "tract"),
      
      # Add dynamic variable selection
      selectInput("variable", "ACS Variable:",
                  choices = c("Median household income (B19013_001)" = "B19013_001",
                              "Total population (B01003_001)" = "B01003_001",
                              "Median age (B01002_001)" = "B01002_001")),
      
      selectInput("state", "Select State:",
                  choices = state.name, selected = "Illinois"),
      
      checkboxInput("show_geometry", "Show Geometry", value = TRUE)
    ),
    
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive expression to fetch ACS data
  acs_data <- reactive({
    # Convert state name to abbreviation
    state_abbr <- state.abb[match(input$state, state.name)]
    
    # Get ACS data
    get_acs(
      geography = input$geography,
      variables = input$variable,
      state = state_abbr,
      geometry = input$show_geometry
    )
  })
  
  # Render mapview plot
  output$map <- renderLeaflet({
    # Fetch data reactively
    data <- acs_data()
    
    # Create mapview object
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric(palette = "viridis", domain = data$estimate)(data$estimate),
        color = "white",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        popup = ~paste0("Estimate: ", round(estimate, 0))
      ) %>%
      addLegend(
        pal = colorNumeric(palette = "viridis", domain = data$estimate),
        values = ~estimate,
        opacity = 1.0,
        title = "Estimate",
        position = "bottomright"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
