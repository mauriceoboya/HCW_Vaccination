# Load required libraries
library(shiny)
library(shinymanager)  # Load shinymanager for authentication
library(dplyr)
library(DT)
library(sf)
library(ggplot2)
library(leaflet)

# Sample user credentials (replace with secure storage in real applications)
credentials <- data.frame(
  user = c("admin", "user2"),
  password = c("admin", "password2"), # These should be hashed in production
  admin = c(TRUE, FALSE), # Admin rights: TRUE or FALSE
  stringsAsFactors = FALSE
)

# Load shapefile and dataset
shapefile <- read_sf('./ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp')
dataset <- readRDS(file = './HCWDashboard.rds')

# Check CRS and reproject if necessary
if (st_crs(shapefile)$epsg != 32637) {
  shapefile <- st_transform(shapefile, crs = 32637)
}

# Filter the data for Kakamega and merge datasets
data <- shapefile %>% filter(ADM1_EN == 'Kakamega')
merged_data <- merge(dataset, data, by.x = 'subcounty', by.y = 'ADM2_EN')

# Ensure the merged data is an sf object
merged_sf <- st_as_sf(merged_data)

# Ensure date.x is of Date type
merged_sf$date.x <- as.Date(merged_sf$date.x)

# Define UI
ui <- fluidPage(
  tags$h2("Geospatial Analysis of Child Mortality"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("regionInput", "Select Region:", choices = c("All", unique(merged_sf$subcounty)), selected = "All"),
      dateRangeInput("dateRange", "Select Date Range:", start = min(merged_sf$date.x), end = max(merged_sf$date.x)),
      checkboxInput("showAll", "Show All Data", value = TRUE)
    ),
    mainPanel(
      textOutput("testOutput"),
      plotOutput("ggplotPlot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Call to shinymanager for authentication
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Display authentication status (for debugging)
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Text output for testing
  output$testOutput <- renderText({
    paste("Region selected:", input$regionInput, 
          "Date range selected:", input$dateRange[1], "to", input$dateRange[2])
  })
  
  # Define a color palette
  color_palette <- c(
    "Low" = "#1f77b4",    # Blue
    "Medium" = "#ff7f0e", # Orange
    "High" = "#2ca02c",   # Green
    "Critical" = "#d62728" # Red
  )
  
  # Reactive expression to filter the data based on user input
  filtered_data <- reactive({
    filter_data <- merged_sf
    
    # If not "All", filter by region
    if (input$regionInput != "All") {
      filter_data <- filter_data %>% filter(subcounty == input$regionInput)
      print(paste("Filtered by Region:", input$regionInput))  # Debugging
    }
    
    # Filter by date range
    filter_data <- filter_data %>%
      filter(date.x >= input$dateRange[1], date.x <= input$dateRange[2])
    print(paste("Filtered by Date:", input$dateRange[1], "to", input$dateRange[2]))  # Debugging
    
    return(filter_data)
  })
  
  # Render ggplot2 Map
  output$ggplotPlot <- renderPlot({
    filtered <- filtered_data()  # Retrieve filtered data
    if (nrow(filtered) == 0) return()  # Handle empty dataset
    
    ggplot(data = filtered) +
      geom_sf(aes(fill = risk_level), color = "black") +
      geom_sf_text(aes(label = subcounty), size = 3, color = "white") +
      scale_fill_manual(values = color_palette) +
      labs(title = "Geospatial Analysis of Child Mortality by Risk Level",
           subtitle = "Subcounty and Risk Level Distribution",
           fill = "Risk Level") +
      theme_minimal()
  })
}

# Wrap the UI with secure UI for authentication
ui <- secure_app(ui)

# Run the application 
shinyApp(ui = ui, server = server)
