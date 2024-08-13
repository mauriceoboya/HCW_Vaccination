library(shiny)
library(shinymanager)
library(shinyjs)
library(dplyr)
library(DT)
library(sf)
library(ggplot2)
library(leaflet)

# Load shapefile and dataset
shapefile <- read_sf('./ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp')
dataset <- readRDS(file = './HCWDashboard.rds')

# Filter the data for Kakamega and merge datasets
data <- shapefile %>% filter(ADM1_EN == 'Kakamega')
merged_data <- merge(dataset, data, by.x = 'subcounty', by.y = 'ADM2_EN')

# Ensure the merged data is an sf object
merged_sf <- st_as_sf(merged_data)

# User credentials
credentials <- data.frame(
  user = c("admin"),
  password = sapply(c("admin"), sodium::password_store),  # Hashed password
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$h2("Spatial Data Dashboard"),
  shinymanager::auth_ui(id = "auth"),
  
  # Only show dashboard content if the user is authenticated
  conditionalPanel(
    condition = "output.authenticated == true",
    sidebarLayout(
      sidebarPanel(
        selectInput("regionInput", "Select Region:", choices = c("All", unique(merged_sf$subcounty)), selected = "All"),
        dateRangeInput("dateRange", "Select Date Range:", start = min(merged_sf$date.x), end = max(merged_sf$date.x)),
        checkboxInput("showAll", "Show All Data", value = TRUE)
      ),
      mainPanel(
        plotOutput("ggplotPlot")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Authentication setup using shinymanager
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(credentials)
  )
  
  # Output a flag indicating whether the user is authenticated
  output$authenticated <- reactive({
    res_auth$auth
  })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)
  
  # Define a color palette
  color_palette <- c(
    "Low" = "#1f77b4",    # Blue
    "Medium" = "#ff7f0e", # Orange
    "High" = "#2ca02c",   # Green
    "Critical" = "#d62728" # Red
  )
  
  # Reactive expression to filter the data based on user input
  filtered_data <- reactive({
    req(res_auth$auth)  # Require user to be authenticated
    
    if (input$showAll) {
      return(merged_sf)
    }
    
    filter_data <- merged_sf
    
    # Filter by region if not "All"
    if (input$regionInput != "All") {
      filter_data <- filter_data %>% filter(subcounty == input$regionInput)
    }
    
    # Filter by date range
    filter_data <- filter_data %>%
      filter(date.x >= input$dateRange[1], date.x <= input$dateRange[2])
    
    filter_data
  })
  
  # Render ggplot2 Map
  output$ggplotPlot <- renderPlot({
    req(filtered_data())
    
    ggplot(data = filtered_data()) +
      geom_sf(aes(fill = risk_level), color = "black") +
      geom_sf_text(aes(label = subcounty), size = 3, color = "white") +
      scale_fill_manual(values = color_palette) +
      labs(title = "Spatial Plot of HCW Vaccination by Risk Level",
           subtitle = "Subcounty and Risk Level Distribution",
           fill = "Risk Level") +
      theme_minimal()
  })
}

# Wrap the app with secure_app for shinymanager
shinyApp(ui = ui, server = server)
