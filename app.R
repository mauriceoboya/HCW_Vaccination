# Load required libraries
library(shiny)
library(shinymanager)
library(dplyr)
library(DT)
library(sf)
library(ggplot2)
library(leaflet)
library(memoise)

# --- ONE-TIME DATA LOADING & PREPROCESSING ---

# Sample user credentials (use secure storage in production)
credentials <- data.frame(
  user = c("admin", "user2"),
  password = c("admin", "password2"),
  admin = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Load shapefile and dataset once
shapefile <- read_sf('./ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp')
shapefile <- st_transform(shapefile, crs = 32637)

dataset <- readRDS(file = './HCWDashboard.rds')

# Filter and merge
data <- shapefile %>% filter(ADM1_EN == 'Kakamega')
merged_data <- merge(dataset, data, by.x = 'subcounty', by.y = 'ADM2_EN')
merged_sf <- st_as_sf(merged_data)
merged_sf$date.x <- as.Date(merged_sf$date.x)

# Define color palette for risk levels
color_palette <- c(
  "Low" = "#1f77b4",
  "Medium" = "#ff7f0e",
  "High" = "#2ca02c",
  "Critical" = "#d62728"
)

# --- UI DEFINITION ---
ui <- fluidPage(
  tags$h2("Health Workers Vaccination"),
  sidebarLayout(
    sidebarPanel(
      selectInput("regionInput", "Select Region:", choices = c("All", unique(merged_sf$subcounty)), selected = "All"),
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(merged_sf$date.x),
                     end = max(merged_sf$date.x)),
      checkboxInput("showAll", "Show All Data", value = TRUE)
    ),
    mainPanel(
      textOutput("testOutput"),
      plotOutput("ggplotPlot")
    )
  )
)

# Wrap with authentication UI
ui <- secure_app(ui)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Authentication
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Filtered data (memoised)
  cached_filtered_data <- memoise(function(region, start_date, end_date) {
    data <- merged_sf
    if (region != "All") {
      data <- data %>% filter(subcounty == region)
    }
    data %>% filter(date.x >= start_date & date.x <= end_date)
  })
  
  # Debounced reactive data
  filtered_data <- reactive({
    req(input$regionInput, input$dateRange)
    cached_filtered_data(input$regionInput, input$dateRange[1], input$dateRange[2])
  }) %>% debounce(500)
  
  # Text output
  output$testOutput <- renderText({
    paste("Region selected:", input$regionInput,
          "| Date range:", input$dateRange[1], "to", input$dateRange[2])
  })
  
  # Map plot
  output$ggplotPlot <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    p <- ggplot(data = data) +
      geom_sf(aes(fill = risk_level), color = "black") +
      scale_fill_manual(values = color_palette) +
      labs(title = "Spatial Analysis Risk Level",
           subtitle = "Subcounty and Risk Level Distribution",
           fill = "Risk Level") +
      theme_minimal()
    
    # Only show subcounty labels if one region selected
    if (input$regionInput != "All") {
      p <- p + geom_sf_text(aes(label = subcounty), size = 3, color = "white")
    }
    
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)
