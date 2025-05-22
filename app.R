# Load required libraries
library(shiny)
library(shinymanager)
library(dplyr)
library(DT)
library(sf)
library(ggplot2)
library(memoise)

# --- ONE-TIME DATA LOADING & PREPROCESSING ---

# User credentials
db_credentials <- data.frame(
  user = c("admin", "user2"),
  password = c("admin", "password2"),
  admin = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Load shapefile and dataset
shapefile <- read_sf('./ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp')
shapefile <- st_transform(shapefile, crs = 32637)

# Load dataset
dataset <- readRDS('./HCWDashboard.rds')

# Filter and merge
kakamega_shape <- shapefile %>% filter(ADM1_EN == 'Kakamega')
merged_data <- merge(dataset, kakamega_shape, by.x = 'subcounty', by.y = 'ADM2_EN')
merged_sf <- st_as_sf(merged_data)
merged_sf$date.x <- as.Date(merged_sf$date.x)

# Age group classification
merged_sf <- merged_sf %>%
  mutate(age_group = case_when(
    age < 30 ~ "<30",
    age >= 30 & age < 40 ~ "30-39",
    age >= 40 & age < 50 ~ "40-49",
    age >= 50 & age < 60 ~ "50-59",
    age >= 60 ~ "60+"
  ))

# Color palette for risk levels
color_palette <- c(
  "Moderate" = "#ff7f0e",
  "High" = "#d62728"
)

# --- UI ---
ui <- fluidPage(
  titlePanel("Health Workers Vaccination Risk Dashboard - Kakamega"),
  sidebarLayout(
    sidebarPanel(
      selectInput("regionInput", "Select Subcounty:",
                  choices = c("All", unique(merged_sf$subcounty)), selected = "All"),
      selectInput("sexInput", "Select Sex:", choices = c("All", "Male", "Female"), selected = "All"),
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(merged_sf$date.x),
                     end = max(merged_sf$date.x)),
      checkboxInput("showAll", "Show All Data", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(4, h4("Total Records:"), verbatimTextOutput("totalCount")),
                   column(4, h4("High Risk %:"), verbatimTextOutput("highRiskPct")),
                   column(4, h4("Top High-Risk Cadres:"), DTOutput("topCadres"))
                 )
        ),
        tabPanel("Risk Map", plotOutput("ggplotPlot")),
        tabPanel("Summary Table", DTOutput("summaryTable")),
        tabPanel("Risk Trends", plotOutput("timeSeriesPlot")),
        tabPanel("Age-Sex Breakdown", plotOutput("ageSexPlot")),
        tabPanel("Heatmap", plotOutput("heatmapPlot"))
      )
    )
  )
)

ui <- secure_app(ui)

# --- SERVER ---
server <- function(input, output, session) {
  res_auth <- secure_server(check_credentials = check_credentials(db_credentials))
  
  cached_filtered_data <- memoise(function(region, sex, start_date, end_date) {
    data <- merged_sf
    if (region != "All") {
      data <- data %>% filter(subcounty == region)
    }
    if (sex != "All") {
      data <- data %>% filter(sex == !!sex)
    }
    data %>% filter(date.x >= start_date & date.x <= end_date)
  })
  
  filtered_data <- reactive({
    req(input$regionInput, input$sexInput, input$dateRange)
    cached_filtered_data(input$regionInput, input$sexInput, input$dateRange[1], input$dateRange[2])
  }) %>% debounce(500)
  
  output$totalCount <- renderText({
    nrow(filtered_data())
  })
  
  output$highRiskPct <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return("0%")
    }
    high_risk_count <- sum(data$risk_level == "High", na.rm = TRUE)
    pct <- round(high_risk_count / nrow(data) * 100, 1)
    paste0(pct, "%")
  })
  
  output$topCadres <- renderDT({
    data <- filtered_data()
    data %>%
      st_drop_geometry() %>%
      filter(risk_level == "High") %>%
      count(cadre, sort = TRUE) %>%
      head(5) %>%
      datatable(options = list(dom = 't'))
  })
  
  output$ggplotPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data) +
      geom_sf(aes(fill = risk_level), color = "black") +
      scale_fill_manual(values = color_palette) +
      labs(title = "Risk Level by Subcounty", fill = "Risk Level") +
      theme_minimal()
  })
  
  output$summaryTable <- renderDT({
    data <- filtered_data()
    data %>%
      st_drop_geometry() %>%
      group_by(subcounty, cadre, risk_level) %>%
      summarise(count = n(), .groups = "drop") %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$timeSeriesPlot <- renderPlot({
    data <- filtered_data() %>% st_drop_geometry()
    data_summary <- data %>%
      mutate(week = format(date.x, "%Y-%W")) %>%
      group_by(week, risk_level) %>%
      summarise(count = n(), .groups = "drop")
    
    ggplot(data_summary, aes(x = week, y = count, fill = risk_level)) +
      geom_col(position = "dodge") +
      labs(title = "Weekly Risk Trends", x = "Week", y = "Number") +
      scale_fill_manual(values = color_palette) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$ageSexPlot <- renderPlot({
    data <- filtered_data() %>% st_drop_geometry()
    data %>%
      group_by(age_group, sex, risk_level) %>%
      summarise(n = n(), .groups = "drop") %>%
      ggplot(aes(x = age_group, y = n, fill = risk_level)) +
      geom_col(position = "stack") +
      facet_wrap(~ sex) +
      labs(title = "Risk by Age Group and Sex", x = "Age Group", y = "Count") +
      scale_fill_manual(values = color_palette) +
      theme_minimal()
  })
  
  output$heatmapPlot <- renderPlot({
    data <- filtered_data() %>% st_drop_geometry()
    ggplot(data, aes(x = risk_level, y = reorder(cadre, age))) +
      geom_bar(stat = "count", fill = "steelblue") +
      facet_wrap(~ subcounty) +
      theme_minimal() +
      labs(title = "Cadre vs Risk Level by Subcounty", x = "Risk Level", y = "Cadre")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
