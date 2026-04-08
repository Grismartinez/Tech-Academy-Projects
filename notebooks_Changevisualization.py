# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)

# Load dataset
housing <- read_csv("housing.csv")
# UI
ui <- dashboardPage(
  dashboardHeader(title = "Housing Data Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
               # Row 1
              fluidRow(
                box(title = "Ocean Proximity Distribution", status = "primary", solidHeader = TRUE,
                    plotOutput("barOcean")),
                
                box(title = "Population vs Households", status = "primary", solidHeader = TRUE,
                    plotOutput("popHouse"))
              ),
              # Row 2
              fluidRow(
                box(title = "Total Rooms vs Total Bedrooms", status = "primary", solidHeader = TRUE,
                    plotOutput("roomsBedrooms"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # 1. Ocean proximity count
  output$barOcean <- renderPlot({
    housing %>%
      count(ocean_proximity) %>%
      ggplot(aes(x = ocean_proximity, y = n, fill = ocean_proximity)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Number of Homes by Ocean Proximity",
           x = "Ocean Proximity",
           y = "Count")
  })
  
  # 2. Population vs Households
  output$popHouse <- renderPlot({
    ggplot(housing, aes(x = population, y = households)) +
      geom_point(alpha = 0.5) +
      theme_minimal() +
      labs(title = "Population vs Households",
           x = "Population",
           y = "Households")
  })
  
    #3. This Data change to change visualization
    output$priceBoxPlot <- renderPlot({
        ggplot(housing, aes(x = ocean_proximity, y = median_house_value, fill = ocean_proximity)) +
        geom_boxplot(alpha = 0.7) +
        theme_minimal() +
        labs(title = "House Prices by Ocean Proximity",
         x = "Ocean Proximity",
         y = "Median House Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Run app