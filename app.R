
# -----------------------------------#
# Setting working directory          #
# -----------------------------------#
setwd("C:/Users/Enita/OneDrive - University of West Georgia/Desktop/UWG Course Work/Spring Semester 2025/Advanced Visual Analytics/data")


#-----------------#
# Libraries       #
#-----------------#

library(shiny)
library(ggplot2)
library(tidyverse)
library(readr)
library(shinyFeedback)

#---------------------------#
# Data Preparation          #
#---------------------------#

# Loading the data
covid_data <- read.csv("US_states_covid_data_2023.csv")

# Convert date
covid_data$date <- as.Date(covid_data$date)

# Calculating new cases and deaths
covid_data <- covid_data %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(new_cases = cases - lag(cases, default = first(cases)),
         new_deaths = deaths - lag(deaths, default = first(deaths)))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Cases and Deaths in the US States (2023) Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("date", label = "Select Date:", choices = unique(covid_data$date)),
            selectInput("state", "Select State:", choices = unique(covid_data$state)),
            dateRangeInput("date_range", "Date Range:", start = min(covid_data$date), end =max(covid_data$date)) ,
            actionButton("update", "Update")
            ),
            
        
        # Show a plot of the generated distribution
        mainPanel(
           textOutput("new_cases_deaths"),
           plotOutput("linePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$new_cases_deaths <- renderText({
        req(input$update)
        select_date <- input$date
        select_state <- input$state
        
        selected_data <- covid_data %>%
            filter(date == select_date & state == select_state)
        
        paste("State: ", select_state, "\n",
              "Date: ", select_date, "\n",
              "New Cases: ", selected_data$new_cases, "\n" ,
              "New Deaths: ", selected_data$new_deaths)
    })

    observeEvent(input$update, {
        # Update the plot based on the selected date and state
        output$linePlot <- renderPlot({
            selected_data <- covid_data %>%
                filter(date >= input$date_range[1] & date <= input$date_range[2] & state == input$state)
            
            ggplot(selected_data, aes(x = date)) +
                geom_line(aes(y = new_cases, color = "New Cases")) +
                geom_line(aes(y = new_deaths, color = "New Deaths")) +
                labs(title = paste("COVID-19 Cases and Deaths in", input$state),
                     x = "Date",
                     y = "Count") +
                scale_color_manual(values = c("New Cases" = "blue", "New Deaths" = "red")) +
                theme_minimal()
        })
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
