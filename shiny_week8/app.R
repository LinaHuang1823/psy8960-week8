library(shiny)
library(tidyverse)
library(rsconnect)

#load data
week8_tbl<-readRDS("../shiny/week8_tbl.rds") 

ui <- fluidPage(
  titlePanel("Mean Scores on Q1-Q6 vs Mean Scores on Q8-Q10"), # Setting title of the page
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "gender", # Creating a dropdown menu for selecting gender
                  label = "Select Gender",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      selectInput(inputId = "error", # Creating a dropdown menu for displaying/suppressing error band
                  label = "Select Error Band",
                  choices = c("Display Error Band", "Suppress Error Band"),
                  selected = "Display Error Band"),
      selectInput(inputId = "date", # Creating a dropdown menu for including/excluding assessments after August 1, 2017
                  label = "Include Assessments After August 1, 2017?",
                  choices = c("Include", "Exclude"),
                  selected = "Include")
    ),
    mainPanel(
      plotOutput(outputId = "scatterplot") # Generating the scatterplot output
    )
  )
)
# Define the server function with input and output arguments
server <- function(input, output) {
  
  
# Create a reactive expression to filter data based on user input using if and else statement
  filtered_data <- reactive({
    if (input$gender == "All") { #If the user selects "All" genders, return the entire data table
      data<- week8_tbl
    } else {    #Otherwise, filter the data by the selected gender
      data <- dplyr::filter(week8_tbl, gender == input$gender)
    }
    if (input$date == "Include") { # If the user selects to include all dates, return the entire data table
      data<- week8_tbl
    } else {   # Otherwise, filter the data to only include dates after August 1, 2017
      data <- dplyr::filter(week8_tbl,timeEnd >= "2017-08-01") 
    }
    return(data) # Return the filtered data
  })
  
  # Create scatterplot with renderplot for interactive effect
  output$scatterplot <- renderPlot({
    filtered_data() %>%
      # Calculate the mean scores for each participant
      mutate(Q1_Q6_Mean = rowMeans(select(., q1:q6)),
             Q8_Q10_Mean = rowMeans(select(., q8:q10))) %>%
      # Create a scatterplot with regression line and error band
      ggplot(aes(x = Q1_Q6_Mean, y = Q8_Q10_Mean)) +
      geom_point(position = "jitter") +
      geom_smooth(method = "lm", se = ifelse(input$error == "Display Error Band", TRUE, FALSE), color = "purple") +
      labs(title = "Mean Scores on Q1-Q6 vs Mean Scores on Q8-Q10",
           x = "Mean Scores on Q1-Q6",
           y = "Mean Scores on Q8-Q10") 
    
  })
  
  
}

shinyApp(ui = ui, server = server)

