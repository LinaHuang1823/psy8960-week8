library(shiny)
library(tidyverse)
library(rsconnect)


week8_tbl<-readRDS("../shiny/week8_tbl.rds")

ui <- fluidPage(
  titlePanel("Mean Scores on Q1-Q6 vs Mean Scores on Q8-Q10"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "gender",
                  label = "Select Gender",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      selectInput(inputId = "error",
                  label = "Select Error Band",
                  choices = c("Display Error Band", "Suppress Error Band"),
                  selected = "Display Error Band"),
      selectInput(inputId = "date",
                  label = "Include Assessments After August 1, 2017?",
                  choices = c("Include", "Exclude"),
                  selected = "Include")
    ),
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

server <- function(input, output) {
  
  
  # Filter data based on user input
  filtered_data <- reactive({
    if (input$gender == "All") {
      data<- week8_tbl
    } else {
      data <- dplyr::filter(week8_tbl, gender == input$gender)
    }
    if (input$date == "Include") {
      data<- week8_tbl
    } else {  
      data <- dplyr::filter(week8_tbl,timeEnd >= "2017-08-01")
    }
    return(data)
  })
  
  # Create scatterplot
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
