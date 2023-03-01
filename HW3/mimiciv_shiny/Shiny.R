#load needed packages
library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)
library(tidyverse)
library(lubridate)

#load necessary dataset
icu_cohort <- readRDS("icu_cohort.rds")

## demographics
demographvar <- c("age_at_admission","gender", "ethnicity", "language", 
             "insurance", "marital_status", "Bicarbonate", 
             "Chloride", "Creatinine", "Glucose", "Potassium", "Sodium",
             "Hematocrit", "White_Blood_Cells","Heart_Rate", 
              "Non_Invasive_Blood_Pressure_systolic",
              "Non_Invasive_Blood_Pressure_mean", "Respiratory_Rate",
              "Temperature_Fahrenheit")
#boxplot variable
boxplotvar <- c("age_at_admission","Bicarbonate", "Chloride", "Creatinine", "Glucose", "Potassium",
                "Sodium", "Hematocrit", "White_Blood_Cells", "Heart_Rate", 
                "Non_Invasive_Blood_Pressure_systolic", 
                "Non_Invasive_Blood_Pressure_mean", 
                "Respiratory_Rate", "Temperature_Fahrenheit")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny app for exploring the ICU cohort data!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "demo",
                  label = "Choose the variable : ",
                  choices = demographvar,
                  selected = "age_at_admission"
                  )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # helpText("Summary stastistics of selected variable:"),
      # tableOutput("demographtable"),
      # Output: 
      helpText("Plot of selected variable:"),
      plotOutput(outputId = "demoboxplot"),
      verbatimTextOutput("summary")
    )
  )
)
#create the server
server <- function(input, output) {
  output$demoboxplot <- renderPlot({
#separate the categoricol and the quantitative    
    if(input$demo %in% boxplotvar){
      
      icu_cohort %>%
        ggplot(., aes_string(x = input$demo)) +
        geom_boxplot(color = "black", fill = "pink") +
        coord_cartesian(xlim = 
        quantile(icu_cohort[input$demo], c(0.025, 0.95), na.rm = T)) +
        xlab(input$demo) + labs(title = paste0("Boxplot of ", input$demo))
      
      } else {
        icu_cohort %>%
          ggplot(., aes_string(x = input$demo)) +
          geom_bar(aes_string(x = input$demo, fill = icu_cohort$day30mort)) +
          labs(title = paste0("Bar chart of ", input$demo))
      }
  })
  #create the table chart in the ui
  output$summary <- renderPrint({
    if (input$demo %in% boxplotvar) {
    summary(icu_cohort[input$demo])
      } else
      {
    table(icu_cohort[input$demo])   
      }
  })
}
#run the dataset
shinyApp(ui = ui, server = server)