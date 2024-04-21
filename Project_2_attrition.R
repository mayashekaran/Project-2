library(shiny)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(dplyr)

# List of departments
Department <- c("All", "Human Resources","Research & Development","Sales")

# UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Project 2 Attrition app"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select CSV Files ----
      fileInput("file1", "Choose Attrition Data CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Radio buttons for plot type
      radioButtons("radio", label = h3("Choice of Plot"),
                   choices = list("Bar Graphs" = 1, "Boxplot" = 2), 
                   selected = 1),
      
      # Select Department
      selectInput("select", label = h3("Choose a Department for Scatterplot"), 
                  choices = Department,
                  selected = "All")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: bar or boxplot based on radio button selection
      plotOutput(outputId = "distPlot"),
      
      # Output: Scatterplot
      plotOutput(outputId = "scatterPlot")
      
    )
  )
)

# Server
server <- function(input, output) {
  
  # Load data when files are uploaded
  dataset <- reactive({
    req(input$file1)
    
    #connecting to the dataset - dataset provided for this project
    read.csv(input$file1$datapath, header= TRUE)
  })
  
  # Plot bar graph or boxplot based on radio button selection. 
  output$distPlot <- renderPlot({
    req(dataset())
    if (input$radio == 1) {
      bar1 <- ggplot(dataset(), aes(x = Gender, fill = Attrition)) +
        geom_bar() +
        labs(x = "Gender", y = "Number of Employees", title = "Number of Employees by Gender")
      bar2 <- ggplot(dataset(), aes(x = OverTime, fill = Attrition)) +
        geom_bar() +
        labs(x = "OverTime", y = "Number of Employees", title = "Number of Employees by Exempt Status")
      bar3 <- ggplot(dataset(), aes(x = BusinessTravel, fill = Attrition)) +
        geom_bar() +
        labs(x = "Business Travel", y = "Number of Employees", title = "Number of Employees who travel for business") +
        facet_wrap(~ Department)
      
      (bar1 + bar2) / bar3
    } else {
      plot1 <- ggplot(dataset(), aes(x = MaritalStatus,y = MonthlyIncome,fill = Attrition)) +
        geom_boxplot() +
        labs(x = "MaritalStatus", y = "Monthly Income", title = "Monthly Income by Marital Status")
      
      plot2 <- ggplot(dataset(), aes(x = JobRole,y = MonthlyIncome,fill = Attrition)) +
        geom_boxplot() +
        labs(x = "Job role", y = "Monthly Income", title = "Monthly Income by Job Role")
      
      plot1 / plot2
    }
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlot({
    req(dataset())
    if (input$select == "All") {
      # If input$select is "All", plot without filtering
      plot3 <- ggplot(dataset(), aes(x = TotalWorkingYears, y = PercentSalaryHike, color = Attrition)) +
        geom_point() +
        labs(title = "Scatterplot of Total Working Years Vs. Percent salary hike", x = "Total Working Years", y = "% Salary Hike") +
        scale_color_manual(values = c("No" = "lightblue", "Yes" = "red"))
      
      plot4 <- ggplot(dataset(),aes(x = YearsAtCompany, y = YearsSinceLastPromotion, color = Attrition)) +
        geom_point() +
        labs(title = "Scatterplot of Years at Company vs. Years since last promotion", x = "Years at Company", y = "Years since last promotion") +
        scale_color_manual(values = c("No" = "blue", "Yes" = "red"))
      
      plot3 / plot4
      
    } else {
      # If input$select is not "All", filter dataset by selected department and then plot
      plot5 <- ggplot(filter(dataset(), Department == input$select), aes(x = TotalWorkingYears, y = PercentSalaryHike, color = Attrition))  +
        geom_point() +
        labs(title = "Scatterplot of Total Working Years Vs. Percent salary hike", x = "Total Working Years", y = "% Salary Hike") +
        scale_color_manual(values = c("No" = "lightblue", "Yes" = "red"))
      
      plot6 <- ggplot(filter(dataset(), Department == input$select),aes(x = YearsAtCompany, y = YearsSinceLastPromotion, color = Attrition)) +
        geom_point() +
        labs(title = "Scatterplot of Years at Company vs. Years since last promotion", x = "Years at Company", y = "Years since last promotion") +
        scale_color_manual(values = c("No" = "blue", "Yes" = "red"))
      
      plot5 / plot6
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

