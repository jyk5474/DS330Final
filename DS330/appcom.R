# Nessecary Packages for app
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tibble")
# install.packages("stringr")


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tibble)
library(stringr)

responses <- read.csv('responses.csv')

# Recodeing
responses$Smoking <- gsub(" ", "", responses$Smoking)
responses$smoking_numeric <- ifelse(responses$Smoking == "neversmoked", 0,
                                    ifelse(responses$Smoking == "triedsmoking",0,
                                           ifelse(responses$Smoking == "formersmoker", 1,
                                                  ifelse(responses$Smoking == "currentsmoker", 1, NA))))

responses$smoking_numeric <- as.integer(responses$smoking_numeric)

na.omit(responses$Workaholism)
na.omit(responses$smoking_numeric)

#UI
ui <- fluidPage(
  titlePanel("Student visualization dashboard"),
  fluidRow(
    column(width = 6,
           # App 1
           titlePanel("Comparing Density graphs between hobbies and fears(red)"),
           sidebarLayout(
             sidebarPanel(
               selectInput("y", "Fear:",
                           c("Flying", "Darkness", "Heights", "Snakes")),
               selectInput("x", "Hobby:",
                           c("Religion","Writing",
                             "Dancing", "Shopping", "Adrenaline.sports", "Fun.with.friends", "Cars" ))
             ),
             mainPanel(
               plotOutput(outputId = "density", width = "110%"),
               textOutput(outputId = "myNumber")
             )
           )
    ),
    column(width = 6,
           # App 2
           titlePanel("Mean scores of hobby selected by gender"),
           fluidRow(
             column(width = 12,
                    plotOutput(outputId = "barplot", width = "80%", height = "400px")
             )
           )
    )
  ),
  
  # App 3
  titlePanel("Regression Analysis for smoking based on life style"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x2","Independent variable", c("Gender","Healthy.eating",
                                                 "Loss.of.interest","Age", "God")),
      
    ),
    
    mainPanel(textOutput(outputId = "regression_model"))
  )
  
) 

#Server
server <- function(input, output) {
  
  # App 1
  data_and_corr <- reactive({
    p <- na.omit(responses[, c(input$x, input$y), drop = FALSE])
    myCorr <- cor(as.numeric(p[[input$x]]), as.numeric(p[[input$y]]))
    list(data = p, corr = myCorr) 
  })
  
  output$density <- renderPlot({
    p <- data_and_corr()$data
    ggplot(p, aes_string(x = input$x, y = "..density..")) +
      geom_density() +
      geom_density(aes_string(x = input$y), color = "red")
  })
  
  output$myNumber <- renderText({
    myNumber <- data_and_corr()$corr
    paste0("The correlation Cofficient is ", myNumber)
  })
  
  # App 2
  output$barplot <- renderPlot({
    p <- responses[, c(input$x, "Gender")]
    p <- p %>% 
      group_by(Gender) %>% 
      summarize(`Mean Score` = mean(.data[[input$x]], na.rm = TRUE))
    p = p[-1,]
    
    ggplot(data = p, aes( y = `Mean Score`, x = Gender,  fill= Gender)) +
      geom_bar(stat = "identity") +
      labs(x = "Gender", y = "Mean Score", title = paste("Mean Scores for", input$hobby, "by Gender"))
  })
  #App3
  output$regression_model <- renderPrint({
    
    p <- responses[, c(input$x2, "smoking_numeric"), drop = FALSE]
    lm_model <- lm(as.formula(paste("smoking_numeric ~", input$x2)), data = p)
    regsum <- summary(lm_model)
    regsum
    
  })
}

shinyApp(ui = ui, server = server)

