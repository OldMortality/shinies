
# Shiny dashboard with all my apps
library(shiny)
library(shinydashboard)
#library(shinyjs)
#library(ggplot2)
#library('shinyBS)

shinyServer <- function(input, output) {  
  output$summary <- renderText(
    getSummary()
  )
  
}
