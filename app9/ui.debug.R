# app 9
# Plots of 2 variables
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

 
library(gridExtra)
library(shinyTable)

ui <- dashboardPage(
  
  dashboardHeader(title = paste("Lab 4: Experiment 2.",
                  "Voluntary movement in response to sensory stimulus"  ),
                  titleWidth = 850),
  dashboardSidebar( 
                   
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
      fluidRow(
        column(width = 6,
                box(title="Enter data you have collected",width=NULL,
                   htable("tbl",
                          colHeaders = 'provided')
                   )
               )
          )
      )
    )
    
  
