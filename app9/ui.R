# app 9
# Plots of 2 variables
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)
library(gridExtra)


data <- read.csv('Lab3data.csv',header=T)


ui <- dashboardPage(
  
  dashboardHeader(title = paste("Lab 4: Experiment 2.",
                  "Voluntary movement in response to sensory stimulus"  ),
                  titleWidth = 850),
  dashboardSidebar(useShinyjs(),
                   
                   
                   selectInput("plot.type", h3("Graph type"), 
                               choices = list("Histogram" = 1, 
                                              "Scatter" = 2,
                                              "Boxplot" = 3
                               ), selected = 1),
                   uiOutput("B_ui"),
                   checkboxInput("showmean", "Show population mean (red)", FALSE),
                   
                    uiOutput("checkboxes")
                   
                   
                   
                   
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 12,
             box(title="", width=NULL,
                 plotOutput("distPlot", height = 300)
             )
          )),
      fluidRow(
        column(width = 6,
               box(title="Enter data you have collected",width=NULL,
                   rHandsontableOutput("hot", width = 800)
                   
               )),
        column(width = 6,
               
               box(  
                 title="Summary", 
                 width=NULL,
                 htmlOutput('summary')
               ))
        )
      )
    )
    
  
