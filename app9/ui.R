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
  
  dashboardHeader(title = 
                  "Movement in response to sensory stimuli",
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
                 plotOutput("distPlot", height = 450)
             )
          )),
      fluidRow(
        column(width = 6,
               box(title="Enter data you have collected",width=NULL,
                   htable("tbl",
                          colHeaders = 'provided'),
                   actionButton("actionButtonID","Save")
                   
                   
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
    
  
