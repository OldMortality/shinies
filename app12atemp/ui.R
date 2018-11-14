# app 12a.
# height vs finger length
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shiny)
library(shinyTable)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Task B.2 Estimating a linear relationship",
                  titleWidth = 850),
  dashboardSidebar(
    useShinyjs(),
    checkboxInput("showline", "Show your fitted line", FALSE)
    ),
                   
  dashboardBody(
    
    fluidRow( 
      column(width = 6,
             box( 
               title="Enter the height and digit length of your table", 
               width=NULL,
               htable("tbl",
                      colHeaders = 'provided'),
               tags$style(HTML(".actionButtonID-inline {margin-top: 42px;}")),
               actionButton("actionButtonID","Save"), 
               height = 900)
            ),
      column(width = 6,
             box( 
               title="", 
               width=NULL,
               plotOutput("thePlot",height=500), 
               height = 600),
             box( 
               title="Line of best fit", 
               width=NULL,
               htmlOutput('summary', height = 100), 
               height = 300)
      )
    )
  )
)

 