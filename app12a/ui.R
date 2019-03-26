
# app 12a.
# height vs finger length
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shiny)
library(shinyTable)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Estimating a linear relationship",
                  titleWidth = 850),
  dashboardSidebar(
    useShinyjs(),
    # link to the css stylesheet. It is in the www folder.
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    
    checkboxInput("showdata", "Show your data", FALSE),
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

 