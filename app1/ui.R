# app 1
# Showing the population height distribution,
#   and allows you to find your own height
#
library(shinydashboard)
library(ggplot2) 

shinyUI <- dashboardPage(
  
  dashboardHeader(
     
    title = "How tall are HUBS191 students?",
                  titleWidth = 800),
  dashboardSidebar(
    tags$head(
      # link to the css stylesheet. It is in the www folder.
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
      checkboxInput("showmean", "Show population mean (red)", FALSE),
      checkboxInput("showsd", "Show standard deviation", FALSE),
      checkboxInput("showyourheight", "Show your height (blue)", FALSE),
      textInput("yourheight", label = h4("Enter your height (mm)")
                ,value=1750)
      ), 
  dashboardBody( 
    tags$head(
      # link to the css stylesheet. It is in the www folder.
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow( 
      column(width = 12,
              box(
                title="Distribution of height for the population of HUBS191 students", 
                width=NULL,
                plotOutput("plot1", height = 350)), 
              box( 
                title="", 
                width=NULL,
                htmlOutput('summary')
              )
            )
      )
  )
)

