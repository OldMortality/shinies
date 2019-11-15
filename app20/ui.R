# app 6.
# intervals
library(shiny)
library(shinydashboard)



shinyUI <- dashboardPage( 
  
  dashboardSidebar(tags$link(rel = "stylesheet", type = "text/css", href = "custom.xcss")),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(column(width = 6 ), 
      
    )
  )
)