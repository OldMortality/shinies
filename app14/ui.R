# app 14
# enter 10 velocities.show dot plot and summary
#   
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(rhandsontable)

shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Group velocity",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs()
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
             box(title="Enter your group data",width=NULL,
                 rHandsontableOutput("hot", width = 200)
             ),
             box(  
               title="Summary", 
               width=NULL,
               htmlOutput('summary',height = 300),
               height=300
               
             )
             
            ),
      
      column(width = 6,       
             box(
               title="Dotplot", 
               width=NULL,
                plotOutput("dotplot", height = 300)
                
             )) 
      
    )
  )
)

