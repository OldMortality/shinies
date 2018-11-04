# app 13.
# height vs finger length - your group
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shinyTable)


shinyUI <- dashboardPage(
  
  
  
  dashboardHeader(title = "Height vs finger length - your group",
                  titleWidth = 550),
  dashboardSidebar(useShinyjs(),
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample (n=10)"),
                   checkboxInput("showalllines", "Show all red lines", FALSE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 12,
             
             box(title="Enter your group's data",
                 box(title="Enter data you have collected",width=NULL,
                     htable("tbl",
                            colHeaders = 'provided'),
                     actionButton("actionButtonID","Save")
                     
                 )
             ),
             box( 
               title="", 
               plotOutput("thePlot") )
      )
    )
  )
)

 