# app 12.
# height vs finger length
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shiny)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Sampling variation in regression lines",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   # link to the css stylesheet. It is in the www folder.
                   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                   
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample"), 
                   
                   radioButtons("n", "Sample size:",
                    c("10" = 10,
                    "50" = 50,
                    "100"= 100)),
                   checkboxInput("showall","Show all (N=615)",FALSE), 
                   checkboxInput("showalllines", "Show all red lines", FALSE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 12,
             box( 
               title="", 
               width=NULL,
               plotOutput("thePlot",height=600), 
               height = 600)
            )
    )
  )
)

 