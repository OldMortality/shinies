# app 2
# Plots of height
#   
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)
 
shinyUI <- dashboardPage( 
  dashboardHeader(title = "Task A.3 How tall are the people at your table?",
                  titleWidth = 800),
  dashboardSidebar(useShinyjs(), 
                   checkboxInput("showhist", "Histogram", FALSE),
                   checkboxInput("showbox", "Boxplot", FALSE),
                   checkboxInput("showdot", "Dotplot", FALSE) 
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
              box(title="Enter the height of your table members",
                  width=NULL,
                  rHandsontableOutput("hot", height = 300),
                  htmlOutput('summary', height = 100)
              ),
              box(
               title="Histogram of height", width=NULL,
               conditionalPanel(
                 condition = "input.showhist",
                 plotOutput("histogram", height = 300)
                )
              )
             ),
      column(width=6,
              box(
                title="Boxplot of height", width=NULL,
                conditionalPanel(
                  condition = "input.showbox",
                  plotOutput("boxplot", height = 373)
                )
              ),
              box(
                title="Dotplot of height", width=NULL,
                conditionalPanel(
                  condition = "input.showdot",
                  plotOutput("dotplot", height = 300)
                )
              ) 
            )
        )
      
      )
    ) 
