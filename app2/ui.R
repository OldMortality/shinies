# app 2
# Plots of height
#   
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(shinyTable)
 
shinyUI <- dashboardPage( 
  dashboardHeader(title = "How tall are the people at your table?",
                  titleWidth = 800),
  dashboardSidebar(useShinyjs(), 
                   # link to the css stylesheet. It is in the www folder.
                   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                   
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
                  htable("tbl",
                         colHeaders = 'provided'),
                  actionButton("actionButtonID","Save"),
                  htmlOutput('summary'),
                  height = 450
              ),
              box(
               title="Histogram of height", width=NULL,
               conditionalPanel(
                 condition = "input.showhist",
                 plotOutput("histogram", height = 390)
                ),
               height=450
              )
             ),
      column(width=6,
              box(
                title="Boxplot of height", width=NULL,
                conditionalPanel(
                  condition = "input.showbox",
                  plotOutput("boxplot", height = 390)
                ),
                height = 450
              ),
              box(
                title="Dotplot of height", width=NULL,
                conditionalPanel(
                  condition = "input.showdot",
                  plotOutput("dotplot", height = 390)
                ),
                height = 450
              ) 
            )
        )
      
      )
    ) 
