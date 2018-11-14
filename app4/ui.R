# app 4
# height plots and table, 90 people with 
#   2 errors and 1 true outlier
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shinyTable)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Task A4. How tall are the people in a HUBS191 lab stream?",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs(), 
                   checkboxInput("showhist", "Histogram", FALSE),
                   checkboxInput("showbox", "Boxplot", FALSE),
                   checkboxInput("showdot", "Dotplot", FALSE) 
  ), 
  dashboardBody(
    fluidRow(
      column(width = 6,
             box(title="Heights of 90 HUBS191 students",width=NULL,
                 htable("tbl",
                        colHeaders = 'provided'),
                 actionButton("actionButtonID","Save"),
                 htmlOutput('summary', height = 100)
             )
             
             
      ),
      column(width=6,
             box(
               title="Histogram of height", width=NULL,
               conditionalPanel(
                 condition = "input.showhist",
                 plotOutput("histogram", height = 400)
               )
             ),
             box(
               title="Boxplot of height", width=NULL,
               conditionalPanel(
                 condition = "input.showbox",
                 plotOutput("boxplot", height = 400)
               )
             ),
             box(
               title="Dotplot of height", width=NULL,
               conditionalPanel(
                 condition = "input.showdot",
                 plotOutput("dotplot", height = 400)
               )
             ) 
      )
    )
    
  )
)


 