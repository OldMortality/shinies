# app 4
# height plots and table, 90 people with 
#   2 errors and 1 true outlier
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)



shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Task A4. Heights for a lab stream (n=90)",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(), 
                   checkboxInput("showhist", "Histogram", FALSE),
                   checkboxInput("showbox", "Boxplot", FALSE),
                   checkboxInput("showdot", "Dotplot", FALSE) 
  ), 
  dashboardBody(
    fluidRow(
      column(width = 6,
             box(title="Heights of 90 HUBS191 students",width=NULL,
                 rHandsontableOutput("hot", width = 200),
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


 