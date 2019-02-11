# app 16
# 
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
 


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Joint laxity",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(), 
                   # link to the css stylesheet. It is in the www folder.
                   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                   
                   checkboxInput("showdots", "Show dotplots", FALSE),
                   checkboxInput("showpre",  "Show summary pre-puberty", FALSE),
                   checkboxInput("showpost", "Show summary post-puberty", FALSE)
  ), 
  dashboardBody(
    fluidRow(
      
      column(width=6,
             box(
               title="Dotplot - pre-puberty", width=NULL,
               conditionalPanel(
                 condition = "input.showdots",
                 plotOutput("dot.pre", height = 400)
               )
             ),
             box( 
               title="Pre-puberty", 
               width=NULL,
               htmlOutput('summary.pre', height = 200), 
               height = 300) ),
      column(width=6,
             box(
               title="Dotplot - post-puberty", width=NULL,
               conditionalPanel(
                 condition = "input.showdots",
                 plotOutput("dot.post", height = 400)
               )
             ),
             box( 
               title="Post-puberty", 
               width=NULL,
               htmlOutput('summary.post', height = 200), 
               height = 300)
        )
      
    )
    
  )
)


 