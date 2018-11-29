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
                   checkboxInput("showdots", "Show dotplots", FALSE)
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


 