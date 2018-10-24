# app 7b.
# Comparing 2 groups: 2 sample distributions
#   in 1 plot
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Comparing 2 groups",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample"),
                   actionButton("sample10",label="Take 10 samples"),
                   actionButton("sample100",label="Take 100 samples"),
                   actionButton("start",label="Start "),
                   actionButton("stop",label="Stop "),  
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100)),
                   checkboxInput("shownormal", "Show Normal", TRUE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box( 
               title="Population", 
               width=NULL,
               plotOutput("CLTplot1",height=200), 
               height = 250),
             box( 
               width=NULL,
               plotOutput("thissamplemean",height=50),
               height = 75),
              
             box(title="Means of all samples",  
                 width=NULL,
                 plotOutput("samplemean",height=200), 
                 height = 250)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('sampleSummary',height=200), 
               height = 250),
             box( 
               title=htmlOutput('onesamplesummary',height=50), 
               width=NULL,
               
               height = 75),
              
             box( 
               width=NULL,
               title="All samples", 
               htmlOutput('sampleMeanSummary',height=200), 
               height = 250)
      )
    )
  )
)
 