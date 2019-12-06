# app 6.
# intervals
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)



shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Confidence intervals for the population mean",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs(),
                   # link to the css stylesheet. It is in the www folder.
                   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                   
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample"),
                   actionButton("sample10",label="Take 10 samples"),
                   actionButton("sample100",label="Take 100 samples"),
                   uiOutput("start"),
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100))
                   ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box( 
               title="Distribution of height for the population of HUBS191 students.", 
               width=NULL,
               plotOutput("plot1",height=200), 
               height = 275),
             box( 
               width=NULL,
               plotOutput("thissamplemean",height=80),
               height = 100),
             box(
                 width=NULL,
                 plotOutput("samplemean",height=350), 
                 height = 400)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('sampleSummary',height=200), 
               height = 275),
             box( 
               title=htmlOutput('onesamplesummary',height=80), 
               width=NULL, 
               height = 100),
             box( 
               width=NULL,
               title="Confidence intervals for all samples", 
               htmlOutput('sampleMeanSummary',height=350), 
               height = 400)
      )
    )
  )
)

 