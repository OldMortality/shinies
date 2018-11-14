# app 7.
# like app5, but binary population
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2) 

shinyUI <- dashboardPage(
  
  dashboardHeader(title = "B.1 Estimating a proportion",
                  titleWidth = 850),
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
    # 
    fluidRow( 
      column(width = 6,
             box( 
               title=paste("Distribution for frequent exercise (Yes/No)", 
                           " in the population of HUBS191 students.",sep=' '),
               width=NULL,
               plotOutput("plot1",height=300), 
               height = 400),
             box( 
               width=NULL,
               plotOutput("thissamplemean",height=50),
               height = 75),
             box(title="Distribution of all sample proportions",  
                 width=NULL,
                 plotOutput("samplemean",height=400), 
                 height = 500)
      ), 
      column(width=6, 
             box(  
               title="", 
               width=NULL,
               htmlOutput('sampleSummary',height=300), 
               height = 400),
             box( 
               title=htmlOutput('onesamplesummary',height=50), 
               width=NULL,
               
               height = 75),
             box( 
               width=NULL,
               title="All samples", 
               htmlOutput('sampleMeanSummary',height=400), 
               height = 500)
      )
    )
  )
)

 