# app 6.
# intervals
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
#library(DT)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Task A.7 Confidence intervals for the population mean",
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
                   checkboxInput("showtruemean", "Show true mean", TRUE),
                   checkboxInput("showerrs", "Color errors", TRUE)
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
               title="Confidence interval",
               width=NULL,
               plotOutput("thissamplemean",height=50),
               height = 125),
             box(title="Confidence intervals of all samples",  
                 width=NULL,
                 plotOutput("samplemean",height=500), 
                 height = 575)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('sampleSummary',height=200), 
               height = 275),
             box( 
               title=htmlOutput('onesamplesummary',height=50), 
               width=NULL, 
               height = 125),
             box( 
               width=NULL,
               title="Confidence intervals for all samples", 
               htmlOutput('sampleMeanSummary',height=500), 
               height = 575)
      )
    )
  )
)

 