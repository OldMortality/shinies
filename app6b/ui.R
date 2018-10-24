# app 6b.

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Confidence intervals for the mean",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample"),
                   sliderInput("mu.2", "Blue mean:",
                               min = 1400, max = 2000, value = 1740,step=1
                   ),
                   
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100))#,
                   #checkboxInput("showtruemean", "Show true mean", TRUE),
                   #checkboxInput("showerrs", "Color errors", TRUE)
  ),
  
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
             box( 
               title="Sampling distribution", 
               width=NULL,
               plotOutput("samplingdistribution",height=200),
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

