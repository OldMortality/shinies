# app 5.
# 
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "The distribution of sample means",
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
                                  "100"= 100)),
                   checkboxInput("shownormal",
                                 "Show Normal distribution curve", FALSE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box( 
               title="Distribution of the height for the population of HUBS191 students", 
               width=NULL,
               plotOutput("populationPlot",height=300), 
               height = 400),
             box( 
               width=NULL,
               plotOutput("thissamplemean",height=50),
               height = 75),
             box(title="Means of all samples",  
                width=NULL,
                plotOutput("samplemean",height=300), 
                height = 400)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
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
               htmlOutput('sampleMeanSummary',height=300), 
               height = 400)
      )
    )
  )
)

 