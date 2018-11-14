# app 8.
# Comparing 2 groups: difference in means.
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


shinyUI <- dashboardPage(
  dashboardHeader(title = paste("Task C.3 Sampling distribution for a ",
                                "difference in sample means",sep=' '), 
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
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box( 
               title=paste("Distribution of height for the population", 
                           "of HUBS191 students",sep=' '),
               width=NULL,
               plotOutput("plot1",height=375), 
               height = 450),
             box( 
               title="Sample mean",
               width=NULL,
               plotOutput("thissamplemean",height=75),
               height = 150),
             box( 
               title="Red sample mean minus blue sample mean",
               width=NULL,
               plotOutput("difference",height=75),
               height = 150),
             box(title="Difference",  
                 width=NULL,
                 plotOutput("samplemean",height=375), 
                 height = 450)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('summary1',height=375), 
               height = 450),
             box( 
               title="",
               htmlOutput('summary2',height=75), 
               width=NULL,
               height = 150),
             box( 
               title="",
               htmlOutput('summary3',height=75), 
               width=NULL,
               height = 150),
             box( 
               width=NULL,
               title="All samples", 
               htmlOutput('summary4',height=375), 
               height = 450)
      )
    )
  )
)
