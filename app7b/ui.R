# app 7b.
# Comparing 2 groups: 2 sample distributions
#   in 1 plot
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Comparing the sampling distribution for two means",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs(),
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample"),
                   actionButton("sample10",label="Take 10 samples"),
                   actionButton("sample100",label="Take 100 samples"),
                   uiOutput("start"),
                   
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
               title=paste("Distribution of height for the population", 
                           "of HUBS191 students",sep=' '),
               width=NULL,
               plotOutput("plot1",height=375), 
               height = 450),
             box( 
               width=NULL,
               plotOutput("thissamplemean",height=100),
               height = 150),
              
             box(title="Means of all samples",  
                 width=NULL,
                 plotOutput("samplemean",height=375), 
                 height = 450)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('topSummary',height=375), 
               height = 450),
             box( 
               title=htmlOutput('middleSummary',height=100), 
               width=NULL,
               height = 150),
             box( 
               width=NULL,
               title="All samples", 
               htmlOutput('bottomSummary',height=375), 
               height = 450)
      )
    )
  )
)
 