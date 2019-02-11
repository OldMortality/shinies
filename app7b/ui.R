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
                   checkboxInput("shownormal", "Show Normal", TRUE)),
  dashboardBody(
    # 
    fluidRow( 
      column(width = 6,
             box( 
               title=paste("Distribution of height for the population", 
                           "of HUBS191 students",sep=' '),
               width=NULL,
               plotOutput("plot1",height=275), 
               height = 350),
             box( 
               width=NULL,
               plotOutput("thissamplemean",height=50),
               height = 80),
              
             box(title="Means of all samples",  
                 width=NULL,
                 plotOutput("samplemean",height=275), 
                 height = 350)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('topSummary',height=275), 
               height = 350),
             box( 
               title=htmlOutput('middleSummary',height=50), 
               width=NULL,
               height = 80),
             box( 
               width=NULL,
               title="All samples", 
               htmlOutput('bottomSummary',height=275), 
               height = 350)
      )
    )
  )
)
 