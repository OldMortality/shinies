# app 8.
# Comparing 2 groups: difference in means.
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)



shinyUI <- dashboardPage(
  dashboardHeader(title = 
                    "Sampling distribution for a difference in sample means ",
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
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box( 
               title=paste("Distribution of height for the population", 
                           "of HUBS191 students",sep=' '),
               width=NULL,
               plotOutput("plot1",height=240), 
               height = 300),
             box( 
               title="Sample mean",
               width=NULL,
               plotOutput("thissamplemean",height=50),
               height = 110),
             box( 
               title="Red sample mean minus blue sample mean",
               width=NULL,
               plotOutput("difference",height=50),
               height = 110),
             box(title="Difference",  
                 width=NULL,
                 plotOutput("samplemean",height=300), 
                 height = 360)
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('summary1',height=240), 
               height = 300),
             box( 
               title="",
               htmlOutput('summary2',height=50), 
               width=NULL,
               height = 110),
             box( 
               title="",
               htmlOutput('summary3',height=50), 
               width=NULL,
               height = 110),
             box( 
               width=NULL,
               title="All samples", 
               htmlOutput('summary4',height=300), 
               height = 360)
      )
    )
  )
)
