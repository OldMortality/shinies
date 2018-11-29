# app 11.
# 
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)



shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Comparing 2 groups: Conduction velocity",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: blue}")),
                   
                   actionButton("clear",label="Clear"),
                   actionButton("sampleMany",label="Take 200 samples"),
                   sliderInput("mu.red", "Red mean:",
                               min = 10, max = 80, value = 55,step=0.25
                   ),
                   #bsTooltip(id = "sampleMany", title = "This is an input", 
                   #           placement = "top", trigger = "hover"),
                   
                   sliderInput("mu.2", "Blue mean:",
                               min = 10, max = 80, value = 40,step=0.25
                   ),
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100)),
                   textInput("observed", label = "observed difference", value = "21"),
                   checkboxInput("show.perc", "Show percentile", FALSE),
                   checkboxInput("show.mean", "Show mean", FALSE),
                   checkboxInput("show.norm", "Show Normal", FALSE),
                   checkboxInput("show.obs", "Show observed difference", FALSE)
                   
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
               width=NULL,
               plotOutput("difference",height=50),
               height = 75),
             box(title="Red sample mean minus blue sample mean",  
                 width=NULL,
                 plotOutput("samplemean",height=200), 
                 height = 250)
      ), 
      column(width=6, 
             box(  
               title="", 
               width=NULL,
               htmlOutput('sampleSummary',height=200), 
               height = 250),
             box( 
               title=htmlOutput('onesamplesummary',height=50), 
               width=NULL,
               
               height = 75),
             box( 
               title=htmlOutput('differencesummary',height=50), 
               width=NULL,
               
               height = 75),
             box( 
               width=NULL,
               title="Summary", 
               htmlOutput('sampleMeanSummary',height=200), 
               height = 250)
      )
    )
  )
  
  
)

 