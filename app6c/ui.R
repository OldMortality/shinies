# app 6c.
# like 6b,but you type in your own sample data, rather
#   than take samples.
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

shinyUI <- dashboardPage(
  
  dashboardHeader(title = "What can we say based on a single sample mean?",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs(),
                   # link to the css stylesheet. It is in the www folder.
                   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                   
                    sliderInput("mu.2", "Hypothesized mean:",
                                min = 1400, max = 2000, value = 1740,step=1
                               ),
                    textInput("samplemean", label = h4("Your sample mean (mm)")
                             ,value=1750),
                    textInput("samplesd", label = h4("Your sample sd (mm)")
                             ,value=213),
                    textInput("samplesize", label = h4("Your sample size")
                             ,value=10),
                    checkboxInput("showsamplemean","Show sample mean and CI",FALSE),
                    checkboxInput("showsampledist","Show sampling distribution",TRUE) 
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box( 
               title="Population (unknown)", 
               width=NULL,
               plotOutput("plot1",height=140), 
               height = 200),
             box( 
               title="Confidence interval",
               width=NULL,
               plotOutput("thissamplemean",height=65),
               height = 125),
             box( 
               title="Distribution of all sample means (unknown)", 
               width=NULL,
               plotOutput("samplingdistribution",height=390),
               height = 450) 
      ), 
      column(width=6, 
             box(  
               title="", 
               width=NULL,
               #htmlOutput('sampleSummary',height=400), 
               height = 200),
             box( 
               title="",
               htmlOutput('onesamplesummary',height=75), 
               width=NULL,
               height = 125),
             box( 
               width=NULL,
               title="", 
               htmlOutput('sampleMeanSummary',height=390), 
               height = 450)
      )
    )
  )
)

