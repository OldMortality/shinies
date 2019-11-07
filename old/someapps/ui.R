#
# Shiny dashboard with all a sample of apps for HUBS server
#   this is a copy of allapps, with
#   hubsapps:   window.open('../app1
#   instead of
#   allapps:    window.open('/app1
#
#
# 
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shinyBS)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "A few of my apps",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs()
                   ),
  dashboardBody(
    fluidRow(
      column(width = 6,height=2,
             
              
                
             
             
             
             box(
               title="Task A.7 Confidence intervals for the population mean height", width=NULL,
               tags$button("", label = "Task A.7 Confidence intervals for the population mean height", 
                           tags$img(src = "app6.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app6/', '_blank')")
             ),
             
            box(
              title="Task B.3 Sampling variation in regression lines", width=NULL,
              tags$button("", label = "B.3 Sampling variation in regression lines", 
                          tags$img(src = "app12.png",
                                   height = "150px"),
                          onclick = "window.open('https://oldmortality.shinyapps.io/app12/', '_blank')")
            ),
            
             box(
               title="Task C.2 Comparing the sampling distribution for two means", 
               width=NULL,
               tags$button("", label = "Task C.2 Comparing the sampling distribution for two means", 
                           tags$img(src = "app7b.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app7b/', '_blank')")
             ),
             
             box(
               title="11", width=NULL,
               tags$button("", label = "11. Comparing nerve conduction velocity", 
                           tags$img(src = "app11.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app11/', '_blank')")
             )
             
      ))
  )
) 