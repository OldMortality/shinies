#
# Shiny dashboard with all my apps for lab4
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
  
  dashboardHeader(title = "HUBS191. Lab 4: Statistics",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs()
                   ),
  dashboardBody(
    fluidRow(
      column(width = 6,height=2, 
             box(
               title="Lab 4. Voluntary movement in response to sensory stimulus ", width=NULL,
               tags$button("", label = "Lab 4. Voluntary movement in response to sensory stimulus ", 
                           tags$img(src = "app9.png",
                                    height = "150px"),
                           onclick = "window.open('/app9/', '_blank')")
             )
             
      ))
  )
) 