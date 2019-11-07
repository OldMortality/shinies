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
##library('shinyjs)
#library('ggplot2)
#library('shinyBS)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "HUBS191. Lab 4: Anatomy & Physiology of the Nervous Systems",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs()
                   ),
  dashboardBody(
    fluidRow(
      column(width = 6,height=2, 
             box(
               title="Movement in response to sensory stimuli", width=NULL,
               tags$button("", label = "Movement in response to sensory stimuli", 
                           tags$img(src = "app9.png",
                                    height = "150px"),
                           onclick = "window.open('/app9/', '_blank')")
             )
             
      ))
  )
) 