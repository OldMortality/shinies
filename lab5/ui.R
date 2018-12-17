# lab5 app
# Shiny dashboard with all my apps for lab5
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
  
  dashboardHeader(title = "HUBS191. Lab 5: Sensory and motor physiology",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs()
                   ),
  dashboardBody(
    fluidRow(
      column(width = 6,height=2, 
             
             box(
               title="Nerve conduction velocity", width=NULL,
               tags$button("", label = "Nerve conduction velocity", 
                           tags$img(src = "app14.png",
                                    height = "150px"),
                           onclick = "window.open('/app14/', '_blank')")
             ) 
             
      ))
  )
) 