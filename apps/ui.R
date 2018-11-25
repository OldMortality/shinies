# Shiny dashboard with all my labs
#   copy this one to shiny app: hubsapps, with 1 global change:
#   hubsapps:   window.open('../app1
#   instead of
#   allapps:    window.open('/app1
#
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shinyBS)


shinyUI <- dashboardPage(
  
  
  dashboardHeader(title = "HUBS191. Statistics",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs()
                   ),
  dashboardBody(
    tags$head(tags$style(
      #HTML('
      #     body, label, input, button, select { 
      #     font-family: "Calibri";
      #     background-color: blue;
      #     }')
    )),
    
    fluidRow(
      column(width = 6,height=2,
             box(
               title="HUBS191. Lab2: Statistics", 
               width=NULL,
               tags$button("", label = "HUBS191. Lab2: Statistics", 
                           tags$img(src = "lab2.png",
                                    height = "150px"),
                            onclick ="window.open('/lab2/', '_blank')")
               ),
             box(
               title="HUBS191. Lab4: Anatomy and Physiology of the nervous system", 
               width=NULL,
               tags$button("", label = "HUBS191. Lab4: Anatomy and Physiology of the nervous system", 
                           tags$img(src = "lab4.png",
                                    height = "150px"),
                           onclick ="window.open('/lab4/', '_blank')")
             ),
             box(
               title="HUBS191. Lab5: Sensory and motor physiology", 
               width=NULL,
               tags$button("", label = "HUBS191. Lab2: Statistics", 
                           tags$img(src = "lab5.png",
                                    height = "150px"),
                           onclick ="window.open('/lab5/', '_blank')")
             )
             
            
            
             
      ))
  )
) 