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
               title="8", width=NULL,
               tags$button("", label = "8. Comparison", 
                           tags$img(src = "app8.png",
                                    height = "150px"),
                           onclick = "window.open('/app8/', '_blank')")
             ),
             box(
               title="9", width=NULL,
               tags$button("", label = "9. Plots of lab data (heights)", 
                           tags$img(src = "app9.png",
                                    height = "150px"),
                           onclick = "window.open('/app9/', '_blank')")
             ),
             box(
               title="10", width=NULL,
               tags$button("", label = "10. Guillain-Barré vs. normal", 
                           tags$img(src = "app10.png",
                                    height = "150px"),
                           onclick = "window.open('/app10/', '_blank')")
             ),
             box(
               title="11", width=NULL,
               tags$button("", label = "11. Comparing nerve conduction velocity", 
                           tags$img(src = "app11.png",
                                    height = "150px"),
                           onclick = "window.open('/app11/', '_blank')")
             ),
             box(
               title="12a", width=NULL,
               tags$button("", label = "12a. ", 
                           tags$img(src = "app12a.png",
                                    height = "150px"),
                           onclick = "window.open('/app12a/', '_blank')")
             ),
             
             box(
               title="13", width=NULL,
               tags$button("", label = "13. Height vs. finger length - your group", 
                           tags$img(src = "app13.png",
                                    height = "150px"),
                           onclick = "window.open('/app13/', '_blank')")
             ),
             box(
               title="14", width=NULL,
               tags$button("", label = "14. Group velocity", 
                           tags$img(src = "app14.png",
                                    height = "150px"),
                           onclick = "window.open('/app14/', '_blank')")
             ),
             box(
               title="15", width=NULL,
               tags$button("", label = "15. . Sampling distribution: Conduction velocity", 
                           tags$img(src = "app15.png",
                                    height = "150px"),
                           onclick = "window.open('/app15/', '_blank')")
             )
             
      ))
  )
) 