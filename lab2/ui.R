#
# Shiny dashboard with all my apps for lab2
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
  
  dashboardHeader(title = "HUBS191. Lab2: Statistics",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs()
                   ),
  dashboardBody(
    fluidRow(
      column(width = 6,height=2,
             box(
               title="Task A.1 and A.2 How tall are HUBS191 students?", 
               width=NULL,
               tags$button("", label = "Task A.1 and A.2 How tall are HUBS191 students?", 
                           tags$img(src = "app1.png",
                                    height = "150px"),
                            onclick ="window.open('/app1/', '_blank')")
               ),
             box(
               title="Task A.3 How tall are the people at your table?", width=NULL,
               tags$button("", label = "Task A.3 How tall are the people at your table?", 
                           tags$img(src = "app2.png",
                                    height = "150px"),
                           onclick = "window.open('/app2/', '_blank')")
               ),
            box(
               title="Task A.4 Heights for a lab stream (n=90)", width=NULL,
               tags$button("", label = "Task A.4 Heights for a lab stream (n=90)", 
                           tags$img(src = "app4.png",
                                    height = "150px"),
                           onclick = "window.open('/app4/', '_blank')")
                ),
              box(
                 title="Task A.5 Several sample means", width=NULL,
                 tags$button("", label = "Task A.5 Several sample means", 
                             tags$img(src = "app3.png",
                                      height = "150px"),
                             onclick = "window.open('/app3/', '_blank')")
               ), 
                
             box(
               title="Task A.6 How much do sample means vary?", width=NULL,
               tags$button("", label = "Task A.6 How much do sample means vary?", 
                           tags$img(src = "app5.png",
                                    height = "150px"),
                           onclick = "window.open('/app5/', '_blank')")
               ),
             
             
             box(
               title="Task A.7 Confidence intervals for the population mean height", width=NULL,
               tags$button("", label = "Task A.7 Confidence intervals for the population mean height", 
                           tags$img(src = "app6.png",
                                    height = "150px"),
                           onclick = "window.open('/app6/', '_blank')")
             ),
             box(
               title="Task B.1 Estimating a proportion", width=NULL,
               tags$button("", label = "B.1 Estimating a proportion", 
                           tags$img(src = "app7.png",
                                    height = "150px"),
                           onclick = "window.open('/app7/', '_blank')")
             ),
            box(
              title="Task B.3 Sampling variation in regression lines", width=NULL,
              tags$button("", label = "B.3 Sampling variation in regression lines", 
                          tags$img(src = "app12.png",
                                   height = "150px"),
                          onclick = "window.open('/app12/', '_blank')")
            ),
            box(
              title="Task C.1 What if the population mean is unknown?", width=NULL,
              tags$button("", label = "Task C.1 What if the population mean is unknown?", 
                          tags$img(src = "app6b.png",
                                   height = "150px"),
                          onclick = "window.open('/app6b/', '_blank')")
            ),
             box(
               title="Task C.2 Comparing the sampling distribution for two means", 
               width=NULL,
               tags$button("", label = "Task C.2 Comparing the sampling distribution for two means", 
                           tags$img(src = "app7b.png",
                                    height = "150px"),
                           onclick = "window.open('/app7b/', '_blank')")
             ),
            box(
              title="Task D.2 Joint laxity", width=NULL,
              tags$button("", label = "Task D.2 Joint laxity", 
                          tags$img(src = "app16.png",
                                   height = "150px"),
                          onclick = "window.open('/app16/', '_blank')")
            )
             
      ))
  )
) 