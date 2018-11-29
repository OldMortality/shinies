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
               title="App 1 - How tall are HUBS191 students?", 
               width=NULL,
               tags$button("",
                           label = "How tall are HUBS191 students?", 
                           tags$img(src = "app1.png",
                                    height = "150px"),
                            onclick ="window.open('/app1/', '_blank')")
               ),
             box(
               title="App 2 - How tall are the people at your table?", width=NULL,
               tags$button("", label = "How tall are the people at your table?", 
                           tags$img(src = "app2.png",
                                    height = "150px"),
                           onclick = "window.open('/app2/', '_blank')")
               ),
            box(
               title="App 3 - How tall are the people in a HUBS191 lab stream?", width=NULL,
               tags$button("", label = "Heights for a lab stream", 
                           tags$img(src = "app4.png",
                                    height = "150px"),
                           onclick = "window.open('/app4/', '_blank')")
                ),
              box(
                 title="App 4 - Several sample means", width=NULL,
                 tags$button("", label = "Several sample means", 
                             tags$img(src = "app3.png",
                                      height = "150px"),
                             onclick = "window.open('/app3/', '_blank')")
               ), 
                
             box(
               title="App 5 - The distribution of sample means", width=NULL,
               tags$button("", label = "The distribution of sample means", 
                           tags$img(src = "app5.png",
                                    height = "150px"),
                           onclick = "window.open('/app5/', '_blank')")
               ),
             
             
             box(
               title="App 6 - Confidence intervals for the population mean height", width=NULL,
               tags$button("", label = "Confidence intervals for the population mean height", 
                           tags$img(src = "app6.png",
                                    height = "150px"),
                           onclick = "window.open('/app6/', '_blank')")
             ),
             box(
               title="App 7 - Estimating a proportion", width=NULL,
               tags$button("", label = "Estimating a proportion", 
                           tags$img(src = "app7.png",
                                    height = "150px"),
                           onclick = "window.open('/app7/', '_blank')")
             ),
            box(
              title="App 8 - Sampling variation in regression lines", width=NULL,
              tags$button("", label = "Sampling variation in regression lines", 
                          tags$img(src = "app12.png",
                                   height = "150px"),
                          onclick = "window.open('/app12/', '_blank')")
            ),
            box(
              title="App 9 - What can we say based on a single sample mean?", width=NULL,
              tags$button("", label = "What can we say based on a single sample mean?", 
                          tags$img(src = "app6c.png",
                                   height = "150px"),
                          onclick = "window.open('/app6c/', '_blank')")
            ),
            
            
             box(
               title="App 10 -  Comparing the sampling distribution for two means", 
               width=NULL,
               tags$button("", label = "Comparing the sampling distribution for two means", 
                           tags$img(src = "app7b.png",
                                    height = "150px"),
                           onclick = "window.open('/app7b/', '_blank')")
             ),
            box(
              title="App 11 -  Comparing the sampling distribution for two means", 
              width=NULL,
              tags$button("", 
                label = "Sampling distribution for a difference in sample means", 
                          tags$img(src = "app8.png",
                                   height = "150px"),
                          onclick = "window.open('/app8/', '_blank')")
            ),
            box(
              title="App 12 - Joint laxity", width=NULL,
              tags$button("", label = "Joint laxity", 
                          tags$img(src = "app16.png",
                                   height = "150px"),
                          onclick = "window.open('/app16/', '_blank')")
            )
             
      ))
  )
) 