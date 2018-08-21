
# Shiny dashboard with all my apps
library(shinydashboard)
library(shinyjs)
library(ggplot2)

library(shinyBS)


ui <- dashboardPage(
  
  dashboardHeader(title = "All apps- under construction",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs()
                   ),
  dashboardBody(
    fluidRow(
      column(width = 6,height=2,
             box(
               title="1 ", width=NULL,
               tags$button("", label = "1. Mean height", 
                           tags$img(src = "app1.png",
                                    height = "150px"),
                            onclick ="window.open('https://oldmortality.shinyapps.io/meanheight1/', '_blank')")
               ),
             box(
               title="2 ", width=NULL,
               tags$button("", label = "2. Plot heights", 
                           tags$img(src = "app2.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/heightplots/', '_blank')")
               ),
               box(
                 title="3 ", width=NULL,
                 tags$button("", label = "3. Group heights", 
                             tags$img(src = "app3.png",
                                      height = "150px"),
                             onclick = "window.open('https://oldmortality.shinyapps.io/groupheight2/', '_blank')")
               ), 
               box(
               title="4", width=NULL,
               tags$button("", label = "4. Historical data", 
                           tags$img(src = "app4.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/histheight/', '_blank')")
               ), 
             box(
               title="5", width=NULL,
               tags$button("", label = "5. Sampling distribution", 
                           tags$img(src = "app5.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app5/', '_blank')")
               ),
             box(
               title="6", width=NULL,
               tags$button("", label = "6. Interval estimates", 
                           tags$img(src = "app6.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app6/', '_blank')")
             ),
             box(
               title="7", width=NULL,
               tags$button("", label = "7. Exercise", 
                           tags$img(src = "app7.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/binary7/', '_blank')")
             ),
             box(
               title="8", width=NULL,
               tags$button("", label = "8. Comparison", 
                           tags$img(src = "app8.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app8/', '_blank')")
             )
             
      ))
  )
)
  
  
  




server <- function(input, output) {  
  output$summary <- renderText(
    getSummary()
  )
  
}

shinyApp(ui, server)