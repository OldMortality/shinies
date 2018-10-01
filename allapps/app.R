
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
                            onclick ="window.open('https://oldmortality.shinyapps.io/app1/', '_blank')")
               ),
             box(
               title="2 ", width=NULL,
               tags$button("", label = "2. Plot heights", 
                           tags$img(src = "app2.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app2/', '_blank')")
               ),
               box(
                 title="3 ", width=NULL,
                 tags$button("", label = "3. Group heights", 
                             tags$img(src = "app3.png",
                                      height = "150px"),
                             onclick = "window.open('https://oldmortality.shinyapps.io/app3/', '_blank')")
               ), 
               box(
               title="4", width=NULL,
               tags$button("", label = "4. Historical data", 
                           tags$img(src = "app4.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app4/', '_blank')")
               ), 
             box(
               title="5", width=NULL,
               tags$button("", label = "5. Sampling distribution", 
                           tags$img(src = "app5.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app5/', '_blank')")
               ),
             
             box(
               title="6b", width=NULL,
               tags$button("", label = "6b. Confidence intervals", 
                           tags$img(src = "app6b.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app6b/', '_blank')")
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
                           onclick = "window.open('https://oldmortality.shinyapps.io/app7/', '_blank')")
             ),
             box(
               title="7b", width=NULL,
               tags$button("", label = "7b. Exercise", 
                           tags$img(src = "app7b.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app7b/', '_blank')")
             ),
             box(
               title="8", width=NULL,
               tags$button("", label = "8. Comparison", 
                           tags$img(src = "app8.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app8/', '_blank')")
             ),
             box(
               title="9", width=NULL,
               tags$button("", label = "9. Plots of lab data (heights)", 
                           tags$img(src = "app9.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app9/', '_blank')")
             ),
             box(
               title="10", width=NULL,
               tags$button("", label = "10. Guillain-Barré vs. normal", 
                           tags$img(src = "app10.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app10/', '_blank')")
             ),
             box(
               title="11", width=NULL,
               tags$button("", label = "11. Comparing nerve conduction velocity", 
                           tags$img(src = "app11.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app11/', '_blank')")
             ),
             box(
               title="12", width=NULL,
               tags$button("", label = "12. Height vs. finger length", 
                           tags$img(src = "app12.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app12/', '_blank')")
             ),
             box(
               title="13", width=NULL,
               tags$button("", label = "13. Height vs. finger length - your group", 
                           tags$img(src = "app13.png",
                                    height = "150px"),
                           onclick = "window.open('https://oldmortality.shinyapps.io/app13/', '_blank')")
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