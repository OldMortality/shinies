
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
               title="", width=NULL,
               actionButton("action", label = "4. Historical data",
                            onclick ="window.open('https://oldmortality.shinyapps.io/histheight/', '_blank')"),
             ), 
             box(
               title="", width=NULL,
               actionButton("action", label = "5. Sampling distribution",
                            onclick ="window.open('https://oldmortality.shinyapps.io/app5/', '_blank')")
             ),
             box(
               title="", width=NULL,
               actionButton("action", label = "6. Interval estimates",
                            onclick ="window.open('https://oldmortality.shinyapps.io/app6/', '_blank')")
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