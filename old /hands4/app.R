# app 2
# Plots of height
#   

library(shinydashboard)

library(ggplot2)
library(DT)
library(rhandsontable)



ui <- dashboardPage(
  
  dashboardHeader(title = "Plots of group height",
                  titleWidth = 450),
  dashboardSidebar(
                    
                   
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
             
                 rHandsontableOutput("hot", height = 300)
             ),
             
             box(
               
               
                 plotOutput("histogram", height = 300)
               
             )
      )
    )
    
  )






server <- function(input, output) {
  
  
  
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) {
      DF = data.frame(height=c(1750,rep(NA,9)))
    } else {
      DF = hot_to_r(input$hot)
    }
    rhandsontable(DF)  
  })
  
  xbar <- 1711
  sd <- 93
  
  x.breaks <- round(seq(xbar-3*sd,xbar+3*sd,sd))
  
  
  output$histogram <- renderPlot({
    
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      p <- ggplot(DF, aes(height)) + 
        geom_histogram(bins=5) +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL)
      
      p 
      
    }
  }) # end histogram
  
  
    
   
  
  
    
   
  
  
  
  
}

shinyApp(ui, server)