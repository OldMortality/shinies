# app 2
# Plots of height
#   

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)



ui <- dashboardPage(
  
  dashboardHeader(title = "Plots of group height",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   
                   checkboxInput("showhist", "Histogram", TRUE),
                   checkboxInput("showbox", "Boxplot", TRUE),
                   checkboxInput("showdot", "Dotplot", TRUE)                   
                   
                   
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
              box(title="Enter the height of your group members",width=NULL,
                  rHandsontableOutput("hot", height = 300)
              ),
            
              box(
               title="histogram", width=NULL,
               conditionalPanel(
                 condition = "input.showhist",
                 plotOutput("histogram", height = 300)
                )
              )
             ),
      column(width=6,
              box(
                title="boxplot", width=NULL,
                conditionalPanel(
                  condition = "input.showbox",
                  plotOutput("boxplot", height = 300)
                )
              ),
              box(
                title="dotplot", width=NULL,
                conditionalPanel(
                  condition = "input.showdot",
                  plotOutput("dotplot", height = 300)
                )
              ) 
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
  #upp <- xbar + 3 * sd
  #low <- xbar - 3 * sd
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
  
  output$boxplot <- renderPlot({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
     
    
    # limits sets the width of the boxplot
    p <- ggplot(DF, aes(y=height)) + geom_boxplot(width=0.2) +
      scale_x_continuous(breaks = NULL,minor_breaks=NULL,
                         limits=c(-0.5,0.5)
                         )  
      
      
    p 
    }
    
  }) # end boxplot
  
  output$dotplot <- renderPlot({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      p <- ggplot(DF, aes(height)) + 
      scale_y_continuous(breaks = NULL,minor_breaks=NULL)  
    
      hts <- DF$height
    if (length(hts[!is.na(hts)])==1) {
      # dotplot gives one huge dot if there is !1 
      # datapoint
      p <- ggplot(DF, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot(binwidth=5,dotsize=0.03)
    } else {
      p <- ggplot(DF, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
       geom_dotplot()
    }
    
    p 
    }
    
  }) # end boxplot
  
  
  
   
}

shinyApp(ui, server)