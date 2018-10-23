# app 2
# Plots of height
#   

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)
 
ui <- dashboardPage( 
  dashboardHeader(title = "Task A.3 How tall are the people at your table?",
                  titleWidth = 800),
  dashboardSidebar(useShinyjs(), 
                   checkboxInput("showhist", "Histogram", FALSE),
                   checkboxInput("showbox", "Boxplot", FALSE),
                   checkboxInput("showdot", "Dotplot", FALSE) 
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
              box(title="Enter the height of your table members",
                  width=NULL,
                  rHandsontableOutput("hot", height = 300),
                  htmlOutput('summary', height = 100)
              ),
              box(
               title="Histogram of height", width=NULL,
               conditionalPanel(
                 condition = "input.showhist",
                 plotOutput("histogram", height = 300)
                )
              )
             ),
      column(width=6,
              box(
                title="Boxplot of height", width=NULL,
                conditionalPanel(
                  condition = "input.showbox",
                  plotOutput("boxplot", height = 373)
                )
              ),
              box(
                title="Dotplot of height", width=NULL,
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
  x.breaks <- round(seq(xbar-3*sd,xbar+3*sd,sd))
  
  
  getSummary <- function() {
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot) 
      n <-sum(!is.na(DF$height))
      mn <- round(mean(DF$height,na.rm=T),1)
      sd <- round(sqrt(var(DF$height,na.rm=T)),1)
      line0 <- paste("Sample size n =",n,sep=" ")
      line1 <- paste("Sample mean =",mn,sep=" ")
      line2 <- paste("Sample standard deviation =",sd,sep=" ")      
      result <- paste(line0,line1,line2,sep='<br>')
    return(result)
    }
  }
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  
  output$histogram <- renderPlot({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      p <- ggplot(DF, aes(height)) + 
       geom_histogram(binwidth=25,fill="white",colour='black') +
       scale_x_continuous(limits=c(1500,2100),breaks=seq(1500,2100,100)) +
       scale_y_continuous(breaks = seq(0,10),minor_breaks=NULL) +
       ylab("Frequency") +
       xlab("Height (mm)")
     p 
    }
  }) # end histogram
  
  output$boxplot <- renderPlot({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot) 
    # limits sets the width of the boxplot
    p <- ggplot(DF, aes(y=height)) + 
      geom_boxplot(width=0.2) +
      scale_x_continuous(breaks = NULL,minor_breaks=NULL,
                         limits=c(-0.5,0.5)
                         )  +
      ylab("Height (mm)")
    p 
    }
    
  }) # end boxplot
  
  output$dotplot <- renderPlot({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      #p <- ggplot(DF, aes(height)) + 
      #  scale_y_continuous(breaks = NULL,minor_breaks=NULL)  +
      #  ylab("Frequency")
      
    hts <- DF$height
    if (length(hts[!is.na(hts)])==1) {
      # dotplot gives one huge dot if there is !1 datapoint
      p <- ggplot(DF, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot(binwidth=5,dotsize=0.03) +
        ylab("Frequency") +
        xlab("Height (mm)")
    } else {
      p <- ggplot(DF, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot() +
        ylab("Frequency") +
        xlab("Height (mm)")
    } 
    p 
  }
    
  }) # end boxplot
  
  
  
   
}

shinyApp(ui, server)