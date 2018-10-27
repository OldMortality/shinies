# app 2
# Plots of height
#   
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable) 

shinyserver <- function(input, output) { 
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

