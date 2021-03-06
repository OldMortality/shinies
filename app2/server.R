# app 2
# Plots of height
#   
# install.packages("devtools")
# library(devtools)
# install_github("trestletech/shinyTable")
# this one may not work in rstudio on Ubuntu

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shinyTable)




shinyserver <- function(input, output) { 
  
  rv <- reactiveValues(cachedTbl = NULL)
  
  output$tbl <- renderHtable({
    if (is.null(input$tbl)){
      
      #fill table 
      tbl <- data.frame(list('height'=c(1750,rep('',9)))) 
      rv$cachedTbl <- tbl
       
      return(tbl)
    } else{
      rv$cachedTbl <- input$tbl
      return(input$tbl)
    }
  })  
  
  
  xbar <- 1711
  sd <- 93
  x.breaks <- round(seq(xbar-3*sd,xbar+3*sd,sd))
  
  
  getSummary <- function() { 
      input$actionButtonID
      
      height <- isolate(rv$cachedTbl$height)
      
      # need as.character, or we get factor levels!
      ht <- as.numeric(as.character(height))
      n <-length(which(!is.na(ht)))
      
      mn <- round(mean(ht,na.rm=T),1)
       
      sd <- round(sqrt(var(ht,na.rm=T)),1)
       
      line0 <- paste("Sample size n =",n,sep=" ")
      line1 <- paste("Sample mean =",mn,sep=" ")
      line2 <- paste("Sample standard deviation =",sd,sep=" ")      
      result <- paste(line0,line1,line2,sep='<br>')
    return(result)
     
  }
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  
  output$histogram <- renderPlot({
    
    input$actionButtonID
    
    ht <- isolate(as.numeric(as.character(rv$cachedTbl[,1])))
    DF = data.frame(height=ht)
    p <- ggplot(DF, aes(height)) + 
      geom_histogram(binwidth=25,
                     fill="white",colour='black') +
       scale_x_continuous(limits=c(1250,2250),breaks=seq(1250,2250,100)) +
       scale_y_continuous(breaks = seq(0,10),minor_breaks=NULL) +
       ylab("Frequency") +
       xlab("Height (mm)")
     p 
    
  }) # end histogram
  
  output$boxplot <- renderPlot({
    input$actionButtonID
    
    ht <- isolate(as.numeric(as.character(rv$cachedTbl[,1])))
    DF = data.frame(height=ht)
    # limits sets the width of the boxplot
    p <- ggplot(DF, aes(y=height)) + 
      geom_boxplot(width=0.2) +
      scale_x_continuous(breaks = NULL,minor_breaks=NULL,
                         limits=c(-0.5,0.5)) +
      #scale_fill_manual(values=c("#ff0000")) +
      ylab("Height (mm)")
    p 
    
    
  }) # end boxplot
  
  output$dotplot <- renderPlot({
    
    input$actionButtonID
    col = '#696969'
    ht <- isolate(as.numeric(as.character(rv$cachedTbl[,1])))
    DF = data.frame(height=ht)
    hts <- DF$height
    if (length(hts[!is.na(hts)])==1) {
      # dotplot gives one huge dot if there is !1 datapoint
      p <- ggplot(DF, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot(binwidth=5,dotsize=0.03,color=col
                     ,fill=col) +
        ylab("Frequency") +
        xlab("Height (mm)")
    } else {
      p <- ggplot(DF, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot(color=col,fill=col) +
        ylab("Frequency") +
        xlab("Height (mm)")
    } 
    p 
  
    
  }) # end boxplot
  
   
}

