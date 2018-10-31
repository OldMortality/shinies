# app 4
# height plots and table, 90 people with 
#   2 errors and 1 true outlier
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shinyTable) 


shinyServer <- function(input, output) {
  
  rv <- reactiveValues(cachedTbl = NULL)
  
  output$tbl <- renderHtable({
    
    if (is.null(input$tbl)){ 
      
      # make up historical data
      tbl <- data.frame(height=c(round(rnorm(90,1710,90))))
      # 2 people who put in cm instead of mm
      tbl$height[22] <- 170
      tbl$height[77] <- 3500
      # a hobbit
      tbl$height[55] <- 1300
      rv$cachedTbl <<- tbl
      return(tbl)
    } else{
      rv$cachedTbl <<- input$tbl
      return(input$tbl)
    }
  })  
  
  observeEvent(input$showhist, {
    
    input$actionButtonID
  })
  
  
  getSummary <- function() {
    
    input$actionButtonID
    
    height <- isolate(as.numeric(as.character(rv$cachedTbl$height)))
    
    n <-length(which(!is.na(height)))
    mn <- round(mean(height,na.rm=T),1)
    sd <- round(sqrt(var(height,na.rm=T)),1)
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
    
    ht <- isolate(as.numeric(rv$cachedTbl[,1]))
    data = data.frame(height=ht)
   
    p <- ggplot(data, aes(x=height)) + 
      geom_histogram(binwidth=25,fill="white",colour='black') +
      scale_x_continuous(limits=c(1500,2100),breaks=seq(1500,2100,100)) +
      scale_y_continuous(breaks = seq(0,20),minor_breaks=NULL) +
      ylab("Frequency") +
      xlab("Height (mm)") 
    p
     
    
  }) # end histogram
  
  output$boxplot <- renderPlot({
    
    input$actionButtonID
    
    ht <- isolate(as.numeric(rv$cachedTbl[,1]))
    data = data.frame(height=ht)
    
    p <- ggplot(data, aes(y=height)) + 
      geom_boxplot(width=0.2) +
      scale_x_continuous(breaks = NULL,minor_breaks=NULL,
                         limits=c(-0.5,0.5)
      )  +
      ylab("Height (mm)")
    
    p
  }) # end boxplot
  
  output$dotplot <- renderPlot({
    
    input$actionButtonID
    
    ht <- isolate(as.numeric(rv$cachedTbl[,1]))
    d = data.frame(height=ht)
    
    p <- ggplot(d, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot() +
        ylab("Frequency") +
        xlab("Height (mm)")  
    p  
  }) # end dotplot 
  
}
