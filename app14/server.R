# app 14
# enter 10 velocities.show dot plot and summary
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
      tbl = data.frame(data.frame(velocity=c(50,75,rep("",8))))
      
      rv$cachedTbl <<- tbl
      return(tbl)
    } else{
      rv$cachedTbl <<- input$tbl
      return(input$tbl)
    }
  })  
  
  
  
   
  
  
  getSummary <- function() {
    
    input$actionButtonID
    
    group.tbl <- isolate(rv$cachedTbl)  
    velocity <- as.numeric(as.character(group.tbl$velocity))
    
    d.m <- round(mean(velocity,na.rm=T),1)
    d.v <- round(var(velocity,na.rm=T),1)
    d.sd <- round(sqrt(d.v),1)
    d.me <- round(median(velocity,na.rm=T),1)
    d.min <- round(range(velocity,na.rm=T)[1],1)
    d.max <- round(range(velocity,na.rm=T)[1],1)
    n <- length(which(!is.na(velocity)))
    
    l.ci <- ""
    if (n>1) {
      ci.low <- d.m + qt(0.025,df=n) * d.sd/sqrt(n)
      ci.upp <- d.m + qt(0.925,df=n) * d.sd/sqrt(n)
      ci.low <- round(ci.low,1)
      ci.upp <- round(ci.upp,1)
      l.ci <- paste("A 95% confidence interval for the mean velocity is",
                    "from",ci.low,"to",ci.upp)
     
    }
    
    l1 <- paste("The mean is:",d.m,sep=' ')
    l2 <- paste("The variance is:",d.v,sep=' ')
    l3 <- paste("The standard deviation is:",d.sd,sep=' ')
    l4 <- paste("The median is:",d.me,sep=' ')
    l5 <- paste("The lowest velocity is:",d.min,sep=' ')
    l6 <- paste("The highest velocity is:",d.max,sep=' ')
    l7 <- l.ci
    
    result <- paste(l1,l2,l3,l4,l5,l6,l7,sep="<br>")
    return(result)
     
  }
  
  output$summary <- renderText(
    getSummary()
  )
  
  
  
  output$dotplot <- renderPlot({
    
    input$actionButtonID
    
    group.tbl <- isolate(rv$cachedTbl) 
    
    velocity <- as.numeric(as.character(group.tbl$velocity))
    
    dropm <- which(is.na(velocity))
    if (length(dropm)>0) {
      velocity <- velocity[-dropm]
    }
    
    df <- data.frame(velocity=velocity)
    if (length(df$velocity) == 1) {
      dotsz = 0.05
    } else {
      dotsz = 0.15
    }
    p <- ggplot(df,aes(velocity)) + 
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      geom_dotplot(dotsize=dotsz,binwidth=10) 
    p
    
    
  }) # end dotplot
  
}
