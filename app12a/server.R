# app 12a.
# height vs finger length.Estimating a linear relationship
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shiny)
library(shinyTable)

shinyServer <- function(input, output) {
  
  rv <- reactiveValues(cachedTbl = NULL)
  coef <- reactiveValues(intercept=NULL,
                         slope=NULL)
  
  output$tbl <- renderHtable({
    
    if (is.null(input$tbl)){ 
      
      # make up historical data
      tbl <- data.frame(height = c(1750,rep('',9)),
                        digit.length=c(75,rep('',9)))
      #colnames(tbl) <- c(height (mm)','digit length (mm)')
      
      rv$cachedTbl <- tbl
      return(tbl)
    } else{
      rv$cachedTbl <- input$tbl
      return(input$tbl)
    }
  })  
  
  
  
  
  output$thePlot <- renderPlot({
     
    input$actionButtonID
    
    height <- isolate(as.numeric(as.character(rv$cachedTbl$height)))
    digit.length <- isolate(as.numeric(as.character(rv$cachedTbl$digit.length)))
    plot.data <- data.frame(x = height, y=digit.length)
    
    print(plot.data)
    p <- ggplot(data=plot.data,aes(x=x,y=y)) + geom_point() +
      scale_x_continuous(limits=c(1300,2100)) +
      scale_y_continuous(limits=c(50,150)) +
      ylab('digit length (mm)') +
      xlab('height (mm)') 
    
    m <- lm(digit.length~height,data=plot.data)
    coef$intercept <- coefficients(m)[[1]]
    coef$slope <- coefficients(m)[[2]]
    
    if (input$showline) {
      p <- p + 
        geom_abline(intercept=coefficients(m)[[1]],
                    slope=coefficients(m)[[2]],colour='red')  
    } 
    
    p
    
  }) # end thePlot
  
  
  getSummary <-function() {
    plus.or.minus <- "plus"
    intercept <- round(as.numeric(coef$intercept),2)
    slope <- round(as.numeric(coef$slope),2)
    print(paste(intercept,slope),sep= ' ')
    line1 <- ""
    if (!is.na(slope)) {
      if (slope < 0) {
        plus.or.minus <- "minus"
      }
      line1 <- paste("digit length =",
                   as.character(intercept),
                   plus.or.minus,
                   as.character(slope),
                   "* height",
                   sep=' '
                   )
    
      }
    return(line1)
  }
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
}
