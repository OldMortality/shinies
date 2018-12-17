# app 12.
# height vs finger length
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(shiny)



shinyServer <- function(input, output) {
  
  # the sample
  samp <- reactiveVal()
  
  f <- read.csv('combined_clean.csv',header=T)
  
  f <- f[-which(f$heightmm < 1000),]
  f <- f[-which(f$heightmm > 2500),]
  f <- f[-which(is.na(f$heightmm)),]
  f <- f[-which(f$indexfingerlengthmm<20),]
  
  f$hei <- f$heightmm
  f$fin <- f$indexfingerlengthmm
  
  m <- lm(fin~hei,data=f)
  a0 <- coefficients(m)[[1]]
  a1 <- coefficients(m)[[2]]
  
  alla0 <- reactiveValues(a0s=vector())
  alla1 <- reactiveValues(a1s=vector())
  
  
   
  
  observeEvent(input$clear,{
    
    #a0 <- coefficients(m)[[1]]
    #a1 <- coefficients(m)[[2]]
    alla0$a0s <- vector()
    alla1$a1s <- vector()
    # clear samp
    samp <- NULL
    samp(samp)
    
    
  })
  
 
  
  # 1 sample
  observeEvent(input$sample,{ 
    s <- f[sample(nrow(f),size=input$n),]  
    m2 <- lm(fin~hei,data=s)
    alla0$a0s <- c(alla0$a0s,coefficients(m2)[[1]])
    alla1$a1s <- c(alla1$a1s,coefficients(m2)[[2]])
    
    # store sample to show in plot
    samp <- s
    # no idea what this does
    samp(samp)
    
  })  
  
  
  output$thePlot <- renderPlot({
     
    plot.data <- data.frame(x = f$hei, y=f$fin)
    
    p <- ggplot(data=plot.data,aes(x=x,y=y)) + 
      scale_x_continuous(limits=c(1300,2100)) +
      scale_y_continuous(limits=c(50,100)) +
      ylab('finger length (mm)') +
      xlab('height (mm)') 
    
    if (input$showall) {
      p <- p+ geom_point()
      p <- p + geom_abline(intercept=a0,
                           slope=a1,
                           colour='black')
      p <- p + geom_vline(linetype=5,
                          xintercept=mean(f$hei,na.rm=T),
                          colour='black')
      p <- p + geom_hline(linetype=5,
                          yintercept=mean(f$fin,na.rm=T),
                          colour='black')
      
      
    } 
    
    if (!is.null(samp()) ) {
      p <- p +
       geom_point(data=samp(),aes(x=hei,y=fin),colour='red')
    }
    col <- 'red'
     
    if (input$showalllines) {
      
      for (i in 1:length(alla1$a1s)) {
        p <- p + geom_abline(intercept=alla0$a0s[i],slope=alla1$a1s[i],colour=col)
      
      }
    } else {
      i <- length(alla1$a1s)
      p <- p + geom_abline(intercept=alla0$a0s[i],slope=alla1$a1s[i],colour=col)
    }
    
    p
    
  }) # end thePlot
  
  
  
}
