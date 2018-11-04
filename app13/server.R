# app 13.
# height vs finger length - your group
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2) 
library(shinyTable)

shinyServer <- function(input, output) {
  
  df <- data.frame(x=rnorm(100))
  
  rv <- reactiveValues(cachedTbl = NULL)  
  
   
  datasetInput <- reactive({
    result <- data.frame(finger.length=c(70,rep(NA,9)),
                         height=c(1750,rep(NA,9)))
    result
  })
  
  
   
   
  
  output$tbl <- renderHtable({ 
    
    if (is.null(input$tbl)){  
      tbl = data.frame(finger.length=c(70,rep("",9)),
                       height=c(1750,rep("",9)))
      
      rv$cachedTbl <<- tbl
      return(tbl)
    } else{
      rv$cachedTbl <<- input$tbl
      return(input$tbl)
    }
  })  
  
  
  
  
  # the sample
  samp <- reactiveVal()
  
  f <- read.csv('combined_clean.csv',header=T)
  
  f <- f[-which(f$heightmm < 1000),]
  f <- f[-which(f$heightmm > 2500),]
  f <- f[-which(is.na(f$heightmm)),]
  f <- f[-which(f$indexfingerlengthmm<20),]
  
  f$hei <- f$heightmm
  f$fin <- f$indexfingerlengthmm
  
  m <- lm(hei~fin,data=f)
  a0 <- coefficients(m)[[1]]
  a1 <- coefficients(m)[[2]]
  
  alla0 <- reactiveValues(a0s=vector())
  alla1 <- reactiveValues(a1s=vector())
  
  
  
  
  observeEvent(input$clear,{
    
    a0 <- coefficients(m)[[1]]
    a1 <- coefficients(m)[[2]]
    alla0$a0s <- vector()
    alla1$a1s <- vector()
    # clear samp
    samp <- NULL
    samp(samp)
    
    
    
    
  })
  
 
  
  
  
  # 1 sample
  observeEvent(input$sample,{
    
    
    s <- f[sample(nrow(f),size=10),]
    
    
    m2 <- lm(hei~fin,data=s)
    alla0$a0s <- c(alla0$a0s,coefficients(m2)[[1]])
    alla1$a1s <- c(alla1$a1s,coefficients(m2)[[2]])
    
    # store sample to show in plot
    samp <- s
    # no idea what this does
    samp(samp)
    
  })  
  
  
  output$thePlot <- renderPlot({
    
    plot.data <- data.frame(x = f$fin, y=f$hei)
    #alla0$a0s <- c(alla0$a0s) <- coefficients(m2)[1]
    p <- ggplot(data=plot.data,aes(x=x,y=y)) + geom_point() +
      scale_x_continuous(limits=c(50,100)) +
      scale_y_continuous(limits=c(1300,2100)) +
      ylab('height (mm)') +
      xlab('finger length (mm)') 
    
    if (!is.null(samp()) ) {
      p <- p +
        geom_point(data=samp(),aes(x=fin,y=hei),colour='red')
    }
    col <- 'red'
    
    # trigger all this when the button is pressed
    input$actionButtonID 
    
    group.tbl <- isolate(rv$cachedTbl) 
     
    # remove rows with NA
    dropm <- which(is.na(as.numeric(as.character(group.tbl$finger.length))) |
                             is.na(as.numeric(as.character(group.tbl$height))))
     
    if (length(dropm)>0) {
      group.tbl <- group.tbl[-dropm,]
    }
    
    
    len <- as.numeric(as.character(group.tbl$finger.length))
    hei <- as.numeric(group.tbl$height)
     
    # show the hot table data
    if (dim(group.tbl)[1] > 0) {
      p <- p + geom_point(data=group.tbl,aes(x=len,
                                           y=hei),colour='blue',
                          shape = 4,
                          size = 5)
    }
    if (dim(group.tbl)[1] >= 2) {
      # plot regression line
      m <- lm(hei~len)
      a <- coefficients(m)[1]
      b <- coefficients(m)[2]
      p <- p + geom_abline(intercept=a,slope=b,colour='blue')
    
    }
    
    if (input$showalllines) {
      #print(length(alla1$a1s))
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
