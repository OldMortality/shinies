# app 6c.
# show CI
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

shinyServer <- function(input, output) {
  
  output$sampleCounter <- renderInfoBox({
    infoBox(
      "Samples: ", paste0(counter$counterValues), icon = icon("list"),
      color = "purple"
    )
  })
  
  
  
  output$plot1 <- renderPlot({ 
    
    mu <- as.numeric(input$mu.2)
    sd <- 93 
    low <- mu-3*sd
    upp <- mu+3*sd
    x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
    xbar <- as.numeric(input$samplemean)
    
    p <- ggplot(data = data.frame(x = c(low , upp)), aes(x)
    ) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = mu, sd = sd)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      xlab("Height") #+
      #geom_vline(xintercept = mu,col='red')
    
    p <- p + geom_segment(x=mu,xend=mu,
                          y=-0.1,yend=0.00449,
                          colour='red') +
      annotate("text", label = "mean", 
               x = mu-10, 
               y= 0.00475, hjust=0,
               size = 5, colour = "red")
    
    if (!is.na(xbar)) {
      # points for the sample
      pts <- data.frame(x = as.numeric(input$samplemean),y=0) 
      p <- p + geom_point(data=pts,aes(y=y),
                          colour='blue',
                          shape=24,
                          size=5,
                          fill='blue')
    }
    p
  }) # end plot1
  
  #
  # This is the strip, with 1 dot and interval for this sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    
    mu <- as.numeric(input$mu.2)
    sd <- 93 
    low <- mu-3*sd
    upp <- mu+3*sd
    x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
    
    
    if (input$samplemean>0) {
      
      xbar <- as.numeric(input$samplemean)
      s <- as.numeric(input$samplesd)
      n <- as.numeric(input$samplesize)
      lo <- xbar + qt(0.025,n-1) * s/sqrt(n)
      up <- xbar + qt(0.975,n-1) * s/sqrt(n)
      df <- data.frame(x=xbar)
      p <- ggplot(df, aes(x = x,y=0 )) +
        geom_point(colour='black') +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Height (mm)") +
        geom_segment(aes(x=lo,y=0,xend=up,yend=0))
      p
    }
    
  })
  
  # sampling distribution, if the blue distribution
  #    were true.
  output$samplingdistribution <- renderPlot({ 
    
      sd=93
      
      # sd of the sampling distribution
      sd.samp <- sd/sqrt(as.numeric(input$samplesize))
      mu <- as.numeric(input$mu.2)
      
      low <- mu-3*sd
      upp <- mu+3*sd
      x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
      thisOne <- as.numeric(input$samplemean)
      # for plotting shady bit
     
        x.low <- input$mu.2-3*sd
        x.upp <- thisOne
      
      
      # for plotting shaded area, lower bit
      df.norm.x <- data.frame(x=seq(x.low,x.upp,0.1))
      df.norm.y <- data.frame(y=dnorm(df.norm.x$x,mean=input$mu.2,sd=sd.samp))
      df.norm <- cbind(df.norm.x,df.norm.y)
      # for plotting shaded area, upper bit
      x.low <- thisOne
      x.upp <- input$mu.2+3*sd
      
      df.norm.upp.x <- data.frame(x=seq(x.low,x.upp,0.1))
      df.norm.upp.y <- data.frame(y=dnorm(df.norm.upp.x$x,mean=input$mu.2,sd=sd.samp))
      df.norm.upp <-   cbind(df.norm.upp.x,df.norm.upp.y)
      
      p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
      ) + 
        geom_point(aes(x=thisOne,y=0)) + 
        stat_function(fun = dnorm, show.legend=F,
                    colour='blue', 
                    args = list(mean = input$mu.2, sd = sd.samp) )+ 
        ylab("") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        theme(legend.position = "none") +
        xlab("Height") +
         geom_ribbon(data=df.norm,aes(x=df.norm$x,ymin=0,ymax=df.norm$y),
                  fill='red',alpha=0.1)  +
        geom_ribbon(data=df.norm.upp,aes(x=df.norm.upp$x,ymin=0,ymax=df.norm.upp$y),
                    fill='blue',alpha=0.1)  
     p
    
  })
  
  getSampleSummary <- function() {
    return("")
  }
  
  getSampleSummary.old <- function() {
    
    points <- data.frame(x = as.numeric(input$samplemean) )
    sum <- 0
    if (dim( points )[1] > 0 ) {
      sum <- sum(points$x)
    }
    count <- counter$countervalue
    str0 <- paste('This is sample number: ',count,sep=' ')
    str0b <- "The black dots indicate the sample."
    str1 <- paste('The total of the sample is ',sum,sep=': ')
    
    xbar <- round(1000* sum / as.numeric(input$n))/1000
    str2 <- paste('The average of the sample is ',sum,
                  'divided by ',as.numeric(input$n),'= ',xbar,sep=' ')
    result <- paste(str0,'<br>',str0b,'<br>',str1,'<br>',str2)
    
    return(result)
  }
  
  getSampleMeansSummary <- function() {
     
    line1a <- "The red shaded area represents the area under the curve"
    line1b <- "below your sample mean"
    line1 <- paste(line1a,line1b,sep=' ')
    result <- paste("<font size=4>",line1,"</font>",sep="<br>")
    return(result)
   
      #sampleMean <- as.numeric(input$samplemean)
      #sd.samp.dist <- sd/sqrt(as.numeric(input$n))
      #prob <- pnorm(sampleMean,mean=input$mu.2,sd=sd.samp.dist)
      #if (input$mu.2 < mu) {
      #  prob <- 1-  prob
      #}
      #prob <- round(prob,3)
      #str0 <- paste("Suppose the true value of the population mean is",
      #              input$mu.2,sep=" ")
      #str1 <- paste("The observed sample mean is",sampleMean,sep=' ')
      #str2 <- paste("The probability of seeing the observed sample mean is",prob)
      #result <- paste(str0,str1,str2,sep="<br>")
    
     
    
    
  }
  
  getOneSampleSummary <- function() {
    
    xbar <- as.numeric(input$samplemean)
    s <- as.numeric(input$samplesd)
    n <- as.numeric(input$samplesize)
    lo <- round(xbar + qt(0.025,n-1) * s/sqrt(n),1)
    up <- round(xbar + qt(0.975,n-1) * s/sqrt(n),1)
      
    line1 <- paste("Your sample mean is:",input$samplemean,sep=' ')
    line2 <- paste("The 95% confidence interval for the population mean is",
                   lo,'to',up,sep=' ')
    lines <- paste(line1,line2,sep="<br>")
    result <- paste("<font size=4>",lines,"</font>",sep="")
    return(result)
  }
  
  output$sampleSummary <- renderText(
    getSampleSummary()
  )
  
  output$sampleMeanSummary <- renderText(
    getSampleMeansSummary()
  )
  
  output$onesamplesummary <- renderText(
    
    getOneSampleSummary()
  )
  
}
