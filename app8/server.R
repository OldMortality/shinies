# app 8.
# Comparing 2 groups: difference in means
#
# Change log
#    07/12/2019  removed ggplot
#
#
#
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)

shinyServer <- function(input, output) {
  
  # from combined clean csv
  mu1 = 1712
  mu2 = 1671
  sd1 = 92
  sd2 = 92
  xbreaks <- seq(mu1-3*sd1,mu1+3*sd1,by=sd1)
  upp <- mu1 + 3 * sd1
  low <- mu1 - 3 * sd1
  x.breaks <- round(seq(mu1-3*sd1,mu1+3*sd1,sd1))
  
  
    
  
  thisSampleMean <- 0
  shinyjs::disable("shownormal")
  
  # how many sample means do we show on the stripchart?
  showMean <- reactiveVal(1)
  # do we show the sample dots in the top chart?
  showSample <- reactiveVal(FALSE)
 
  samp1 <- reactiveVal()
  samp2 <- reactiveVal()
  meansamp <- reactiveVal()
  values <- reactiveValues(total = 0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  showDiff <- reactiveValues(summary = 0)
  
  
  animate.counter <- 0
  max.animate <- 25
  
  output$start <- renderUI({
    actionButton("click", 
                 label = label(),
                 style=style()
                 #icon=icon("running",lib = "font-awesome")
    )
  })
  
  style <- reactive({
    if (autorun$auto == 1) {
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    } else {
      # default
      style="color: #444444; background-color: #F4F4F4; border-color: #444444"
    }
    
  })
  
  label <- reactive({
    if (autorun$auto == 1) {
      label <- "Stop"
    } else {
      label <- "Start"
    }
  })
  
  observeEvent(input$click, {
    if (autorun$auto == 1) {
      autorun$auto <- 0  
    } else {
      autorun$auto <- 1
    }
    
  })
  
  
  
  observeEvent(input$clear,{
    thisSampleMean <- 0
    thisSampleMean1 <- 0
    s.1 <- numeric(0)
    s.2 <- numeric(0)
    values$total1 = 0
    values$total2 = 0
    values$diff = 0
    counter$countervalue = 0
    autorun$auto = 0
    samp1(s.1)
    samp2(s.2)
  })
  
  observeEvent(input$n, {
    click("clear")
    invalidateLater(1)
  })
  
  
  doSamples <- function(n.samples) {
    if (n.samples == 1) {
      showDiff$summary <- 1
      showSample(TRUE)
    } else {
      showDiff$summary <- 0
      showSample(FALSE)
    }
    showMean(n.samples)
    sample.size <- as.numeric(isolate(input$n))
    # instead of taking n.samples samples of, say 30, we are taking one sample of n * 30
    # and then split that up to get the means
    s1 <- rnorm(sample.size*n.samples,mean=mu1,sd=sd1)
    s2 <- rnorm(sample.size*n.samples,mean=mu2,sd=sd2)
    if (n.samples==1) {
      # same result as in the else branch, but suppresses a warning.
      means.1 <- mean(s1)
      means.2 <- mean(s2)
    } else {
      means.1 <- unlist(lapply(split(s1, sort(rep_len(1:n.samples, length(s1)))),   mean))
      means.2 <- unlist(lapply(split(s2, sort(rep_len(1:n.samples, length(s2)))),   mean))
      
    }
    values$total1 <- c(values$total1,means.1)
    values$total2 <- c(values$total2,means.2)
    values$diff <- c(values$diff,means.1-means.2)  
    
    samp1(round(s1[1:sample.size],2))
    samp2(round(s2[1:sample.size],2))
  }
  
  # 1 sample
  observeEvent(input$sample,{
    # showMean(1)
    # showDiff$summary <- 1
    # showSample(TRUE)
    doSamples(1)
  })
  
  # 10 samples
  observeEvent(input$sample10,{
    doSamples(10)    
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    # showDiff$summary <- 0 
    # showMean(100)
    # showSample(FALSE)
    doSamples(100)
  })
  
  
  observeEvent(input$sample, {
    counter$countervalue <- counter$countervalue + 1
    if (counter$countervalue < 30) {
      shinyjs::disable("shownormal")
    } else {
      shinyjs::enable("shownormal")
    }
  })
  
  observeEvent(input$sample10, {
    
    counter$countervalue <- counter$countervalue + 10
    if (counter$countervalue < 30) {
      shinyjs::disable("shownormal")
    } else {
      shinyjs::enable("shownormal")
    }
  })
  observeEvent(input$sample100, {
    counter$countervalue <- counter$countervalue + 100
    shinyjs::enable("shownormal")
  })
  
   
  
  
  # animate: click sample repeatedly
  observe({
    if (autorun$auto == 1) {
      
      # run this function again in 2000ms
      invalidateLater(2000)
      click("sample")
      animate.counter <<- animate.counter + 1
      if (animate.counter > max.animate) {
        # stop animation.
        autorun$auto <- 0
        animate.counter <<- 0
      } 
      
    }
  })
   
   
  
  output$plot1 <- renderPlot({
    
    
    # background color, margins and plot outside area (for legend)
    par(bg="#EBEBEB",mar= c(5.5, 1.1, 4.1, 0.5),xpd=T)
    # use base R for plotting is much faster
    plot('',xlim=c(low,upp),ylim=c(0,0.004),
         ylab="",xlab="",xaxt="n",yaxt="n",bty="n") 
    abline(v=xbreaks,col='white')
    axis(1,  at =  seq(mu1-3*sd1,mu1+3*sd1,by=sd1))
    curve(dnorm(x,mean=mu1,sd=sd1),low,upp,col='red',add=T)
    curve(dnorm(x,mean=mu2,sd=sd2),low,upp,col='blue',add=T)
    
    if (showSample() & length(samp1())>0 ) {
      s1 <- samp1()
      s2 <- samp2()
      n <- length(s1)
      cols <- c(rep('red',n),rep('blue',n))
      points(c(s1,s2),rep(0,(2*n)),pch=21,col=cols,bg=cols)
    }
    
  }) # end plot1
  
  
  
  #
  # This is the strip, with 1 dot for each sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    if (showSample()==TRUE){ 
      
      if (length(samp1())>0) {
        thisOne1 <- tail(values$total1,showMean())
        thisOne2 <- tail(values$total2,showMean())
        pts <- c(thisOne1,thisOne2)
        cols <- c('red','blue')
        cols <- c(rep('red',length(thisOne1)),rep('blue',length(thisOne2)))
        x.breaks <- round(seq(mu1-3*sd1,mu1+3*sd1,sd1))
        par(bg="#EBEBEB",mar= c(2,1,1,1))
        plot('',xlim=c(low,upp),ylim=c(0,2),
           ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
        axis(1, at = seq(mu1-3*sd1,mu1+3*sd1,by=sd1))
        abline(v=x.breaks,col='white')
        points(x=pts,
               y=rep(1,length(pts)), 
              pch=21,
              col= cols,
              bg = cols
        )
      } 
    }
  })
  
  
  #
  # This is the 2nd strip, with the difference
  # 
  output$difference.old <- renderPlot({
    low <- -150
    upp <- 150
    x.breaks <- seq(low,upp,15)
    if (length(samp1())>0) {
      thisOne1 <- tail(values$total1,showMean())
      thisOne2 <- tail(values$total2,showMean())
      diff <- thisOne1 - thisOne2
      df <- data.frame(x=diff)
      p <- ggplot(df, aes(x = x,y=0,colour='black' )) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        # xlab("Red sample mean minus blue sample mean") +
        geom_point(colour="black") 
      
      p
    }
    
  })
  
  
  output$difference <- renderPlot({
    
    low <- -150
    upp <- 150
    # this one is at a different scale
    x.breaks <- seq(low,upp,15)
    
    if (length(samp1())>0) {
      thisOne1 <- tail(values$total1,showMean())
      thisOne2 <- tail(values$total2,showMean())
      diff <- thisOne1 - thisOne2
      n <- length(diff)
      par(bg="#EBEBEB",mar= c(2,1,1,1))
      plot('',xlim=c(low,upp),ylim=c(0,2),
           ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
      axis(1, at = x.breaks)
      abline(v=x.breaks,col='white')
      points(x=diff,y=rep(1,n),
             pch=21,col='black',bg='black'
      )
    } 
  })
  
  #
  # This is the tricky plot, with the histogram
  #   of all sample means.
  #
  output$samplemean.old <- renderPlot({
    
    lo <- -150
    up <- 150
    bin.width = 15
    x.breaks <- seq(lo,up,bin.width)
    
    sampleMeans <- values$diff[-1]
    if (length(sampleMeans)>0) {
      df <- data.frame(x=sampleMeans)
      if (length(sampleMeans == 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_point(size=0.3) +
          scale_x_continuous(limits=c(lo,up),
                             breaks = x.breaks,minor_breaks=NULL) + 
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          ylab("Frequency")
      }
      
      if (length(sampleMeans > 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_dotplot(dotsize=0.3) +
          #coord_cartesian(ylim=c(0,10),expand=T) +
          scale_x_continuous(limits=c(lo,up),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          ylab("Frequency")
      } 
      
      # overlay normal distribution if required, and if there
      #  are enough samples (>= 30)
      if (length(sampleMeans) > 30){
        
        # show histogram
        p <- ggplot(df, aes(x = x)) +
          geom_histogram(binwidth = bin.width,alpha=0.5,) +
          scale_x_continuous(limits=c(lo,up),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          ylab("Frequency")
        
        #geom_histogram(binwidth = bin.width,alpha=0.5,
        #               position="dodge2") +
        #  scale_x_continuous(limits=c(lower,upper),
        #                     breaks = x.breaks,minor_breaks=NULL) +
        #  scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        #  theme(legend.position = "none") 
        
        # show the red line
        if (input$shownormal ) {
          samplesize <-as.numeric(input$n) 
          sd.hat <- sqrt((sd1^2 + sd2^2)/samplesize)
          
          p <- p + stat_function( 
            color="red",
            fun = function(x, mean, sd, n, bw){ 
              dnorm(x = x, mean = mean, sd = sd) * n * bw
            }, 
            args = c(mean = mu1-mu2, sd = sd.hat, 
                     n = counter$countervalue , 
                     bw = bin.width))
          
        }
      }
      p <- p + xlab("")
      p
      
    }
    
  }) 
  
  output$samplemean <- renderPlot({
    
    
    lo <- -150
    up <- 150
    bin.width = 15
    x.breaks <- seq(lo,up,2*bin.width)
    
    par(bg="#EBEBEB",mar= c(2,1,1,1))
    plot('',xlim=c(lo,up),ylim=c(1,2),
         ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
    axis(1, at = x.breaks)
    abline(v=x.breaks,col='white')  
    sampleMeans <- values$diff[-1]
    if (length(sampleMeans)>0 ) {
      if (length(sampleMeans) < 100 ) {
        # show dotplot.
        # put data in bins
        x <- 20* round(sampleMeans/20)
        stripchart(x,method='stack',add=T,pch=21,col='black', bg='black')
      } else {
        # show histogram
        hist(sampleMeans,30,xlab="",ylab="",probability = T,
             xlim=c(lo,up),xaxt="n",yaxt="n",main="")
        axis(1, at = x.breaks)
        if (input$shownormal ) {
          sample.size <- isolate(as.numeric(input$n))
          sd.hat <- sqrt((sd1^2 + sd2^2)/sample.size)
          curve(dnorm(x,mean=mu1-mu2,sd=sd.hat),from = lo ,to=up,add=T,col='red')
        }
      }
    }
  })
  
  
  
  getSummary1 <- function() {
    x1 <- samp1()
    x2 <- samp2()
    points <- data.frame(x1 = x1,x2=x2 )
    
    line1 <-"Red dots indicate sample of frequent exercisers"
    line2 <-"Blue dots indicate sample of not frequent exercisers"
    
    line3 <- ""
    line4 <- ""
    line5 <- ""
    line6 <- ""
    if (dim( points )[1] > 0 ) {
      
      xbar1 <- round(mean(points$x1),2)
      xbar2 <- round(mean(points$x2),2)
      sd1 <- round(sqrt(var(points$x1)),2)
      sd2 <- round(sqrt(var(points$x2)),2)
      line3 <- paste("Mean height of frequent exercisers:",xbar1,"(mm)",sep=' ')
      line4 <- paste("Mean height of not frequent exercisers:",xbar2,"(mm)",sep=' ')
      line5 <- paste("Standard deviation of frequent exercisers is:",
                     sd1,"(mm)",sep=' ')
      line6 <- paste("Standard deviation of not frequent exercisers is:",
                     sd2,"(mm)",sep=' ')
    } 
    
    result <- paste(line1,line2,line3,line4,line5,line6,sep="<br>") 
    if (showSample()==FALSE) {
      result <- ""
    }
    #print(result)
    return(result)
  }
  
  getSummary2 <- function() {
    line1 <- "Red dot indicates sample mean for frequent exercisers."
    line2 <- "Blue dot indicates sample mean for not frequent exercisers."
    result <- paste(line1,line2,sep="<br>")
    return(result)
  }
  
  getSummary3 <- function() {
    result <- paste("Each black dot represents the difference",
                    "between the sample means for the frequent exercisers",
                    "and not frequent exercisers.") 
    return(result)
    
  }
  
  getSummary4 <- function() {
    # the vector of sample means
   sampleMeans <- values$diff[-1]
    if (length(sampleMeans > 0)) {
    
      m.hat <- round(mean(sampleMeans),2)
      ss <- round(sqrt(var(sampleMeans)),2)
      count <- counter$countervalue
      str0 <- paste('Total number of samples: ',count,sep=' ')
      str1 <- paste('Mean of all differences in sample means:',m.hat,"(mm)",sep=' ')
      if (length(sampleMeans) <= 1) {
        str2 <- ''  
      } else {
        str2 <- paste('Standard deviation of all differences in sample means:',ss,"(mm)",sep=' ')
      }
      result <- paste(str0,'<br>',str1,'<br>',str2)
    } else {
      result <- ""
    }
    return(result)
  }
  
  
  output$summary1 <- renderText(
    paste("<font size=4>",getSummary1(),
          "</font>",sep=' ')
  )
  
  output$summary2 <- renderText(
    paste("<font size=4>",getSummary2(),
          "</font>",sep=' ')
  )
 
  
  output$summary3 <- renderText(
    paste("<font size=4>",getSummary3(),
          "</font>",sep=' ')
  )
  
  output$summary4 <- renderText(
    paste("<font size=4>",getSummary4(),
          "</font>",sep=' ')
  )
  
  
 
  
}
