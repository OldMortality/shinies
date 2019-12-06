# app 5.
# 
# Change log
# 24/11/2019 - Moved definition of top plot to global variable
# 28/11/2019 - changed to base plot (away from ggplot, because that is too slow.)
# 06/12/2019 - removed ggplot.
#            - general tidy up.
#
#
#
library(shiny)
library(shinydashboard)
library(shinyjs)



shinyServer <- function(input, output) {
  
  mu <- 1711
  sd <- 93
  upper <- mu + 3 * sd
  lower <- mu - 3 * sd 
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
  
  shinyjs::disable("shownormal")
  
  samp <- reactiveVal()
  meansamp <- reactiveVal()
  allmeansamp <- reactiveValues(allm=vector())
  values <- reactiveValues(total = 0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
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
  
  ## react to the start/stop button
  observeEvent(input$click, {
    if (autorun$auto == 1) {
      autorun$auto <- 0
    } else {
      autorun$auto <- 1
    }
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
  
  
  observeEvent(input$clear,{
    #clear shownormal
    if (input$shownormal) {
      shinyjs::click("shownormal")
    }
    shinyjs::disable("shownormal")
    
    samp <- round(rnorm(0,mean=mu,sd=sd),1) 
    meansamp <- reactiveVal()
    allmeansamp$allm = vector()
    values$total = 0
    counter$countervalue = 0
    autorun$auto = 0
    samp(samp)
  })
  
  observeEvent(input$n, {
    click("clear")
    invalidateLater(1)
  })
    
  
  # 1 sample
  observeEvent(input$sample,{  
    samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
    samp(samp)
    meansamp <- round(mean(samp),2)
    meansamp(meansamp) 
    values$total <- c(values$total,meansamp) 
  })
  
  # 10 samples
  observeEvent(input$sample10,{ 
    newSamples <- rep(NA,10)
    for (i in 1:10) {
      samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
      meansamp <- round(mean(samp),2)
      newSamples[i] <- meansamp
    }
    # 14/11/2019 moved updates to reactives out of the loop
    samp(samp)
    meansamp(meansamp) 
    values$total <- c(values$total,newSamples) 
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    newSamples <- rep(NA,100)
    for (i in 1:100) {
      samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
      meansamp <- round(mean(samp),2)
      newSamples[i] <- meansamp 
    }
    # update reactives
    samp(samp)
    meansamp(meansamp) 
    values$total <- c(values$total,newSamples) 
    
  })
  
  
  handleSample <- function(n.samples) {
    counter$countervalue <- counter$countervalue + n.samples
    if (counter$countervalue < 100) {
      shinyjs::disable("shownormal")
    } else {
      shinyjs::enable("shownormal")
    }
  }
  
  observeEvent(input$sample, {
    handleSample(n.samples = 1)
   
  })
  observeEvent(input$sample10, {
    handleSample(n.samples = 10)
  })
  
  observeEvent(input$sample100, {
    handleSample(n.samples = 100)
  })
  

  
  
  output$populationPlot <- renderPlot({ 
    
    # background color, margins and plot outside area (for legend)
    par(bg="#EBEBEB",mar= c(5.5, 1.1, 4.1, 0.5),xpd=T)
    # use base R for plotting is much faster
    plot('',xlim=c(lower,upper),ylim=c(0,0.004),
         ylab="",xlab="",xaxt="n",yaxt="n",bty="n") 
    abline(v=x.breaks,col='white')
    axis(1,  at =  seq(mu-3*sd,mu+3*sd,by=sd))
    curve(dnorm(x,mean=mu,sd=sd),lower,upper,col='red',add=T)
    
    if (length(samp())>0) {
      s <- samp()
      n <- length(s)
      points(c(s),rep(0,n),pch=21,col='black',bg='black')
    }
    
  }) # end populationPlot
   
  
  #
  # This is the strip, with 1 dot for this sample mean
  # 
  output$thissamplemean <- renderPlot({
    par(bg="#EBEBEB",mar= c(2,1,1,1))
    plot('',xlim=c(lower,upper),ylim=c(0,1),
       ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
    axis(1, at = x.breaks)
    abline(v=x.breaks,col='white')  
    theSample <- samp()
    n <- isolate(as.numeric(input$n))
    if (length(theSample > 0)) {
      thisOne <- mean(theSample)
      s <- sqrt(var(samp()))
      lo <- thisOne + qt(0.025,n-1) * s/sqrt(n)
      up <- thisOne + qt(0.975,n-1) * s/sqrt(n) 
      points(x=thisOne,y=0.5,col="blue", pch=21,bg='blue')
    } 
  })
  
  #
  #
  # This is the tricky plot, with the histogram
  #   of all sample means.
  #   It's a dotplot, but changes into a histogram
  #   at > 100 dots.
  #
   
  output$samplemean <- renderPlot({

    par(bg="#EBEBEB",mar= c(2,1,1,1))
    plot('',xlim=c(lower,upper),ylim=c(1,2),
           ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
    axis(1, at = x.breaks)
    abline(v=x.breaks,col='white')  
    sampleMeans <- values$total[-1]
    if (length(sampleMeans)>0 ) {
      if (length(sampleMeans) < 100 ) {
      # show dotplot.
      # put data in bins
      x <- 20* round(sampleMeans/20)
      stripchart(x,method='stack',add=T,pch=21,col='blue', bg='blue')
      } else {
        # show histogram
        hist(sampleMeans,30,xlab="",ylab="",probability = T,
            xlim=c(lower,upper),xaxt="n",yaxt="n",main="")
          axis(1, at = x.breaks)
        if (input$shownormal ) {
          sample.size <- isolate(as.numeric(input$n))
          s <- sd/sqrt(sample.size)
          curve(dnorm(x,mean=mu,sd=s),from = lower,to=upper,add=T,col='red')
        }
      }
    }
  })

  
  
  getSampleSummary <- function() {
    points <- data.frame(x = samp() )
    sum <- 0
    if (dim( points )[1] > 0 ) {
      sum <- sum(points$x)
    }
    count <- counter$countervalue
    xbar <- round(1000* sum / as.numeric(input$n))/1000
    str0 <- "The black dots represent the sample."
    str1 <- paste("The mean for sample",count,"=",xbar,"(mm)",sep=' ')
    result <- paste(str0,'<br>',str1,'<br>')
     
    return(result)
  }
  
  getSampleMeansSummary <- function() {
    # the vector of sample means
    sampleMeans <- values$total[-1]
    
    m.hat <- round(100* mean(sampleMeans))/100
    ss <- round(100* sqrt(var(sampleMeans)))/100
    count <- counter$countervalue
    str0 <- paste('We now have this many samples: ',count,sep='')
    str1 <- paste('The mean of all sample means = ',m.hat,"(mm)",sep='')
    str2 <- paste('The standard deviation of all sample means = ',ss,"(mm)",sep='')
    result <- paste(str0,'<br>',str1,'<br>',str2)
    return(result)
  }
  
  getOneSampleSummary <- function() {
    line <- "Each blue dot indicates a sample mean."
    paste("<font size=4>",line,
          "</font>")
  }
  
  output$sampleSummary <- renderText(
    paste("<font size=4>",getSampleSummary(),
          "</font>")
  )
  
  output$sampleMeanSummary <- renderText(
    paste("<font size=4>",getSampleMeansSummary(),
          "</font>")
  )
  
  output$onesamplesummary <- renderText(
    getOneSampleSummary()
  )
  
}
