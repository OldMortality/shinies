# app 5.
# 
# Change log
# 24/11/2019 - Moved definition of top plot to global variable
#
#
#
#
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
  
  mu <- 1711
  sd <- 93
  upper <- mu + 3 * sd
  lower <- mu - 3 * sd 
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
    #thisSampleMean <- meansamp
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
      #thisSampleMean <- meansamp
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
  

  
   
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
 
  
  popPlot <-  ggplot(data = data.frame(x = c(lower, upper)), aes(x)) +
    stat_function(fun = dnorm, show.legend=F,
                  colour='red', 
                  args = list(mean = mu, sd = sd)) + 
    ylab("") +
    scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
    scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
    theme(legend.position = "none") +
    xlab("Height (mm)")
  
  
  ## This is topplot, with the Gaussian curve and the
  ##   sample along the bottom
  output$populationPlot <- renderPlot({
    
    p <- popPlot
    if (length(samp()) > 0 ) {
      # points for the sample
      pts <- data.frame(x = samp(),y=rep(0,length(samp)))
      p <- p + geom_point(data=pts,aes(y=y),#width=0,
                          colour='black')
    }
    p
  }) # end populationPlot
  
  
  #
  # This is the strip, with 1 dot for this sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    if (length(samp())>0) {
      # show the sample mean of the last sample
      thisOne <- tail(values$total,1)
      df <- data.frame(x=thisOne)
      p <- ggplot(df, aes(x = x,y=0 )) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(lower,upper)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Height (mm)") +
        geom_point(size=2,colour='blue')
     
      p
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
    
    sampleMeans <- values$total[-1]
    if (length(sampleMeans)>0) {
      df <- data.frame(x=sampleMeans)
      
      thePlot <- ggplot(df, aes(x = x)) +
        scale_x_continuous(limits=c(lower,upper),
                           breaks = x.breaks,minor_breaks=NULL) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) + 
        xlab("")
      if (length(sampleMeans) <= 100) {
        p <- thePlot + 
          geom_dotplot(dotsize=0.3,colour='blue',binwidth = 30)  
      } else {
        # show histogram
        bin.width <- 10
        p <- thePlot + 
          geom_histogram(binwidth = bin.width,fill='white',colour='blue')
        ## overlay Gaussian
        if (input$shownormal ) {
          sample.size <- as.numeric(input$n)
          s <- sd/sqrt(sample.size) 
          p <- p + stat_function( 
            color="red",
            fun = function(x, mean, sd, n, bw){ 
              dnorm(x = x, mean = mean, sd = sd) * n * bw
            }, 
            args = c(mean = mu, sd = s, 
                     n = counter$countervalue , 
                     bw = bin.width))
        }
        ##
      }
      
      p 
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
