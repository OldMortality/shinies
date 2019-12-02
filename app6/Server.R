#   app6, 
#
#  Change log
#  07/11/2019 - Rather than drawing  all segments each time, we remember 
#             the plot, and just add a segment for each click of the sample 
#             button. I took updates to the reactives out of the 10,100 samples
#             loops.
#  14/11/2019 - removed assignments to thisSampleMean, which wasn't used. 
#             - removed declarations of lower, upper and a duplicate for sd
#                  as we already had low, upp.
#             - removed change with highest.index.plotted, because vectorising
#                  the adding of segments is better.
#  15/11/2019 - created doSamples() function so as not to repeat code
#             - removed reactive values$total, because it was not used.
#             - removed output$sampleCounter, because it was not used.
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(dplyr)


shinyServer <- function(input, output) {
  
  mu <- 1711
  sd <- 93
  upp <- mu + 3 * sd
  low <- mu - 3 * sd
  
  
  # the current sample, for top plot
  samp <- reactiveVal()
  
  
  # all segments
  segments <- reactiveValues(all_l=vector(),all_u=vector())
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  # number of red intervals
  countReds <- reactiveValues(counter=0)
  
  animate.counter <- 0
  max.animate <- 25
  
  output$start <- renderUI({
    actionButton("click", 
                 label = label(),
                 style=style()
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
  
  
  
  observeEvent(input$clear,{
    
    segments$all_l=vector()
    segments$all_u=vector()
    ###thisSampleMean <- 0
    allm <- vector()
    samp <- round(rnorm(0,mean=mu,sd=sd),1)
    #meansamp <- reactiveVal()
    #allmeansamp$allm = vector()
    #values$total = 0
    counter$countervalue = 0
    countReds$counter = 0
    autorun$auto = 0 
    samp(samp)
    #
    # enable sample buttons
    shinyjs::enable("sample") 
    shinyjs::enable("sample10")
    shinyjs::enable("sample100") 
    shinyjs::enable("start") 
  })
  
   
  
  doSamples <- function(n.samples,input.n) {
    lo <- vector()
    up <- vector()
    reds <- 0
    for (i in 1:n.samples) {
      the.sample <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
      s <- sqrt(var(the.sample))
      lo[i] <- mean(the.sample) + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      up[i] <- mean(the.sample) + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      if (! ((mu>lo[i]) & (mu<up[i])) ) {
        reds <- reds + 1
      }
    }
    result <- list(res.samp = the.sample, 
                   res.lo = lo,
                   res.up = up,
                   res.reds = reds) 
    return(result)
  }
  
  updateReactives <- function(res) {
    samp(res$res.samp)
    segments$all_l <- c(segments$all_l,res$res.lo)
    segments$all_u <- c(segments$all_u,res$res.up)
    countReds$counter <- countReds$counter + res$res.reds
  }

  observeEvent(input$sample,{
    res <- doSamples(n.samples = 1, input.n = input$n) 
    updateReactives(res)
  })
  
  
  observeEvent(input$sample10,{
    res <- doSamples(n.samples = 10, input.n = input$n)
    updateReactives(res)
  })
  
  
  observeEvent(input$sample100,{
    res <- doSamples(n.samples = 100, input.n = input$n) 
    updateReactives(res)
  })
  
  # 
  observeEvent(input$sample, {
    counter$countervalue <- counter$countervalue + 1
  })
  observeEvent(input$sample10, {
    counter$countervalue <- counter$countervalue + 10
  })
  observeEvent(input$sample100, {
    counter$countervalue <- counter$countervalue + 100
  })
  
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
  
  # limit number of samples to limit load on server.
  max.samples <- 250
  observe({
    if (counter$countervalue > max.samples) {
      # disable sample buttons
      shinyjs::disable("sample") 
      shinyjs::disable("sample10")
      shinyjs::disable("sample100") 
      shinyjs::disable("start")
    } 
    
  })
  
  
  
  
  
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
  
  
  topPlot <- ggplot(data = data.frame(x = c(low, upp)), aes(x)) +
    stat_function(fun = dnorm, show.legend=F,
                  colour='red', 
                  args = list(mean = mu, sd = sd)) + 
    ylab("") +
    scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
    scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
    theme(legend.position = "none") +
    xlab("Height (mm)")
  
  
  output$plot1 <- renderPlot({
    
    if (length(samp())>0) {
      # points for the sample
      pts <- data.frame(x = samp(),y=rep(0,length(samp))) 
      topPlot <- topPlot + geom_point(data=pts,aes(y=y),
                          colour='black')
    }
    topPlot
  }) # end plot1
  
  #
  #
  # This is the strip, with 1 dot and interval for this sample mean
  # 
  #
  stripPlot <- ggplot() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
    scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                       limits=c(-0.01,0.01)) + 
    ylab("") + 
    xlab("Confidence interval") 
  
  
  #
  # Render the stripplot.
  #    reacts to: samp, input$n (shouldn't really)
  #  
  output$thissamplemean <- renderPlot({
    
    theMiddlePlot <- stripPlot
    theSample <- samp()
    if (length(theSample > 0)) { 
      thisOne <- mean(samp()) 
      df <- data.frame(x=thisOne)
      s <- sqrt(var(samp()))
      lo <- thisOne + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      up <- thisOne + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
        
      theMiddlePlot <- theMiddlePlot + 
        geom_segment(aes(x=lo,y=0,xend=up,yend=0),colour="blue") +
        geom_point(data=df,aes(x=x,y=0),colour="blue")  
    }
    return(theMiddlePlot)
  })
  
  
  # global variable
  theTrickyPlot <- ggplot() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = x.breaks,minor_breaks = NULL,
                       limits = c(low,upp)) +
    scale_y_continuous(breaks = NULL,minor_breaks = NULL,
                       limits = c(0,125)) +
    ylab("") +
    xlab("Sample mean") +
    geom_vline(xintercept = mu,col = 'red') 
  
  
  #
  #
  # render the tricky plot with the segments
  #    reacts to: segments#all_l, segments$all_u
  #
  #
  output$samplemean.old <- renderPlot({ 
    
    df <- data.frame(x=mu,y=0)
    thisPlot <- theTrickyPlot
    if (length(segments$all_l) > 0) {
      ys <- seq.int(1,length(segments$all_l))
      lo <- tail(segments$all_l,n=length(ys))
      up <- tail(segments$all_u,n=length(ys))
      intervalCol = rep('blue',length(ys))
      intervalCol[which(lo > mu | up < mu)] <- 'red'
      # add the segments to the plot
      df.add <- data.frame(x = lo,
                           y = ys,
                           xend = up,
                           yend = ys,
                           colour = intervalCol)
      thisPlot <- thisPlot + geom_segment(data = df.add,
                                          aes(x = x,y = ys,xend = xend, yend = ys),colour=intervalCol)
      
    }
    thisPlot
    
  })
  
  
  output$samplemean <- renderPlot({ 
    
    df <- data.frame(x=mu,y=0)
    par(bg="#EBEBEB",mar= c(5.5, 1.1, 4.1, 0.5),xpd=T)
    plot('',xlim=c(low,upp),ylim=c(0,max.samples),
         ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
    axis(1, at = seq(mu-3*sd,mu+3*sd,by=sd))
   
    abline(v=x.breaks,col='white')
    segments(mu,-15,mu,2*max.samples,col='black')
    
                   
    if (length(segments$all_l) > 0) {
      ys <- seq.int(1,length(segments$all_l))
      lo <- tail(segments$all_l,n=length(ys))
      up <- tail(segments$all_u,n=length(ys))
      intervalCol = rep('blue',length(ys))
      intervalCol[which(lo > mu | up < mu)] <- 'red'
      # add the segments to the plot
       
      segments(lo,ys,up,ys,col=intervalCol)
      
    }
    
  })
  
  
  
  
  getSampleSummary <- function() {
    points <- data.frame(x = samp() )
    sum <- 0
    xbar <- 0
    if (dim( points )[1] > 0 ) {
      sum <- sum(points$x)
      xbar <- mean(points$x,na.rm = T)
    }
    xbar <- round(xbar,2)
    count <- counter$countervalue 
    str0 <- "The black dots represent the sample."
    str1 <- ""
    if (count > 0 ) {
      str1 <- paste("The mean for sample",count,"is",xbar,sep=' ')
    }
    result <- paste(str0,'<br>',str1,'<br>') 
    return(result)
  }
  
  getSampleMeansSummary <- function() {
    
    #sampleMeans <- values$total[-1]
    
    count <- counter$countervalue
    width.bar <- round(mean(segments$all_u - segments$all_l))
    line1 <- "The blue intervals contain the population mean (1711 mm)"
    line2 <- paste("The red intervals","<u>","do not","</u>",
                   "contain the population mean.",sep=" ")
    line3 <- paste("Number of samples:",count,sep=' ')
    line4 <- paste("Number of red intervals:",countReds$counter,sep=' ')
    line5 <- ""
    if (!is.na(width.bar)) {
      line5 <- paste("Average width of all confidence intervals:",width.bar,"(mm)",sep=' ')  
    }
    result <- paste(line1,'<br>',line2,'<br>',line3,"<br>",line4,"<br>",line5)
    return(result)
  }
  
  getOneSampleSummary <- function() {
    line1 <- "The dot indicates the sample mean."
    line2 <- "The bar indicates the 95% confidence interval."
    lines <- paste(line1,'<br><br>',line2)
    paste("<font size=4>",lines,"</font>",sep="<br>")
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
    paste("<font size=4>",getOneSampleSummary(),
          "</font>") 
  )
  
}
 