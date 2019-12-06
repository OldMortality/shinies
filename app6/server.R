#   app6, 
#
#  Change log
#  07/11/2019 - Rather than drawing  all segments each time, we remember 
#             the plot, and just add a segment for each click of the sample 
#             button. I took updates to the reactives out of the 10,100 samples
#             loops.
#  14/11/2019 - removed assignments to thisSampleMean, which wasn't used. 
#             - removed declarations of low, upp and a duplicate for sd
#                  as we already had lower, upper.
#             - removed change with highest.index.plotted, because vectorising
#                  the adding of segments is better.
#  15/11/2019 - created doSamples() function so as not to repeat code
#             - removed reactive values$total, because it was not used.
#             - removed output$sampleCounter, because it was not used.
#  06/12/2019 - Removed ggplot2 from three plots, for speed.
#
#
#
#
library(shiny)
library(shinydashboard)
library(shinyjs) 


shinyServer <- function(input, output) {
  
  mu <- 1711
  sd <- 93
  lower <- mu - 3 * sd
  upper <- mu + 3 * sd
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd)) 
  
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
    s.size <- isolate(as.numeric(input$n))
    for (i in 1:n.samples) {
      the.sample <- round(rnorm(as.numeric(s.size),mean=mu,sd=sd),1)
      s <- sqrt(var(the.sample))
      lo[i] <- mean(the.sample) + qt(0.025,s.size-1) * s/sqrt(s.size)
      up[i] <- mean(the.sample) + qt(0.975,s.size-1) * s/sqrt(s.size)
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
  
  
  
  
  
  
  
 
  #
  #  Top plot, with the population Normal curves
  #
  output$plot1 <- renderPlot({ 
   
    # background color, margins and plot outside area (for legend)
    par(bg="#EBEBEB",mar= c(5.5, 1.1, 4.1, 0.5),xpd=T)
    # use base R for plotting is much faster
    plot('',xlim=c(lower,upper),ylim=c(0,0.003),
         ylab="",xlab="",xaxt="n",yaxt="n",bty="n") 
    abline(v=x.breaks,col='white')
    axis(1,  at =  seq(mu-3*sd,mu+3*sd,by=sd))
    curve(dnorm(x,mean=mu,sd=sd),lower,upper,col='red',add=T)
    
    if (length(samp())>0) {
      s <- samp()
      n <- length(s)
      points(c(s),rep(0,n),pch=21,col='black',bg='black')
    }
    
  }) # end plot1
  
  #
  #
  # This is the strip, with 1 dot and interval for this sample mean
  # 
  #
  # stripPlot <- ggplot() +
  #   theme(legend.position = "none") +
  #   scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(lower,upper)) +
  #   scale_y_continuous(breaks = NULL,minor_breaks=NULL,
  #                      limits=c(-0.01,0.01)) + 
  #   ylab("") + 
  #   xlab("Confidence interval") 
  # 
  
  #
  # Render the stripplot.
  #    reacts to: samp, input$n (shouldn't really)
  #  
  # output$thissamplemean <- renderPlot({
  #   
  #   theMiddlePlot <- stripPlot
  #   theSample <- samp()
  #   if (length(theSample > 0)) { 
  #     thisOne <- mean(samp()) 
  #     df <- data.frame(x=thisOne)
  #     s <- sqrt(var(samp()))
  #     lo <- thisOne + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
  #     up <- thisOne + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
  #       
  #     theMiddlePlot <- theMiddlePlot + 
  #       geom_segment(aes(x=lo,y=0,xend=up,yend=0),colour="blue") +
  #       geom_point(data=df,aes(x=x,y=0),colour="blue")  
  #   }
  #   return(theMiddlePlot)
  # })

  output$thissamplemean <- renderPlot({

    par(bg="#EBEBEB",mar= c(2,1,1,1))
    plot('',xlim=c(lower,upper),ylim=c(0,2),
         ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
    axis(1, at = x.breaks)
    abline(v=x.breaks,col='white')  
    theSample <- samp()
    if (length(theSample > 0)) {
      thisOne <- mean(theSample)
      s <- sqrt(var(samp()))
      lo <- thisOne + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      up <- thisOne + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      segments(x0=lo,y0=0.1,x1=up,y1=0.1,col="blue") 
      points(x=thisOne,y=0.1,col="blue", pch=21,bg='blue')
    }
    
  })

    
 
  
  
  #
  # This is the strip, with 1 dot for each sample mean
  # 
  # output$thissamplemean <- renderPlot({
  #   
  #   if (length(samp())>0) {
  #     thisOne1 <- tail(values$total1,showMean())
  #     thisOne2 <- tail(values$total2,showMean())
  #     pts <- c(thisOne1,thisOne2)
  #     n <- length(thisOne1)
  #     cols <- c(rep('red',n),rep('blue',n))
  #     x.breaks <- round(seq(mu1-3*sd1,mu1+3*sd1,sd1))
  #     par(bg="#EBEBEB",mar= c(2,1,1,1))
  #     plot('',xlim=c(lower,upper),ylim=c(0,2),
  #          ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
  #     axis(1, at = seq(mu1-3*sd1,mu1+3*sd1,by=sd1))
  #     abline(v=x.breaks,col='white')
  #     points(x=pts,y=c(rep(1,n),rep(1.5,n)),
  #            pch=21,
  #            col= cols,
  #            bg = cols
  #     )
  #   } 
  # })
  
  
  
  # global variable
  theTrickyPlot <- ggplot() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = x.breaks,minor_breaks = NULL,
                       limits = c(lower,upper)) +
    scale_y_continuous(breaks = NULL,minor_breaks = NULL,
                       limits = c(0,125)) +
    ylab("") +
    xlab("Sample mean") +
    geom_vline(xintercept = mu,col = 'red') 
  
  
 
  
  output$samplemean <- renderPlot({ 
    
    df <- data.frame(x=mu,y=0)
    par(bg="#EBEBEB",mar= c(5.5, 1.1, 4.1, 0.5),xpd=T)
    plot('',xlim=c(lower,upper),ylim=c(0,max.samples),
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
 