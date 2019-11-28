# app 7b.
#
#
# Comparing 2 groups: 2 sample distributions in 1 plot
#
# Change log:
#
#   28/11/2019 - changed to base plot (away from ggplot, because that is too slow.)
#
#
#
#



library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)


shinyServer <- function(input, output) {
  
  #options(shiny.reactlog = TRUE)
  
  # from combined clean csv
  mu1 = 1712
  mu2 = 1671
  sd1 = 92
  sd2 = 92 
  lower <- mu1 - 3 * sd1
  upper <- mu1 + 3 * sd1
  x.breaks <- round(seq(lower,upper,sd1))
  
  shinyjs::disable("shownormal")
  
  # how many sample means do we show on the stripchart?
  showMean <- reactiveVal(1)
  # do we show the sample dots in the top chart?
  showSample <- reactiveVal(FALSE)
  
  samp1 <- reactiveVal()
  samp2 <- reactiveVal()
  meansamp <- reactiveVal()
  values <- reactiveValues(total = 0,total2=0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  animate.counter <- 0
  max.animate <- 25
  
  output$start <- renderUI({
    actionButton("click", label = label(),
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
  
  ## respond to the Start/Stop button
  observeEvent(input$click, {
    if (autorun$auto == 1) {
      autorun$auto <- 0  
    } else {
      autorun$auto <- 1
    }
  }) 
  
  
  observeEvent(input$clear,{
    thisSampleMean <- 0  
    meansamp1 <- reactiveVal()
    meansamp2 <- reactiveVal()
    values$total1 = 0
    values$total2 = 0
    counter$countervalue = 0
    autorun$auto = 0
    samp1(numeric(0))
    samp2(numeric(0))
  })
  
  observeEvent(input$n, {
    click("clear")
    #invalidateLater(1)
  })
  
  ##
  ## Adds to the number of all samples taken.
  ## Manages the shownormal button.
  ##
  updateCounterValue <- function(n.add) {
    counter$countervalue <- counter$countervalue + n.add
    if (counter$countervalue < 100) {
      shinyjs::disable("shownormal")
    } else {
      shinyjs::enable("shownormal")
    }
  }
  
    
  doSamples <- function(n.samples) {
    showMean(n.samples)
    showSample((n.samples == 1)) 
    first.means <- vector()
    second.means <- vector()
    for (i in 1:n.samples) {
      sample.first <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
      sample.second <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
      first.means[i] <- round(mean(sample.first),2)
      second.means[i] <- round(mean(sample.second),2)
    }  
    # the latest samples
    samp1(sample.first)
    samp2(sample.second) 
    # vector of all sample means
    values$total1 <- c(values$total1,first.means)
    values$total2 <- c(values$total2,second.means)
    updateCounterValue(n.samples)
  }
  
  # 1 sample
  observeEvent(input$sample,{
    doSamples(n.samples = 1)
  })
  
  
  # 10 samples
  observeEvent(input$sample10,{
      doSamples(n.samples = 10)
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    doSamples(n.samples = 100)
   
  })
  
  
  
 
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
  
 
  
  #
  #  Top plot, with the population Normal curves
  #
  output$plot1 <- renderPlot({
    
    xbreaks <- seq(mu1-3*sd1,mu1+3*sd1,by=sd1)
    
    # background color, margins and plot outside area (for legend)
    par(bg="#EBEBEB",mar= c(5.5, 1.1, 4.1, 0.5),xpd=T)
    # use base R for plotting is much faster
    plot('',xlim=c(lower,upper),ylim=c(0,0.0050),
         ylab="",xlab="",xaxt="n",yaxt="n",bty="n") 
    abline(v=xbreaks,col='white')
        axis(1,  at =  seq(mu1-3*sd1,mu1+3*sd1,by=sd1))
    curve(dnorm(x,mean=mu1,sd=sd1),lower,upper,col='red',add=T)
    curve(dnorm(x,mean=mu2,sd=sd2),lower,upper,col='blue',add=T)
    legend(1750,0.0060,
           c('Does not excercise frequently',
             'Exercises frequently'),
           lty=c(1,1), 
           bty = "n",  # no box
           lwd=c(2.5,2.5),col=c('blue','red')) 
    
    if (showSample() & length(samp1())>0 ) {
      s1 <- samp1()
      s2 <- samp2()
      n <- length(s1)
      cols <- c(rep('blue',n),rep('red',n))
      points(c(s1,s2),rep(0,(2*n)),pch=21,col=cols,bg=cols)
    }
    
  }) # end plot1
  
  
  #
  # This is the strip, with 1 dot for each sample mean
  # 
  # output$thissamplemean2 <- renderPlot( {
  #   
  #   if (length(samp1())>0) {
  #     
  #     pts <- c(tail(values$total1,showMean()),
  #       tail(values$total2,showMean()))
  #     
  #     par(bg="#EBEBEB",mar= c(5.5, 1.1, 4.1, 0.5),xpd=T)
  #     # use base R for plotting is much faster
  #     plot('',xlim=c(lower,upper),ylim=c(0,0.0050),
  #          ylab="",xlab="",xaxt="n",yaxt="n",bty="n") 
  #     abline(v=xbreaks,col='white')
  #     axis(1,  at =  seq(mu1-3*sd1,mu1+3*sd1,by=sd1))
  #     points(pts,y=rep(0,length(pts),
  #                      col=c(rep('red',length(pts)/2),rep('blue',length(pts)/2))))
  #   }
  #  
  # })

  
  output$thissamplemean <- renderPlot({
    
    if (length(samp1())>0) {
      thisOne1 <- tail(values$total1,showMean())
      thisOne2 <- tail(values$total2,showMean())
      pts <- c(thisOne1,thisOne2)
      n <- length(thisOne1)
      cols <- c(rep('red',n),rep('blue',n))
      x.breaks <- round(seq(mu1-3*sd1,mu1+3*sd1,sd1))
      par(bg="#EBEBEB",mar= c(2,1,1,1))
      plot('',xlim=c(lower,upper),ylim=c(0,2),
          ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
      axis(1, at = seq(mu1-3*sd1,mu1+3*sd1,by=sd1))
      abline(v=xbreaks,col='white')
      points(x=pts,y=rep(1,(2*n)),
              pch=21,
              col= cols
              bg = cols
      )
    } 
  })
  
  
  #
  # This is the tricky plot, with the dotplot/histogram
  #   of all sample means.
  #
  output$samplemean <- renderPlot({
    #  
    sampleMeans1 <- values$total1[-1]
    sampleMeans2 <- values$total2[-1]
    if (length(sampleMeans1>0)) {
      df1 <- data.frame(x=sampleMeans1)
      df1$e <-'darkblue'
      df2 <- data.frame(x=sampleMeans2)
      df2$e <-'darkred'
      df <- rbind(df1,df2)
    }
    
    dsize <- 0.6
    if (length(sampleMeans1)>0) {
      
      if (length(sampleMeans1 >= 1)) {
        p <- ggplot(df, aes(x = x,fill=e,colour=e)) +
          geom_dotplot(dotsize=dsize,alpha=1) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          theme(legend.position = "none") 
      } 
      
      # show histogram if there are >30 samples
      # overlay normal distribution if required, and if there
      #  are enough samples (>= 30)
      if (length(sampleMeans1) > 30){
        bin.width <- 10
        # show histogram
        p <- ggplot(df, aes(x = x,fill=e,colour=e)) +
          geom_histogram(binwidth = bin.width,alpha=0.5,
                         position="dodge2") +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          theme(legend.position = "none") 
        if (input$shownormal ) {
          sample.size <- as.numeric(input$n)
          
          # sd1==sd2 so we can take either 
          s <- sd1/sqrt(sample.size)
          p <- p + stat_function( 
            color="red",
            fun = function(x, mean, sd, n, bw){ 
              dnorm(x = x, mean = mean, sd = sd) * n * bw
            }, 
            args = c(mean = mu1, sd = s, 
                     n = counter$countervalue , 
                     bw = bin.width)) +
             stat_function( 
              color="blue",
              fun = function(x, mean, sd, n, bw){ 
                dnorm(x = x, mean = mean, sd = sd) * n * bw
              }, 
              args = c(mean = mu2, sd = s, 
                       n = counter$countervalue , 
                       bw = bin.width))+
            theme(legend.position = "none") 
        }
      }
      p <- p + xlab("")
      p
      
    }
    
  })
  
  ## first box down, right column
  getTopSummary <- function() {
    
    topSum1 <- "<b>HUBS191 data</b>"
    topSum2 <- "The mean height of not-frequent exercisers is 1671 mm"
    topSum3 <- "The standard deviation of the height of not frequent exercisers is 92 mm"
    topSum4 <- "The mean height of frequent exercisers is 1712 mm"
    topSum5 <- "The standard deviation of the height of frequent exercisers is 92 mm"
    topSum <- paste(topSum1,topSum2,topSum3,topSum4,topSum5,sep="<br>")
    result <- topSum
    
    ## show the summary of 1 sample only if we have created 1 sample (as opposed to 10,100)
    if (showSample()) {
      x1 <- samp1()
      x2 <- samp2()
      points <- data.frame(x1 = x1,x2=x2 )
      line0 <- "<b>One sample</b>"
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
        line3 <- paste("Mean height of exercisers:",xbar1,"mm",sep=' ')
        line4 <- paste("Mean height of not frequent exercisers:",xbar2,"mm",sep=' ')
        line5 <- paste("Standard deviation of frequent exercisers is:",
                   sd1,"mm",sep=' ')
        line6 <- paste("Standard deviation of not frequent exercisers is:",
                   sd2,"mm",sep=' ')
      } 
       
      result <- paste(topSum,line0,
                    line1,line2,line3,line4,line5,line6,sep="<br>")
    }
    return(result)
  }
  
 
  
  # middle box
  getMiddleSummary <- function() {
    line1 <- "Red dot indicates sample mean for frequent exercisers."
    line2 <- "Blue dot indicates sample mean for not frequent exercisers."
    result <- paste(line1,line2,sep="<br>")
    
    return(result)
  }
  
  # third box down
  getBottomSummary <- function() {
    
    sampleMeans1 <- values$total1[-1]
    sampleMeans2 <- values$total2[-1]
    
    if (length(sampleMeans1) > 0) {
      
      m.hat1 <- round(mean(sampleMeans1),2)
      m.hat2 <- round(mean(sampleMeans2),2)
      
      count <- counter$countervalue
      str0 <- paste('Total number of samples:',count,sep=' ')
      str1 <- paste('Mean of red sample means:',m.hat1,"(mm)",sep=' ')
      str2 <- paste('Mean of blue sample means is:',m.hat2,"(mm)",sep=' ')
      
      result <- paste(str0,'<br>',str1,'<br>',str2)
    } else {
      result <- ""
    }
    return(result)
  }
  
  
  
 
  
  output$topSummary <- renderText(
    paste("<font size=4>",getTopSummary(),
          "</font>",sep=' ')
  )
  
  output$middleSummary <- renderText(
    paste("<font size=4>",getMiddleSummary(),
          "</font>",sep=' ')
  )
  
  output$bottomSummary <- renderText(
    paste("<font size=4>",getBottomSummary(),
          "</font>",sep=' ')
    
  )
  
  
  #output$differencesummary <- renderText(
  #  getDifferenceSummary()
  #)
  
}
