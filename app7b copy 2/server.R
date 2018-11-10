# app 7b.
# Comparing 2 groups: 2 sample distributions
#   in 1 plot
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)

shinyServer <- function(input, output) {
  
  #options(shiny.reactlog = TRUE)
  
  # from combined clean csv
  mu1 = 1712
  mu2 = 1671
  sd1 = 92
  sd2 = 92
  
  # temp:
  mu = mu1
  sd = sd1
  lower <- mu1-3*sd
  upper <- mu1+3*sd
  thisSampleMean <- 0
  shinyjs::disable("shownormal")
  
  # how many sample means do we show on the stripchart?
  showMean <- 1
  # do we show the sample dots in the top chart?
  showSample <- TRUE
  
  
  #allm <- vector()
  samp1 <- reactiveVal()
  samp2 <- reactiveVal()
  meansamp <- reactiveVal()
  #allmeansamp <- reactiveValues(allm=vector())
  values <- reactiveValues(total = 0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  #showDiff <- reactiveValues(summary = 0)
  
  observeEvent(input$clear,{
    thisSampleMean <- 0
    #thisSampleMean1 <- 0
    #allm <- vector()
    samp1 <- round(rnorm(0,mean=mu,sd=sd),1)
    samp2 <- round(rnorm(0,mean=mu,sd=sd),1)
    #samp(samp)
    meansamp1 <- reactiveVal()
    meansamp2 <- reactiveVal()
    #allmeansamp$allm = vector()
    values$total1 = 0
    values$total2 = 0
    #values$diff = 0
    counter$countervalue = 0
    autorun$auto = 0
    samp1(samp1)
    samp2(samp2)
  })
  
  observeEvent(input$n, {
    click("clear")
    invalidateLater(1)
  })
  
  
  # 1 sample
  observeEvent(input$sample,{
    showMean <<- 1
    #showDiff$summary <- 1
    showSample <<- TRUE
    samp1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
    samp2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
    
    samp1(samp1)
    samp2(samp2)
    meansamp1 <- round(mean(samp1),2)
    meansamp2 <- round(mean(samp2),2)
    #thisSampleMean1 <<- meansamp1
    #meansamp1(meansamp1) 
    #thisSampleMean2 <<- meansamp2
    #meansamp2(meansamp2) 
    values$total1 <- c(values$total1,meansamp1)
    values$total2 <- c(values$total2,meansamp2)
    #values$diff <- c(values$diff,meansamp1-meansamp2)
  })
  
  # 10 samples
  observeEvent(input$sample10,{
    showMean <<- 10
    showSample <<- FALSE
    #showDiff$summary <- 0
    
    for (i in 1:10) {
      samp1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
      samp2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
      
      samp1(samp1)
      samp2(samp2)
      meansamp1 <- round(mean(samp1),2)
      meansamp2 <- round(mean(samp2),2)
      #thisSampleMean1 <<- meansamp1
      #meansamp1(meansamp1) 
      #thisSampleMean2 <<- meansamp2
      #meansamp2(meansamp2) 
      values$total1 <- c(values$total1,meansamp1)
      values$total2 <- c(values$total2,meansamp2)
      #values$diff <- c(values$diff,meansamp1-meansamp2)
    }
    
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    #showDiff$summary <- 0 
    showMean <<- 100
    showSample <<- FALSE
    
    for (i in 1:100) {
      
      samp1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
      samp2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
      
      samp1(samp1)
      samp2(samp2)
      meansamp1 <- round(mean(samp1),2)
      meansamp2 <- round(mean(samp2),2)
      #thisSampleMean1 <<- meansamp1
      #meansamp1(meansamp1) 
      #thisSampleMean2 <<- meansamp2
      #meansamp2(meansamp2) 
      values$total1 <- c(values$total1,meansamp1)
      values$total2 <- c(values$total2,meansamp2)
      #values$diff <- c(values$diff,meansamp1-meansamp2)
    }
  })
  
  
  observeEvent(input$sample, {
    counter$countervalue <- counter$countervalue + 1
    if (counter$countervalue < 100) {
      shinyjs::disable("shownormal")
    } else {
      shinyjs::enable("shownormal")
    }
  })
  
  observeEvent(input$sample10, {
    
    counter$countervalue <- counter$countervalue + 10
    if (counter$countervalue < 100) {
      shinyjs::disable("shownormal")
    } else {
      shinyjs::enable("shownormal")
    }
  })
  observeEvent(input$sample100, {
    counter$countervalue <- counter$countervalue + 100
    if (counter$countervalue < 100) {
      shinyjs::disable("shownormal")
    } else {
      shinyjs::enable("shownormal")
    }
  })
  
  observeEvent(input$start, {
    autorun$auto <<- 1
  })
  
  observeEvent(input$stop, {
    autorun$auto <<- 0
  })
  
  # click sample every 10 ms
  observe({
    if (autorun$auto == 1) {
      click("sample")
      invalidateLater(1)
    }
  })
  
  
  
  
  
  
  output$sampleCounter <- renderInfoBox({
    infoBox(
      "Samples: ", paste0(counter$counterValues), icon = icon("list"),
      color = "purple"
    )
  })
  
  
  sd <- 93
  upp <- mu + 3 * sd
  low <- mu - 3 * sd
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
  #y   <- dnorm(x,mean=mu, sd=sd)
  #jitter_y <- max(y)/50
  
  output$plot1 <- renderPlot({
    
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = mu1, sd = sd1)) + 
      stat_function(fun = dnorm, show.legend=F,
                    colour='blue', 
                    args = list(mean = mu2, sd = sd2)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      xlab("Height") +  
      geom_segment(x=1820,xend=1870,y=0.0041,yend=0.0041,colour='blue') +
      annotate("text", x = 1948, y = 0.0041, size=5,
               label = "Does not excercise frequently") +
      geom_segment(x=1820,xend=1870,y=0.0038,yend=0.0038,colour='red') +
      annotate("text", x = 1929, y = 0.0038, size=5,
               label = "Excercises frequently")  
    
    if (showSample & length(samp1())>0 ) {
      # points for the sample
      pts1 <- data.frame(x = samp1(),y=rep(0,length(samp1())))
      pts2 <- data.frame(x = samp2(),y=rep(0,length(samp1())))
      
      p <- p + geom_point(data=pts1,aes(y=y),width=0,
                          colour='red')
      p <- p + geom_point(data=pts2,aes(y=y),width=0,
                          colour='blue')
    }
    p
  }) # end plot1
  
  #
  # This is the strip, with 1 dot for each sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    
    if (length(samp1())>0) {
      
      thisOne1 <- tail(values$total1,showMean)
      thisOne2 <- tail(values$total2,showMean)
      df1 <- data.frame(x=thisOne1,col='darkred')
      df2 <- data.frame(x=thisOne2,col='darkblue')
      df <- rbind(df1,df2)
      p <- ggplot(df, aes(x = x,y=0)) +
                    geom_point(colour=df$col,size=3 ) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Sample mean") 
      p
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
      df1$e <-'blue'
      df2 <- data.frame(x=sampleMeans2)
      df2$e <-'red'
      df <- rbind(df1,df2)
    }
    
    
    if (length(sampleMeans1)>0) {
      
      if (length(sampleMeans1 == 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_point(size=0.3,colour=df$e,fill=df$e) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) + 
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      }
      
      if (length(sampleMeans1 > 1)) {
        p <- ggplot(df, aes(x = x,fill=e)) +
          geom_dotplot(dotsize=0.3,alpha=0.3,colour=df$e,
                       fill=df$e) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          theme(legend.position = "none") 
      } 
      
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
          s <- sd/sqrt(sample.size)
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
  
  # first box down
  getTopSummary <- function() {
    print('hello')
    x1 <- samp1()
    x2 <- samp2()
    points <- data.frame(x1 = x1,x2=x2 )
    
    line1 <-"Red dots indicate sample of exercisers"
    line2 <-"Blue dots indicate sample of non-exercisers"
    
    line3 <- ""
    line4 <- ""
    line5 <- ""
    line6 <- ""
    if (dim( points )[1] > 0 ) {
      
      xbar1 <- round(mean(points$x1),2)
      xbar2 <- round(mean(points$x2),2)
      sd1 <- round(sqrt(points$x1),2)
      sd2 <- round(sqrt(points$x2),2)
      
      line3 <- paste("Mean height of exercisers:",xbar1,"(mm)",sep=' ')
      line4 <- paste("Mean height of non-exercisers:",xbar2,"(mm)",sep=' ')
      line5 <- paste("Standard deviation of exercisers is:",
                   sd1,"(mm)",sep=' ')
      line6 <- paste("Standard deviation of non-exercisers is:",
                   sd2,"(mm)",sep=' ')
    } 
       
    result <- paste(line1,line2,line3,line4,line5,line6,sep="<br>") 
    if (!showSample) {
      result <- ""
    }
     
    return(result)
  }
  
 
  
  # middle box
  getMiddleSummary <- function() {
    line1 <- "Red dot indicates sample mean for exercisers."
    line2 <- "Blue dot indicates sample mean for non-exercisers."
    result <- paste("<br>",line1,line2,sep="<br>")
    
    return(result)
  }
  
  # third box down
  getBottomSummary <- function() {
    print('hi')
    # the vector of sample means
    #df <- data.frame(x = values$total[-1])
    #print(paste('.....',values$total))
    #sampleMeans <- values$diff[-1]
    sampleMeans1 <- values$total1[-1]
    sampleMeans2 <- values$total2[-1]
    
    if (length(sampleMeans1) > 0) {
      print('boo')
      m.hat1 <- round(mean(sampleMeans1),2)
      m.hat2 <- round(mean(sampleMeans2),2)
      
      count <- counter$countervalue
      str0 <- paste('Total number of samples:',count,sep=' ')
      str1 <- paste('Mean of red sample means:',m.hat1,sep=' ')
      str2 <- paste('Mean of blue sample means is:',m.hat2,sep=' ')
      
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
