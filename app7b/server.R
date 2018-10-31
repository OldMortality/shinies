# app 7b.
# Comparing 2 groups: 2 sample distributions
#   in 1 plot
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
  allmeansamp <- reactiveValues(allm=vector())
  values <- reactiveValues(total = 0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  showDiff <- reactiveValues(summary = 0)
  
  observeEvent(input$clear,{
    thisSampleMean <- 0
    thisSampleMean1 <- 0
    #allm <- vector()
    samp1 <- round(rnorm(0,mean=mu,sd=sd),1)
    samp2 <- round(rnorm(0,mean=mu,sd=sd),1)
    #samp(samp)
    meansamp1 <- reactiveVal()
    meansamp2 <- reactiveVal()
    allmeansamp$allm = vector()
    values$total1 = 0
    values$total2 = 0
    values$diff = 0
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
    showDiff$summary <- 1
    showSample <<- TRUE
    samp1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
    samp2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
    
    samp1(samp1)
    samp2(samp2)
    meansamp1 <- round(mean(samp1),2)
    meansamp2 <- round(mean(samp2),2)
    thisSampleMean1 <<- meansamp1
    #meansamp1(meansamp1) 
    thisSampleMean2 <<- meansamp2
    #meansamp2(meansamp2) 
    values$total1 <- c(values$total1,meansamp1)
    values$total2 <- c(values$total2,meansamp2)
    values$diff <- c(values$diff,meansamp1-meansamp2)
  })
  
  # 10 samples
  observeEvent(input$sample10,{
    showMean <<- 10
    showSample <<- FALSE
    showDiff$summary <- 0
    
    for (i in 1:10) {
      samp1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
      samp2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
      
      samp1(samp1)
      samp2(samp2)
      meansamp1 <- round(mean(samp1),2)
      meansamp2 <- round(mean(samp2),2)
      thisSampleMean1 <<- meansamp1
      #meansamp1(meansamp1) 
      thisSampleMean2 <<- meansamp2
      #meansamp2(meansamp2) 
      values$total1 <- c(values$total1,meansamp1)
      values$total2 <- c(values$total2,meansamp2)
      values$diff <- c(values$diff,meansamp1-meansamp2)
    }
    
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    showDiff$summary <- 0 
    showMean <<- 100
    showSample <<- FALSE
    
    for (i in 1:100) {
      
      samp1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
      samp2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
      
      samp1(samp1)
      samp2(samp2)
      meansamp1 <- round(mean(samp1),2)
      meansamp2 <- round(mean(samp2),2)
      thisSampleMean1 <<- meansamp1
      #meansamp1(meansamp1) 
      thisSampleMean2 <<- meansamp2
      #meansamp2(meansamp2) 
      values$total1 <- c(values$total1,meansamp1)
      values$total2 <- c(values$total2,meansamp2)
      values$diff <- c(values$diff,meansamp1-meansamp2)
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
  
  output$CLTplot1 <- renderPlot({
    
    
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
      xlab("Height")
    
    
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
  }) # end CLTplot1
  
  #
  # This is the strip, with 1 dot for each sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    
    if (length(samp1())>0) {
      
      #thisOne <- mean(samp()) 
      thisOne1 <- tail(values$total1,showMean)
      thisOne2 <- tail(values$total2,showMean)
      df1 <- data.frame(x=thisOne1,col='red')
      df2 <- data.frame(x=thisOne2,col='blue')
      df <- rbind(df1,df2)
      p <- ggplot(df, aes(x = x,y=0)) +
                    geom_point(colour=df$col ) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Sample mean") #+ 
      #geom_point(df2, aes(x = x,y=0,colour='blue' ))
      #if (showMean) { 
      #  p = p +
      #    geom_point() 
      #}
      p
    }
    #max(table(sampleMeans))+1))
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
      #df <- data.frame(x=sampleMeans1,x2=sampleMeans2)
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
          #coord_cartesian(ylim=c(0,10),expand=T) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          theme(legend.position = "none") 
      } 
      
      # overlay normal distribution if required, and if there
      #  are enough samples (>= 100)
      if (length(sampleMeans1) > 100){
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
          #p <- p + stat_function(fun=dnorm,
          #                  color="red",
          #                  args=list(mean=mean(mu), 
          #                            sd=s)) 
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
  
  
  getSampleSummary <- function() {
    points <- data.frame(x1 = samp1(),x2=samp2() )
    sum1 <- 0
    sum2 <- 0
    if (dim( points )[1] > 0 ) {
      sum1 <- sum(points$x1)
      sum2 <- sum(points$x2)
      
    }
    count <- counter$countervalue
    str0 <- paste('This is sample number: ',count,sep=' ')
    str0b <- "The black dots indicates the sample from the black population."
    str0c <- "The blue dots indicates the sample from the blue population."
    
    str1 <- paste('The total of the red sample is ',sum1,sep=': ')
    str1b <- paste('The total of the blue sample is ',sum2,sep=': ')
    
    xbar1 <- round(1000* sum1 / as.numeric(input$n))/1000
    xbar2 <- round(1000* sum2 / as.numeric(input$n))/1000
    str2 <- paste('The average of the black sample is ',sum1,
                  'divided by ',as.numeric(input$n),'= ',xbar1,sep=' ')
    str2b <- paste('The average of the blue sample is ',sum2,
                   'divided by ',as.numeric(input$n),'= ',xbar2,sep=' ')
    
    result <- paste(str0,'<br>',str0b,'<br>',str0c,'<br>',
                    str1,'<br>',str1b,'<br>',
                    str2,'<br>',str2b,'<br>')
    if (!showSample) {
      result <- ""
    }
    return(result)
  }
  
  getSampleMeansSummary <- function() {
    # the vector of sample means
    #df <- data.frame(x = values$total[-1])
    #print(paste('.....',values$total))
    #sampleMeans <- values$diff[-1]
    sampleMeans1 <- values$total1[-1]
    sampleMeans2 <- values$total2[-1]
    
    if (length(sampleMeans1) > 0) {
      
      m.hat1 <- round(100* mean(sampleMeans1))/100
      m.hat2 <- round(100* mean(sampleMeans2))/100
      #ss <- round(100* sqrt(var(sampleMeans)))/100
      count <- counter$countervalue
      str0 <- paste('We now have this many samples for each group: ',count,sep=' ')
      str1 <- paste('The mean of all red sample means is ',m.hat1,sep=': ')
      str2 <- paste('The mean of all blue sample means is ',m.hat2,sep=': ')
      #str2 <- paste('The standard deviation of all sample means is ',ss,sep=': ')
      result <- paste(str0,'<br>',str1,'<br>',str2)
    } else {
      result <- ""
    }
    return(result)
  }
  
  getOneSampleSummary <- function() {
    return("Each dot indicates a sample mean.")
  }
  
  getDifferenceSummary <- function() {
    result <- ""
    print(paste("......",showMean))
    if (showDiff$summary == 0) {
      result <- ("Each dot represents the difference between two sample means.")
    } else {
      if (length(values$diff[-1])>0) {
        thisOne1 <- round(tail(values$total1,showMean),2)
        thisOne2 <- round(tail(values$total2,showMean),2)
        diff <- round(thisOne1 - thisOne2,2)
        str1 <- paste(thisOne1,"minus",thisOne2,"equals",diff,sep=' ')
        str2 <-("The dot represents the difference between the two sample means.")
        result <- paste(str1,"<br>",str2,"<br>")
      } else {
        result <- ""
      }
    }
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
  
  output$differencesummary <- renderText(
    getDifferenceSummary()
  )
  
}