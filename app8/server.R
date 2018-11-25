# app 8.
# Comparing 2 groups: difference in means
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
  #lower <- mu1-3*sd1
  #upper <- mu1+3*sd1
  
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
  
  
  # 1 sample
  observeEvent(input$sample,{
    showMean(1)
    showDiff$summary <- 1
    showSample(TRUE)
    s.1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
    s.2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
    
    samp1(s.1)
    samp2(s.2)
    meansamp1 <- round(mean(s.1),2)
    meansamp2 <- round(mean(s.2),2)
    values$total1 <- c(values$total1,meansamp1)
    values$total2 <- c(values$total2,meansamp2)
    values$diff <- c(values$diff,meansamp1-meansamp2)
  })
  
  # 10 samples
  observeEvent(input$sample10,{
    showMean(10)
    showSample(FALSE)
    showDiff$summary <- 0
    
    for (i in 1:10) {
      s.1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
      s.2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
      
      samp1(s.1)
      samp2(s.2)
      meansamp1 <- round(mean(s.1),2)
      meansamp2 <- round(mean(s.2),2)
      
      values$total1 <- c(values$total1,meansamp1)
      values$total2 <- c(values$total2,meansamp2)
      values$diff <- c(values$diff,meansamp1-meansamp2)
    }
    
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    showDiff$summary <- 0 
    showMean(100)
    showSample(FALSE)
    
    for (i in 1:100) {
       
       s.1 <- round(rnorm(as.numeric(input$n),mean=mu1,sd=sd1),1)
       s.2 <- round(rnorm(as.numeric(input$n),mean=mu2,sd=sd2),1)
       
       samp1(s.1)
       samp2(s.2)
       meansamp1 <- round(mean(s.1),2)
       meansamp2 <- round(mean(s.2),2)
       
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
  upp <- mu1 + 3 * sd1
  low <- mu1 - 3 * sd1
  x.breaks <- round(seq(mu1-3*sd1,mu1+3*sd1,sd1))
  
  
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
      xlab("Height")
    
    
    if (showSample() & length(samp1())>0 ) {
      # points for the sample
      pts1 <- data.frame(x = samp1(),y=rep(0,length(samp1())))
      pts2 <- data.frame(x = samp2(),y=rep(0,length(samp1())))
      
      p <- p + geom_point(data=pts1,aes(y=y), 
                          colour='red')
      p <- p + geom_point(data=pts2,aes(y=y), 
                          colour='blue')
    }
    p
  }) # end plot1
  
  #
  # This is the strip, with 1 dot for each sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    if (showSample()==TRUE){ 
      
      if (length(samp1())>0) { 
      
        thisOne1 <- tail(values$total1,showMean())
        thisOne2 <- tail(values$total2,showMean())
        df1 <- data.frame(x=thisOne1,col='darkred')
        df1$y =0.00
        df2 <- data.frame(x=thisOne2,col='darkblue')
        df2$y =0.00
      
        df <- rbind(df1,df2)
        
        p <- ggplot(df, aes(x = x,y=y)) +
          geom_point(colour = df$col,size=3) +
          theme(legend.position = "none") +
          scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
          ylab("") + 
          xlab("Sample mean")  
        p
    }
  }  
  })
  
  #
  # This is the 2nd strip, with the difference
  # 
  output$difference <- renderPlot({
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
    #max(table(sampleMeans))+1))
  })
  
  
  #
  # This is the tricky plot, with the histogram
  #   of all sample means.
  #
  output$samplemean <- renderPlot({
    
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
          geom_histogram(binwidth = bin.width) +
          scale_x_continuous(limits=c(lo,up),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          ylab("Frequency")
        # show the red line
        if (input$shownormal ) {
          samplesize <-as.numeric(input$n) 
          #s <- sd/sqrt(samplesize)
             
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
  
  
  getSummary1 <- function() {
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
      sd1 <- round(sqrt(var(points$x1)),2)
      sd2 <- round(sqrt(var(points$x2)),2)
      line3 <- paste("Mean height of exercisers:",xbar1,"(mm)",sep=' ')
      line4 <- paste("Mean height of non-exercisers:",xbar2,"(mm)",sep=' ')
      line5 <- paste("Standard deviation of exercisers is:",
                     sd1,"(mm)",sep=' ')
      line6 <- paste("Standard deviation of non-exercisers is:",
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
    line1 <- "Red dot indicates sample mean for exercisers."
    line2 <- "Blue dot indicates sample mean for non-exercisers."
    result <- paste(line1,line2,sep="<br>")
    return(result)
  }
  
  getSummary3 <- function() {
    result <- paste("Each black dot represents the difference",
                    "between the blue and the red mean",
                    "for a pair of samples.") 
    return(result)
    
  }
  
  getSummary4 <- function() {
    # the vector of sample means
    #df <- data.frame(x = values$total[-1])
    sampleMeans <- values$diff[-1]
    if (length(sampleMeans > 0)) {
    
      m.hat <- round(100* mean(sampleMeans))/100
      ss <- round(100* sqrt(var(sampleMeans)))/100
      count <- counter$countervalue
      str0 <- paste('Total number of samples: ',count,sep=' ')
      str1 <- paste('Mean of all differences in sample means:',m.hat,sep=' ')
      if (length(sampleMeans) <= 1) {
        str2 <- ''  
      } else {
        str2 <- paste('Standard deviation of all differences in sample means:',ss,sep=' ')
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
