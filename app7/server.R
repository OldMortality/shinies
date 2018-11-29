# app 7.
# like app5, but binary population
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)



shinyServer <- function(input, output) {
  
  trueProb = 0.3
  lower <- 0
  upper <- 1
  thisSampleMean <- 0
  shinyjs::disable("shownormal")
  
  # how many sample means do we show on the stripchart?
  showMean <- 1
  # do we show the sample dots in the top chart?
  showSample <- TRUE
  
   
  samp <- reactiveVal()
  meansamp <- reactiveVal()
  allmeansamp <- reactiveValues(allm=vector())
  values <- reactiveValues(total = 0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  
  animate.counter <- 0
  max.animate <- 25
  
  
  output$start <- renderUI({
    actionButton("click", label = label(),
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
  
  observeEvent(input$click, {
    
    if (autorun$auto == 1) {
      autorun$auto <- 0  
    } else {
      autorun$auto <- 1
    }
    
  })
  
  
  observeEvent(input$clear,{
    thisSampleMean <- 0
    allm <- vector()
    samp <- rbinom(0,0,0.5)
     
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
    showMean <<- 1
    showSample <<- TRUE
    samp <- rbinom(as.numeric(input$n),size=1,prob=trueProb)
    samp(samp)
    meansamp <- round(mean(samp),3)
    thisSampleMean <<- meansamp
    meansamp(meansamp) 
    values$total <- c(values$total,meansamp) 
  })
  
  # 10 samples
  observeEvent(input$sample10,{
    showMean <<- 10
    showSample <<- FALSE
    for (i in 1:10) {
      samp <- samp <- rbinom(as.numeric(input$n),size=1,prob=trueProb)
      samp(samp)
      meansamp <- round(mean(samp),3)
      thisSampleMean <<- meansamp
      meansamp(meansamp) 
      values$total <- c(values$total,meansamp) 
    }
     
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    for (i in 1:100) {
      showMean <<- 100
      showSample <<- FALSE
      samp <- rbinom(as.numeric(input$n),size=1,prob=trueProb)
      samp(samp)
      meansamp <- round(mean(samp),3)
      thisSampleMean <<- meansamp
      meansamp(meansamp) 
      values$total <- c(values$total,meansamp) 
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
 
  
  
  
  
  
  
  output$sampleCounter <- renderInfoBox({
    infoBox(
      "Samples: ", paste0(counter$counterValues), icon = icon("list"),
      color = "purple"
    )
  })
  
  # population size
  N <- 500
  upp <- 0
  low <- 1
  x.breaks <- seq(0,1,0.1)
   
  # this is the top plot, with the population
  output$plot1 <- renderPlot({
    df1 <- data.frame(class=c(rep(0,700),rep(1,300)))
    p <- ggplot(df1, aes(class,colour='red',fill='red')) + 
      scale_x_discrete(breaks = c(0,1),limits=c(0,1),
                       labels=c('No','Yes')) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      ylab("Proportion") +
      xlab("Excercise frequently")
    p <- p  + geom_bar(width=0.1) 
      #geom_segment(x=0,xend=0,y=0,yend=100,colour='black')
    
    if (showSample & length(samp())>0 ) {
       
      if (length(samp())==10) {
        
        dfs <- data.frame(y=samp())
        print(dfs)
        p <- p + geom_dotplot(data=dfs,aes(x=y),dotsize = 0.4,colour='black',fill='black')
        
        
      } else {
        # sample size = 50 or 100. We cannot show the dots
        #   without filling the graph.
        # number of ones in the sample
        y1 <- sum(samp())
        # number of zeros in the sample
        y0 <- length(samp())- y1
        p <- p +
          geom_segment(aes(x = 0, y=0, xend = 0, yend = y0),colour='black',size=4) + 
          geom_segment(aes(x = 1, y=0, xend = 1, yend = y1),colour='black',size=4) 
      }
    }
    p
  }) # end plot1
  
  #
  # This is the strip, with 1 dot for this sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    if (length(samp())>0) {
      thisOne <- tail(values$total,showMean)
      df <- data.frame(x=thisOne)
      p <- ggplot(df, aes(x = x,y=0 )) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq(0,1,0.1),limits=c(0,1)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Sample proportion")
      if (showMean) { 
        p = p +
          geom_point(colour='blue') 
      }
      p
    }
    #max(table(sampleMeans))+1))
  })
  
  #scale_x_continuous(limits=c(low,upp)) + 
  
  #
  # This is the tricky plot, with the histogram
  #   of all sample means.
  #
  output$samplemean <- renderPlot({
    # put data into 60 bins
    sampleMeans <- values$total[-1]
    if (length(sampleMeans)>0) {
      df <- data.frame(x=sampleMeans)
      if (length(sampleMeans == 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_point(size=trueProb,colour='blue') +
          scale_x_continuous(limits=c(0,1),
                             breaks = seq(0,1,0.1),minor_breaks=NULL) + 
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      }
      
      if (length(sampleMeans > 1)) {
        
        p <- ggplot(df, aes(x = x)) +
          geom_dotplot(dotsize=trueProb,colour='blue') +
          #coord_cartesian(ylim=c(0,10),expand=T) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      } 
      
      # overlay normal distribution if required, and if there
      #  are enough samples (>= 100)
      if (length(sampleMeans) > 100){
        
        if (input$n==10) {
          bin.width <- 0.1 
        } else {
          bin.width <- 0.05 
        }
        # show histogram
        p <- ggplot(df, aes(x = x)) +
          geom_histogram(binwidth = bin.width,
                         colour='blue',
                         fill='white') +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          #scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
          ylab("Frequency") +
          xlab("Sample proportion")
        
          if (counter$countervalue > 100) {
            if (input$shownormal ) {
              sample.size <- as.numeric(input$n)
              # sd of the sampling distribution root(pq/N)
              s <- sqrt(trueProb*(1-trueProb)/sample.size)
               p <- p + stat_function( 
                color="red",
                fun = function(x, mean, sd, n, bw){ 
                  dnorm(x = x, mean = mean, sd = sd) * n * bw
                }, 
                args = c(mean = trueProb, sd = s, 
                       n = counter$countervalue , 
                       bw = bin.width))
             }
          }
      }
      p <- p + xlab("")
      p
      
    }
    
  })
  
  #output$CLTplot3 <- renderPlot({
  
  # work out the sample mean for the vertical line only
  # points <- data.frame(x = samp() )
  #  sum <- 0
  #  if (dim( points )[1] > 0 ) {
  #    sum <- sum(points$x)
  #  }
  #  xbar <- round(1000* sum / as.numeric(input$n))/1000
  # xbar is the sample mean
  
  # df <- data.frame(x = values$total[-1])
  #  sampleMeans <- values$total[-1]
  ##  m.hat <- round(100* mean(sampleMeans))/100
  #  ss <- round(100* var(sampleMeans))/100
  #  label1 <- paste('mean: ',m.hat,sep=" " )
  ##  label2 <- paste('variance: ',ss,sep=" " )
  #  if (length(values$total) > 1) {
  
  #    p <- ggplot(df,aes(x=x)) + geom_blank() + 
  #      geom_histogram(aes(x,stat(density)),
  #                    color="black",
  #                     binwidth=0.1) +
  #      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,
  #                         limits=c(low,upp)) 
  
  
  #    if (input$shownormal) {
  #      p <- p +
  #        stat_function(fun=dnorm,
  #                      color="red",
  #                      args=list(mean=mean(m.hat), 
  #                                sd=sqrt(ss))) 
  #    }
  # show the plot
  #    p
  #  }
  #} ) # end CLTplot3
  
  #output$sampleTable <- renderDataTable(data.frame(samp()),
  #                                      options = list(
  #                                        pageLength = 5,
  #                                        initComplete = I("function(settings, json) {;}")
  #                                      )
  #)
  
  getSampleSummary <- function() {
    #points <- data.frame(x = samp() )
    #sum <- 0
    #if (dim( points )[1] > 0 ) {
    #  sum <- sum(points$x)
    #}
    #count <- counter$countervalue
    #str0 <- paste('This is sample number: ',count,sep=' ')
    #str0b <- "The black dots indicate the sample."
    #str1 <- paste('The number of people, who exercise is ',sum,sep=': ')
    
    #xbar <- round(1000* sum / as.numeric(input$n))/1000
    #str2 <- paste('The average of the sample is ',sum,
    #              'divided by ',as.numeric(input$n),'= ',xbar,sep=' ')
    #result <- paste(str0,'<br>',str0b,'<br>',str1,'<br>',str2)
    
    line1 <- paste("The heights of the red bars indicate",
                   " the proportion in the population.")
    line2 <- paste("The black dots show the exercisers and" ,
                   " the non-exercisers in the sample.")
    result <- paste(line1,"<br>",line2)
    if (!showSample) {
      result <- ""
    }
    return(result)    
     
  }
  
  getSampleMeansSummary <- function() {
    # the vector of sample means
    #df <- data.frame(x = values$total[-1])
    #print(paste('.....',values$total))
    sampleMeans <- values$total[-1]
    
    m.hat <- round(100* mean(sampleMeans))/100
    ss <- round(100* sqrt(var(sampleMeans)))/100
    count <- counter$countervalue
    str0 <- paste('We now have this many samples: ',count,sep=' ')
    str1 <- paste('The mean of all sample proportions is ',m.hat,sep=': ')
    str2 <- paste('The standard deviation of all sample means is ',ss,sep=': ')
    result <- paste(str0,'<br>',str1,'<br>',str2)
    
    return(result)
  }
  
  getOneSampleSummary <- function() {
    return(paste("Each blue dot indicates the proportion of",
           " the sample, who exercise frequenty."))
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
