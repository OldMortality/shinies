# app 15.
# Like app5, but with conduction velocity,
#   and with a slider.

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


shinyServer <- function(input, output) {
  
  #mu <- 55
  sd <- 13 
  #lower <- input$mu-3*sd
  #upper <- input$mu+3*sd
  thisSampleMean <- 0
  shinyjs::disable("shownormal")
  
  # how many sample means do we show on the stripchart?
  showMean <- 1
  # do we show the sample dots in the top chart?
  showSample <- TRUE
  
  
  #allm <- vector()
  samp <- reactiveVal()
  meansamp <- reactiveVal()
  allmeansamp <- reactiveValues(allm=vector())
  values <- reactiveValues(total = 0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  
  
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
    #allm <- vector()
    samp <- round(rnorm(0,mean=input$mu,sd=sd),1)
    #samp(samp)
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
    samp <- round(rnorm(as.numeric(input$n),mean=input$mu,sd=sd),1)
    samp(samp)
    meansamp <- round(mean(samp),2)
    thisSampleMean <<- meansamp
    meansamp(meansamp) 
    values$total <- c(values$total,meansamp) 
  })
  
  # 10 samples
  observeEvent(input$sample10,{
    showMean <<- 10
    showSample <<- FALSE
    for (i in 1:10) {
      samp <- round(rnorm(as.numeric(input$n),mean=input$mu,sd=sd),1)
      samp(samp)
      meansamp <- round(mean(samp),2)
      thisSampleMean <<- meansamp
      meansamp(meansamp) 
      values$total <- c(values$total,meansamp) 
    }
    #samp <- NULL
    #samp(samp)
  })
  
  # 100 samples
  observeEvent(input$sample100,{
    for (i in 1:100) {
      showMean <<- 100
      showSample <<- FALSE
      samp <- round(rnorm(as.numeric(input$n),mean=input$mu,sd=sd),1)
      samp(samp)
      meansamp <- round(mean(samp),2)
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
  
  
  
  
  
  
  output$CLTplot1 <- renderPlot({
    upp <- input$mu + 3 * sd
    low <- input$mu - 3 * sd
    x.breaks <- round(seq(input$mu-3*sd,input$mu+3*sd,sd))
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
    ) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = input$mu, sd = sd)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      xlab("Conduction velocity")
    
    
    if (showSample & length(samp())>0 ) {
      # points for the sample
      pts <- data.frame(x = samp(),y=rep(0,length(samp)))
      
      p <- p + geom_point(data=pts,aes(y=y),width=0,
                          colour='black')
    }
    p
  }) # end CLTplot1
  
  #
  # This is the strip, with 1 dot for this sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    x.breaks <- round(seq(input$mu-3*sd,input$mu+3*sd,sd))
    upper <- input$mu + 3 * sd
    lower <- input$mu - 3 * sd
    
    if (length(samp())>0) {
      
      #thisOne <- mean(samp()) 
      thisOne <- tail(values$total,showMean)
      
      df <- data.frame(x=thisOne)
      p <- ggplot(df, aes(x = x,y=0 )) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(lower,upper)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Sample mean")
      #if (showMean) { 
      p = p +
        geom_point(colour='black') 
      #}
      p
    }
    #max(table(sampleMeans))+1))
  })
  
  #scale_x_continuous(limits=c(low,upp)) + 
  
  #
  # This is the tricky plot, with the histogram
  #   of all sample means.
  #
  #
  output$samplemean <- renderPlot({
    
    lower <- input$mu-3*sd
    upper <- input$mu+3*sd
    x.breaks <- round(seq(input$mu-3*sd,input$mu+3*sd,sd))
    
    
    # put data into 60 bins
    sampleMeans <- values$total[-1]
    if (length(sampleMeans)>0) {
      df <- data.frame(x=sampleMeans)
      if (length(sampleMeans == 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_point(size=0.3) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) + 
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      }
      
      if (length(sampleMeans > 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_dotplot(dotsize=0.3) +
          #coord_cartesian(ylim=c(0,10),expand=T) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      } 
      
      # overlay normal distribution if required, and if there
      #  are enough samples (>= 100)
      if (length(sampleMeans) > 100){
        bin.width <- 3
        # show histogram
        p <- ggplot(df, aes(x = x)) +
          geom_histogram(binwidth = bin.width) +
          scale_x_continuous(limits=c(lower,upper),
                             breaks = x.breaks,minor_breaks=NULL) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
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
            args = c(mean = input$mu, sd = s, 
                     n = counter$countervalue , 
                     bw = bin.width))
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
    points <- data.frame(x = samp() )
    sum <- 0
    if (dim( points )[1] > 0 ) {
      sum <- sum(points$x)
    }
    count <- counter$countervalue
    str0 <- paste('This is sample number: ',count,sep=' ')
    str0b <- "The black dots indicate the sample."
    str1 <- paste('The total of the sample is ',sum,sep=': ')
    
    xbar <- round(1000* sum / as.numeric(input$n))/1000
    str2 <- paste('The average of the sample is ',sum,
                  'divided by ',as.numeric(input$n),'= ',xbar,sep=' ')
    result <- paste(str0,'<br>',str0b,'<br>',str1,'<br>',str2)
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
    str1 <- paste('The mean of all sample means is ',m.hat,sep=': ')
    str2 <- paste('The standard deviation of all sample means is ',ss,sep=': ')
    result <- paste(str0,'<br>',str1,'<br>',str2)
    
    return(result)
  }
  
  getOneSampleSummary <- function() {
    return("Each dot indicates a sample mean.")
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