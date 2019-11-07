# app 6.
# intervals
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)



shinyServer <- function(input, output) {
  
  mu <- 1711
  sd <- 92 
  lower <- mu-3*sd
  upper <- mu+3*sd
  thisSampleMean <- 0
  
  
  #allm <- vector()
  samp <- reactiveVal()
  meansamp <- reactiveVal()
  allmeansamp <- reactiveValues(allm=vector())
  all_low <- reactiveValues(all_l=vector())
  all_upp <- reactiveValues(all_u=vector())
  values <- reactiveValues(total = 0)
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
  
  
  
  observeEvent(input$clear,{
    thisSampleMean <- 0
    allm <- vector()
    samp <- round(rnorm(0,mean=mu,sd=sd),1)
    #samp(samp)
    meansamp <- reactiveVal()
    allmeansamp$allm = vector()
    values$total = 0
    counter$countervalue = 0
    countReds$counter = 0
    autorun$auto = 0
    # 
    all_low$all_l=vector()
    all_upp$all_u=vector()
    # 
    samp(samp)
    #
    # enable sample buttons
    shinyjs::enable("sample") 
    shinyjs::enable("sample10")
    shinyjs::enable("sample100") 
    shinyjs::enable("start") 
  })
  
  
  observeEvent(input$sample,{
    samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
    samp(samp)
    meansamp <- round(mean(samp),2)
    thisSampleMean <- meansamp
    meansamp(meansamp) 
    values$total <- c(values$total,meansamp) 
    #s <- sd
    #lo <- meansamp - 1.96 * s/sqrt(as.numeric(input$n))
    #up <- meansamp + 1.96 * s/sqrt(as.numeric(input$n))
    
    s <- sqrt(var(samp))
    lo <- meansamp + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
    up <- meansamp + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
    if (! ((mu>lo) & (mu<up)) ) {
      countReds$counter <- countReds$counter + 1
    }
    all_low$all_l <- c(all_low$all_l,lo)
    all_upp$all_u <- c(all_upp$all_u,up)
  })
  
  
  observeEvent(input$sample10,{
    for (i in 1:10) {
      samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
      samp(samp)
      meansamp <- round(mean(samp),2)
      thisSampleMean <- meansamp
      meansamp(meansamp) 
      values$total <- c(values$total,meansamp) 
      s <- sqrt(var(samp))
      lo <- meansamp + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      up <- meansamp + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      if (! ((mu>lo) & (mu<up)) ) {
        countReds$counter <- countReds$counter + 1
      }
      #up <- meansamp + 1.96 * s/sqrt(as.numeric(input$n))
      all_low$all_l <- c(all_low$all_l,lo)
      all_upp$all_u <- c(all_upp$all_u,up)
    }
  })
  
  observeEvent(input$sample100,{
    for (i in 1:100) {
      samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
      samp(samp)
      meansamp <- round(mean(samp),2)
      thisSampleMean <- meansamp
      meansamp(meansamp) 
      values$total <- c(values$total,meansamp) 
      s <- sqrt(var(samp))
      lo <- meansamp + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      up <- meansamp + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      if (! ((mu>lo) & (mu<up)) ) {
        countReds$counter <- countReds$counter + 1
      }
      all_low$all_l <- c(all_low$all_l,lo)
      all_upp$all_u <- c(all_upp$all_u,up)
      
    }
  })
  
  
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
  max.samples <- 125
  observe({
    if (counter$countervalue > max.samples) {
      # disable sample buttons
      shinyjs::disable("sample") 
      shinyjs::disable("sample10")
      shinyjs::disable("sample100") 
      shinyjs::disable("start") 
      
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
    
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
    ) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = mu, sd = sd)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      xlab("Height (mm)")
    
    
    if (length(samp())>0) {
      # points for the sample
      pts <- data.frame(x = samp(),y=rep(0,length(samp))) 
      p <- p + geom_point(data=pts,aes(y=y),
                          colour='black')
    }
    p
  }) # end plot1
  
  #
  # This is the strip, with 1 dot and interval for this sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    if (length(samp())>0) {
      thisOne <- mean(samp()) 
      #s <- sqrt(var(samp()))
      #lo <- thisOne - 2 * s/sqrt(as.numeric(input$n))
      #up <- thisOne + 2 * s/sqrt(as.numeric(input$n))
      s <- sqrt(var(samp()))
      lo <- thisOne + qt(0.025,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      up <- thisOne + qt(0.975,as.numeric(input$n)-1) * s/sqrt(as.numeric(input$n))
      
      df <- data.frame(x=thisOne)
      p <- ggplot(df, aes(x = x,y=0 )) +
        geom_point(colour='blue') +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Confidence interval") +
        geom_segment(aes(x=lo,y=0,xend=up,yend=0),colour="blue")
      p
    }
     
  })
  
 
  #
  # This is the tricky plot, with all the intervals
  #   one for each sample
  #
  output$samplemean <- renderPlot({ 
     
    df <- data.frame(x=mu,y=0)
    if (length(all_low$all_l)>0) {
      p <- ggplot(df, aes(x = x,y=0 ),colour='green') +
      theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,
                           limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(0,125)) + 
        ylab("") + 
        xlab("Sample mean") 
      for (i in 1:length(all_low$all_l)) { 
        lo <- all_low$all_l[i]
        up <- all_upp$all_u[i]
        df <- data.frame(x=lo,y=up)
        
        # make interval red if mu is not in it 
        intervalCol = 'blue'
        if ((lo <= mu) & (up>=mu)) {
          # do nothing
        } else {
          intervalCol = 'red'
        }
        p <- p +
          geom_segment(x=lo,y=i,xend=up,yend=i,
                       colour=intervalCol)   
      }
      
      # plot the true mean
      p <- p + geom_vline(xintercept = mu,col='red')
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
    str1 <- paste("The mean for sample",count,"is",xbar,sep=' ')
    result <- paste(str0,'<br>',str1,'<br>') 
    return(result)
  }
  
  getSampleMeansSummary <- function() {
    # the vector of sample means
    #df <- data.frame(x = values$total[-1])
    #print(paste('.....',values$total))
    sampleMeans <- values$total[-1]
    
    #m.hat <- round(100* mean(sampleMeans))/100
    #ss <- round(100* sqrt(var(sampleMeans)))/100
    count <- counter$countervalue
    #str0 <- paste('We now have this many samples: ',count,sep=' ')
    #str1 <- paste('The mean of all sample means is ',m.hat,sep=': ')
    #str2 <- paste('The standard deviation of all sample means is ',ss,sep=': ')
    #result <- paste(str0,'<br>',str1,'<br>',str2)
    width.bar <- round(mean(all_upp$all_u - all_low$all_l))
    
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
