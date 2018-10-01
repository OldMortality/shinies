# app 6b.
# show CI
library(shinydashboard)
library(shinyjs)
library(ggplot2)
#library(DT)


ui <- dashboardPage(
  
  dashboardHeader(title = "Confidence intervals for the mean",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample"),
                   sliderInput("mu.2", "Blue mean:",
                               min = 1400, max = 2000, value = 1740,step=1
                   ),
                   
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100))#,
                   #checkboxInput("showtruemean", "Show true mean", TRUE),
                   #checkboxInput("showerrs", "Color errors", TRUE)
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box( 
               title="Population", 
               width=NULL,
               plotOutput("CLTplot1",height=200), 
               height = 250),
             box( 
               width=NULL,
               plotOutput("thissamplemean",height=50),
               height = 75),
             box( 
               title="Sampling distribution", 
               width=NULL,
               plotOutput("samplingdistribution",height=200),
               height = 250) 
      ), 
      column(width=6, 
             box(  
               title="One sample", 
               width=NULL,
               htmlOutput('sampleSummary',height=200), 
               height = 250),
             box( 
               title=htmlOutput('onesamplesummary',height=50), 
               width=NULL,
               
               height = 75),
             box( 
               width=NULL,
               title="All samples", 
               htmlOutput('sampleMeanSummary',height=200), 
               height = 250)
      )
    )
  )
)


server <- function(input, output) {
  
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
  
  observeEvent(input$clear,{
    thisSampleMean <- 0
    allm <- vector()
    samp <- round(rnorm(0,mean=mu,sd=sd),1)
    #samp(samp)
    meansamp <- reactiveVal()
    allmeansamp$allm = vector()
    values$total = 0
    counter$countervalue = 0
    autorun$auto = 0
    # ??
    all_low$all_l=vector()
    all_upp$all_u=vector()
    # ??
    samp(samp)
  })
  
  
  observeEvent(input$sample,{
    samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
    samp(samp)
    meansamp <- round(mean(samp),2)
    thisSampleMean <<- meansamp
    meansamp(meansamp) 
    values$total <- c(values$total,meansamp) 
    
    s <- sqrt(var(samp()))
    lo <- meansamp - 1.96 * s/sqrt(as.numeric(input$n))
    up <- meansamp + 1.96 * s/sqrt(as.numeric(input$n))
    all_low$all_l <- c(all_low$all_l,lo)
    all_upp$all_u <- c(all_upp$all_u,up)
    
  })
  
  
  
 
  
  
  observeEvent(input$sample, {
    counter$countervalue <- counter$countervalue + 1
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
    
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
    ) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='blue', 
                    args = list(mean = input$mu.2, sd = sd)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      xlab("Height") +
      geom_vline(xintercept = mu,col='red')
    
    
    if (length(samp())>0) {
      # points for the sample
      pts <- data.frame(x = samp(),y=rep(0,length(samp))) 
      p <- p + geom_point(data=pts,aes(y=y),
                          colour='black')
    }
    p
  }) # end CLTplot1
  
  #
  # This is the strip, with 1 dot and interval for this sample mean
  # 
  output$thissamplemean <- renderPlot({
    
    
    if (length(samp())>0) {
      thisOne <- mean(samp()) 
      
      
      
      #s <- sqrt(var(samp()))
      s <- sd
      lo <- thisOne - 1.96 * s/sqrt(as.numeric(input$n))
      up <- thisOne + 1.96 * s/sqrt(as.numeric(input$n))
      
     
      
      df <- data.frame(x=thisOne)
      p <- ggplot(df, aes(x = x,y=0 )) +
        geom_point(colour='black') +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Sample mean") +
        geom_segment(aes(x=lo,y=0,xend=up,yend=0))
      p
    }
    #max(table(sampleMeans))+1))
  })
  
  #scale_x_continuous(limits=c(low,upp)) + 
  
  # sampling distribution, if the blue distribution
  #    were true.
  output$samplingdistribution <- renderPlot({
    
     
    if (length(samp())>0) {
       
      # sd of the sampling distribution
      sd.samp <- sd/sqrt(as.numeric(input$n))
       
      print(sd.samp)
      
      thisOne <- mean(samp())
      print(thisOne)
      # for plotting shady bit
      if (input$mu.2 > mu ) {
        x.low <- low
        x.upp <- thisOne
      } else {
        x.low <- thisOne
        x.upp <- upp
      }
      
      # for plotting shaded area
      df.norm.x <- data.frame(x=seq(x.low,x.upp,0.1))
      df.norm.y <- data.frame(y=dnorm(df.norm.x$x,mean=input$mu.2,sd=sd.samp))
      df.norm <- cbind(df.norm.x,df.norm.y)
      
      p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
      ) + 
        geom_point(aes(x=thisOne,y=0)) + 
        stat_function(fun = dnorm, show.legend=F,
                    colour='blue', 
                    args = list(mean = input$mu.2, sd = sd.samp)) + 
        ylab("") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        theme(legend.position = "none") +
        xlab("Height") +
         geom_ribbon(data=df.norm,aes(x=df.norm$x,ymin=0,ymax=df.norm$y),
                  fill='red',alpha=0.1) 
       
    
    
      print(p)
    print('13')
    }
  })
  
  
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
    
    return(result)
  }
  
  getSampleMeansSummary <- function() {
     
    
    result <- ""
    if (length(samp() > 1) ) {
      sampleMean <- mean(samp())
      sd.samp.dist <- sd/sqrt(as.numeric(input$n))
      print(paste('sd.samp.dist',sd.samp.dist))
      prob <- pnorm(sampleMean,mean=input$mu.2,sd=sd.samp.dist)
      if (input$mu.2 < mu) {
        prob <- 1-  prob
      }
      prob <- round(prob,3)
      str0 <- paste("Suppose the true value of the population mean is",
                    input$mu.2,sep=" ")
      str1 <- paste("The observed sample mean is",sampleMean,sep=' ')
      str2 <- paste("The probability of seeing the observed sample mean is",prob)
      result <- paste(str0,str1,str2,sep="<br>")
    }
    return(result)
    
    
  }
  
  getOneSampleSummary <- function() {
    
    result <- ""
    if (length(samp()>0)) {
      thisOne <- round(mean(samp()),0)
      s <- sd
      lo <- thisOne - 1.96 * s/sqrt(as.numeric(input$n))
      up <- thisOne + 1.96 * s/sqrt(as.numeric(input$n))
      lo <- round(lo,0)
      up <- round(up,0)
      str1 <- "The dot indicates the sample mean."
      str2 <- paste("The sample mean is",thisOne)
      str3 <- paste("The 95% Confidence interval runs from",
                    lo,'to',up,sep=" ")
      result <- paste(str1,str2,str3,sep="<br>")
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
  
}

shinyApp(ui, server)