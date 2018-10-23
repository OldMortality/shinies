# app 11.
# 
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
#library(shinyBS)


ui <- dashboardPage(
  
  dashboardHeader(title = "Comparing 2 groups: Conduction velocity",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: blue}")),
                   
                   actionButton("clear",label="Clear"),
                   actionButton("sampleMany",label="Take 200 samples"),
                   sliderInput("mu.red", "Red mean:",
                               min = 10, max = 80, value = 55,step=0.25
                   ),
                   #bsTooltip(id = "sampleMany", title = "This is an input", 
                   #           placement = "top", trigger = "hover"),
                   
                   sliderInput("mu.2", "Blue mean:",
                               min = 10, max = 80, value = 40,step=0.25
                   ),
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100)),
                   textInput("observed", label = "observed difference", value = "21"),
                   checkboxInput("show.perc", "Show percentile", FALSE),
                   checkboxInput("show.mean", "Show mean", FALSE),
                   checkboxInput("show.norm", "Show Normal", FALSE),
                   checkboxInput("show.obs", "Show observed difference", FALSE)
                   
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
               width=NULL,
               plotOutput("difference",height=50),
               height = 75),
             box(title="Red sample mean minus blue sample mean",  
                 width=NULL,
                 plotOutput("samplemean",height=200), 
                 height = 250)
      ), 
      column(width=6, 
             box(  
               title="", 
               width=NULL,
               htmlOutput('sampleSummary',height=200), 
               height = 250),
             box( 
               title=htmlOutput('onesamplesummary',height=50), 
               width=NULL,
               
               height = 75),
             box( 
               title=htmlOutput('differencesummary',height=50), 
               width=NULL,
               
               height = 75),
             box( 
               width=NULL,
               title="Summary", 
               htmlOutput('sampleMeanSummary',height=200), 
               height = 250)
      )
    )
  )
  
  
)


server <- function(input, output) {
  
  
  #addTooltip(session, id = "mu.red", title = "You can use arrow keys",
  #                      placement = "left", trigger = "hover")
  #addTooltip(session, id = "mu.2", title = "You can use arrow keys",
  #           placement = "left", trigger = "hover")
  
  # from combined clean csv
  
  mu2 = 35
  sd1 = 13
  sd2 = 13
  sd = sd1
  
  # temp:
  # replace mu1 by input$mu.red
  mu = 50
  sd = sd1
  lower <- mu-3*sd
  upper <- mu+3*sd
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
  
  observeEvent(input$mu.2, {
    click("clear")
    invalidateLater(1)
  })
  
  
  # 1 sample
  observeEvent(input$sample,{
    showMean <<- 1
    showDiff$summary <- 1
    showSample <<- TRUE
    samp1 <- round(rnorm(as.numeric(input$n),mean=input$mu.red,sd=sd1),1)
    samp2 <- round(rnorm(as.numeric(input$n),mean=input$mu.2,sd=sd2),1)
    
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
      samp1 <- round(rnorm(as.numeric(input$n),mean=input$mu.red,sd=sd1),1)
      samp2 <- round(rnorm(as.numeric(input$n),mean=input$mu.2,sd=sd2),1)
      
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
  observeEvent(input$sampleMany,{
    showDiff$summary <- 0 
    showMean <<- 100
    showSample <<- FALSE
    
    for (i in 1:200) {
      
      samp1 <- round(rnorm(as.numeric(input$n),mean=input$mu.red,sd=sd1),1)
      samp2 <- round(rnorm(as.numeric(input$n),mean=input$mu.2,sd=sd2),1)
      
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
  observeEvent(input$sampleMany, {
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
  
  
  sd <- sd1
  upp <- mu + 3 * sd
  low <- mu - 3 * sd
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
  #y   <- dnorm(x,mean=mu, sd=sd)
  #jitter_y <- max(y)/50
  
  output$CLTplot1 <- renderPlot({
    
    
   
    # red
    data2.label1 <- data.frame(
      time = c(input$mu.red), 
      value = c(0.001), 
      label = c("not GM")
    )
    
    # blue
    data2.label2 <- data.frame(
      time = c(input$mu.2), 
      value = c(0.003), 
      label = c("GM")
    )
    
    
    
      
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = input$mu.red, sd = sd1)) + 
      stat_function(fun = dnorm, show.legend=F,
                    colour='blue', 
                    args = list(mean = input$mu.2, sd = sd2)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      xlab("Conduction velocity") + 
      geom_vline(xintercept=input$mu.red,colour='red') +
      geom_vline(xintercept=input$mu.2,colour='blue') +
      geom_text(data = data2.label1, aes(x = time, y = value, label = label),colour='red') +
      geom_text(data = data2.label2, aes(x = time, y = value, label = label),colour='blue') 
    
    
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
      df1$y =0.005
      df2 <- data.frame(x=thisOne2,col='blue')
      df2$y =0.00
      
      df <- rbind(df1,df2)
      p <- ggplot(df, aes(x = x,y=y)) +
        geom_point(colour = df$col,alpha=0.5 ) +
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
  # This is the 2nd strip, with the difference
  # 
  output$difference <- renderPlot({
    low <- -30
    upp <- 30
    x.breaks <- seq(low,upp,15)
    if (length(samp1())>0) {
      thisOne1 <- tail(values$total1,showMean)
      thisOne2 <- tail(values$total2,showMean)
      diff <- thisOne1 - thisOne2
      df <- data.frame(x=diff)
      p <- ggplot(df, aes(x = x,y=0,colour='black' )) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01)) + 
        ylab("") + 
        xlab("Red sample mean minus blue sample mean") +
        geom_point(colour="black") 
      
      p
    }
     
  })
  
  
  #
  # This is the tricky plot, with the histogram
  #   of all sample means.
  #
  output$samplemean <- renderPlot({
    
    lo <- -30
    up <- 30
    bin.width = 3
    x.breaks <- seq(lo,up,bin.width)
    
    sampleMeans <- values$diff[-1]
    if (length(sampleMeans)>0) {
      df <- data.frame(x=sampleMeans)
      if (length(sampleMeans == 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_point(size=0.3) +
          scale_x_continuous(limits=c(lo,up),
                             breaks = x.breaks,minor_breaks=NULL) + 
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      }
      
      #if (length(sampleMeans > 1)) {
      #  p <- ggplot(df, aes(x = x)) +
      #    geom_dotplot(dotsize=0.3) +
      #    
      #    scale_x_continuous(limits=c(lo,up),
      #                       breaks = x.breaks,minor_breaks=NULL) +
      #    scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      #} 
      
       
      #if (length(sampleMeans) > 100){
        
        # show histogram
        p <- ggplot(df, aes(x = x)) +
          geom_histogram(binwidth = bin.width) +
          scale_x_continuous(limits=c(lo,up),
                             breaks = x.breaks,minor_breaks=NULL) #+
                             #scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
        
      #}
      
      
      p <- p + xlab("") + ylab("")
      
      if (input$show.obs) {
        p <- p + geom_vline(xintercept=as.numeric(input$observed))
      }
      
      if (input$show.mean) {
        m <- mean(sampleMeans)
        p <- p + geom_vline(xintercept = m,colour='red' )
      }
      
      if (input$show.norm) {
        
        m <- input$mu.red - input$mu.2
        s.size <- as.numeric(input$n)
        s <- sqrt(sd1^2/s.size+sd2^2/s.size)
        p <- p + stat_function( 
          color="red",
          fun = function(x, mean, sd, n, bw){ 
            dnorm(x = x, mean = mean, sd = sd) * n * bw
          }, 
          args = c(mean = m, sd = s ,
                   n = length(sampleMeans) , 
                   bw = bin.width))
      }
      
      if (input$show.perc) {
        two.5 <- round(quantile(sampleMeans,0.025),1)
        nine.5 <- round(quantile(sampleMeans,0.975),1)
        p <- p + geom_vline(xintercept = two.5,colour='darkred' )+
          geom_vline(xintercept = nine.5,colour='darkred' )
      }
      
      
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
    str0b <- "The blue dots indicates the sample from the blue population."
    str0c <- "The red dots indicates the sample from the red population."
    
    
    str1 <- paste('The total of the blue sample is ',sum2,sep=': ')
    str1b <- paste('The total of the red sample is ',sum1,sep=': ')
    
    xbar1 <- round(1000* sum1 / as.numeric(input$n))/1000
    xbar2 <- round(1000* sum2 / as.numeric(input$n))/1000
    str2b <- paste('The average of the red sample is ',sum1,
                   'divided by ',as.numeric(input$n),'= ',xbar1,sep=' ')
    str2 <- paste('The average of the blue sample is ',sum2,
                  'divided by ',as.numeric(input$n),'= ',xbar2,sep=' ')
    
    result <- paste(str0,'<br>',str0b,'<br>',str0c,'<br>',
                    str1,'<br>',str1b,'<br>',
                    str2,'<br>',str2b,'<br>')
    if (!showSample) {
      result <- ""
    }
    if (length(points$x1) < 1) {
      result <- ""
    }
    return(result)
  }
  
  getSampleMeansSummary <- function() {
    # the vector of sample means
    sampleMeans <- values$diff[-1]
    if (length(sampleMeans > 0)) {
      
      m.hat <- round(100* mean(sampleMeans))/100
      ss <- round(100* sqrt(var(sampleMeans)))/100
      count <- counter$countervalue
      str0 <- paste('We now have this many samples: ',count,sep=' ')
      #str1 <- paste('The mean of all sample means is ',m.hat,sep=': ')
      if (length(sampleMeans) <= 1) {
        str2 <- ''  
      } else {
        #str2a <- paste('The standard deviation of all sample means is ',ss,sep=': ')
        sampleMeans <- values$diff[-1]
        
        obs <- as.numeric(input$observed)
        if (input$mu.red - input$mu.2 < obs) {
          # counting the right tail
          w <- which(sampleMeans>obs)
        } else {
          # counting the left tail
          w <- which(sampleMeans<obs)
        }
        
        #print(sampleMeans[w])
        numMoreExtremeThanObserved <- length(w)
        str2_a <- paste("The observed difference is",input$observed,
                        sep=': ')
        str2_b <- paste("The number of samples with such a difference,
                       or more extreme is:",
                       numMoreExtremeThanObserved,sep=' ')
        str2_c <- paste("There were",length(sampleMeans),"samples",sep=" ")
        m1 <- round(mean(sampleMeans,na.rm = T),1)
        str_e <- paste("The mean of the sampling distribution is: ",m1,sep=" ")
        two.5 <- round(quantile(sampleMeans,0.025),1)
        nine.5 <- round(quantile(sampleMeans,0.975),1)
        str_f <- paste("The 2.5% percentile of the sampling distribution is:",
                       two.5)
        str_g <- paste("The 97.5% percentile of the sampling distribution is:",
                       nine.5)
        
        
        perc <- 100*round(numMoreExtremeThanObserved/length(sampleMeans),2)
        str2_c1 <- paste("If the population means (red and blue) have the correct distance, ")
        str2_d <- paste(str2_c1, "then the probability of the observed difference (or more extreme) is", 
                        perc,"percent",sep=" ")
        str2 <- paste(str2_a, str2_b,str2_c,
                      str_e,str_f,str_g,
                      str2_d,sep="<br>")
      }
      result <- paste(str2)
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

shinyApp(ui, server)