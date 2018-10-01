# app 6.
# intervals
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
                   actionButton("sample10",label="Take 10 samples"),
                   actionButton("sample100",label="Take 100 samples"),
                   actionButton("start",label="Start "),
                   actionButton("stop",label="Stop "),  
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100)),
                   checkboxInput("showtruemean", "Show true mean", TRUE),
                   checkboxInput("showerrs", "Color errors", TRUE)
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
             box(title="Means of all samples",  
                 width=NULL,
                 plotOutput("samplemean",height=200), 
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
    
    #s <- sqrt(var(samp()))
    s <- sd
    lo <- meansamp - 1.96 * s/sqrt(as.numeric(input$n))
    up <- meansamp + 1.96 * s/sqrt(as.numeric(input$n))
    all_low$all_l <- c(all_low$all_l,lo)
    all_upp$all_u <- c(all_upp$all_u,up)
    
  })
  
  
  
  observeEvent(input$sample10,{
    for (i in 1:10) {
      samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
      samp(samp)
      meansamp <- round(mean(samp),2)
      thisSampleMean <<- meansamp
      meansamp(meansamp) 
      values$total <- c(values$total,meansamp) 
      
      #s <- sqrt(var(samp()))
      s <- sd
      lo <- meansamp - 1.96 * s/sqrt(as.numeric(input$n))
      up <- meansamp + 1.96 * s/sqrt(as.numeric(input$n))
      all_low$all_l <- c(all_low$all_l,lo)
      all_upp$all_u <- c(all_upp$all_u,up)
    }
  })
  
  observeEvent(input$sample100,{
    for (i in 1:100) {
      samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=sd),1)
      samp(samp)
      meansamp <- round(mean(samp),2)
      thisSampleMean <<- meansamp
      meansamp(meansamp) 
      values$total <- c(values$total,meansamp) 
      
      s <- sqrt(var(samp()))
      lo <- meansamp - 2 * s/sqrt(as.numeric(input$n))
      up <- meansamp + 2 * s/sqrt(as.numeric(input$n))
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
    
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
    ) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = mu, sd = sd)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") +
      xlab("Height")
    
    
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
      s <- sqrt(var(samp()))
      lo <- thisOne - 2 * s/sqrt(as.numeric(input$n))
      up <- thisOne + 2 * s/sqrt(as.numeric(input$n))
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
  
  #
  # This is the tricky plot, with all the intervals
  #   one for each sample
  #
  output$samplemean <- renderPlot({
    
    # not sure what this does
    # samp()
    
    df <- data.frame(x=mu,y=0)
    if (length(all_low$all_l)>0) {
      p <- ggplot(df, aes(x = x,y=0 ),colour='green') +
      theme(legend.position = "none") +
        scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,
                           limits=c(low,upp)) +
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(0,100)) + 
        ylab("") + 
        xlab("Sample mean") 
      for (i in 1:length(all_low$all_l)) { 
        lo <- all_low$all_l[i]
        up <- all_upp$all_u[i]
        df <- data.frame(x=lo,y=up)
         
        intervalCol = 'black'
        if (input$showerrs) {
          if ((lo <= mu) & (up>=mu)) {
            # do nothing
          } else {
            intervalCol = 'red'
          }
        }
        p <- p +
          geom_segment(x=lo,y=i,xend=up,yend=i,
                       colour=intervalCol) #+
          #geom_point(x=mean(samp(),y=i)) 
      
      }
      
      if (input$showtruemean) {
        p <- p + geom_vline(xintercept = mu,col='red')
      }
      
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
    return("The dot indicates the sample mean.")
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