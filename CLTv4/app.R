# app 5.
# CLT with dashboard
#   the old one 
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


ui <- dashboardPage(
  
  dashboardHeader(title = "The distribution of the sample mean",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   actionButton("sample",label="Take Sample"),
                   actionButton("start",label="Start "),
                   actionButton("stop",label="Stop "),  
                   
                   radioButtons("n", "Sample size:",
                                c("10" = 10,
                                  "50" = 50,
                                  "100"= 100)),
                   checkboxInput("shownormal", "Show Normal", TRUE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 6,
             box(
               title="Population", width=NULL,
               plotOutput("CLTplot1", height = 250)),
             box(
               title="This sample mean", width=NULL,
               plotOutput("thissamplemean", height = 125)),
             box(title="Means of all samples", 
                 width=NULL,
                 plotOutput("samplemean", height = 250))
      ), 
      column(width=6, 
             box( 
               title="One sample", 
               width=NULL,
               htmlOutput('sampleSummary')),
              box(
               width=NULL,
               title="All samples", 
               htmlOutput('sampleMeanSummary'))
      )
    )
  )
)


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  mu <- 0
  SS <- 1 
  lower <- mu-3*SS
  upper <- mu+3*SS
  thisSampleMean <- 0
  
  observeEvent(input$sample,{
    samp <- round(rnorm(as.numeric(input$n),mean=mu,sd=SS),1)
    samp(samp)
    meansamp <- round(mean(samp),2)
    thisSampleMean <<- meansamp
    meansamp(meansamp) 
    values$total <- c(values$total,meansamp) 
  })
  
  observeEvent(input$sample, {
    counter$countervalue <- counter$countervalue + 1
  })
  
  observeEvent(input$start, {
    autorun$auto <<- 1
  })
  
  observeEvent(input$stop, {
    autorun$auto <<- 0
  })
  
  # click sample every 100 ms
  observe({
    if (autorun$auto == 1) {
      click("sample")
      invalidateLater(50)
    }
  })
  
  allm <- vector()
  samp <- reactiveVal()
  meansamp <- reactiveVal()
  allmeansamp <- reactiveValues(allm=vector())
  values <- reactiveValues(total = 0)
  counter <- reactiveValues(countervalue = 0)
  autorun <- reactiveValues(auto = 0)
  
  
  
  
  output$sampleCounter <- renderInfoBox({
    infoBox(
      "Samples: ", paste0(counter$counterValues), icon = icon("list"),
      color = "purple"
    )
  })
  
  output$CLTplot1 <- renderPlot({
    
    
    x   <- seq(mu-3*SS,mu+3*SS,length=1000)
    y   <- dnorm(x,mean=mu, sd=SS)
    z   <- rnorm(length(x),mu,SS)
    df <- data.frame(x=x,y=y,z=z)
    jitter_y <- max(y)/50
    
    p <-  ggplot(df, aes(x = x, y = y))  + geom_point() +
      scale_x_continuous(limits=c(lower,upper)) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      theme(legend.position = "none") 
    if (length(samp())>0) {
      # points for the sample
      points <- data.frame(x = samp(),y=rep(0,length(samp)))
      p <- p + geom_jitter(data=points,aes(colour = 'red'),width=0,
                           height=jitter_y)
    }
    p
  }) # end CLTplot1
  
  output$thissamplemean <- renderPlot({
    
    
    if (length(samp())>0) {
      thisOne <- mean(samp()) 
      df <- data.frame(x=thisOne)
      p <- ggplot(df, aes(x = x,y=0 )) +
        geom_point(size=5) +
        theme(legend.position = "none") +
        scale_x_continuous(limits=c(lower,upper)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                           limits=c(-0.01,0.01))
      p
    }
    #max(table(sampleMeans))+1))
  })
  
  output$samplemean <- renderPlot({
    # put data into 60 bins
    sampleMeans <- values$total[-1]
    if (length(sampleMeans)>0) {
      df <- data.frame(x=sampleMeans)
      if (length(sampleMeans == 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_point(size=5) +
          scale_x_continuous(limits=c(lower,upper)) + 
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      }
    
      if (length(sampleMeans > 1)) {
        p <- ggplot(df, aes(x = x)) +
          geom_dotplot() +
          scale_x_continuous(limits=c(lower,upper)) +
          scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
      } 
      p
    }
    
    
  })
   
  output$CLTplot3 <- renderPlot({
    
    # work out the sample mean for the vertical line only
    points <- data.frame(x = samp() )
    sum <- 0
    if (dim( points )[1] > 0 ) {
      sum <- sum(points$x)
    }
    xbar <- round(1000* sum / as.numeric(input$n))/1000
    # xbar is the sample mean
    
    df <- data.frame(x = values$total[-1])
    sampleMeans <- values$total[-1]
    m.hat <- round(100* mean(sampleMeans))/100
    ss <- round(100* var(sampleMeans))/100
    label1 <- paste('mean: ',m.hat,sep=" " )
    label2 <- paste('variance: ',ss,sep=" " )
    if (length(values$total) > 1) {
      
      p <- ggplot(df,aes(x=x)) + geom_blank() + 
        geom_histogram(aes(x,stat(density)),
                       color="black",
                       binwidth=0.1) +
        geom_vline(xintercept=xbar, colour="red") +
        annotate("text", label = xbar, x = xbar, y = 0.3, size = 4, colour = "red") 
      if (input$shownormal) {
        p <- p +
          stat_function(fun=dnorm,
                        color="red",
                        args=list(mean=mean(m.hat), 
                                  sd=sqrt(ss))) 
      }
      # show the plot
      p
    }
  } ) # end CLTplot3
  
  output$sampleTable <- renderDataTable(data.frame(samp()),
                                        options = list(
                                          pageLength = 5,
                                          initComplete = I("function(settings, json) {;}")
                                        )
  )
  
  getSampleSummary <- function() {
    points <- data.frame(x = samp() )
    sum <- 0
    if (dim( points )[1] > 0 ) {
      sum <- sum(points$x)
    }
    count <- counter$countervalue
    str0 <- paste('This is sample number: ',count,sep=' ')
    str1 <- paste('The total of the sample is ',sum,sep=': ')
    
    xbar <- round(1000* sum / as.numeric(input$n))/1000
    str2 <- paste('The average of the sample is ',sum,
                  'divided by ',as.numeric(input$n),'= ',xbar,sep=' ')
    result <- paste(str0,'<br>',str1,'<br>',str2)
    
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
  
  output$sampleSummary <- renderText(
    getSampleSummary()
  )
  
  output$sampleMeanSummary <- renderText(
    getSampleMeansSummary()
  )
  
}

shinyApp(ui, server)