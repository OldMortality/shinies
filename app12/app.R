# app 12.
# height vs finger length
library(shinydashboard)
library(shinyjs)
library(ggplot2)


ui <- dashboardPage(
  
  
  
  dashboardHeader(title = "Height vs finger length",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample"), 
                   radioButtons("n", "Sample size:",
                    c("10" = 10,
                    "50" = 50,
                    "100"= 100)),
                   checkboxInput("showalllines", "Show all lines", FALSE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 12,
             box( 
               title="", 
               width=NULL,
               plotOutput("thePlot",height=600), 
               height = 600)
            )
    )
  )
)


server <- function(input, output) {
  
  # the sample
  samp <- reactiveVal()
  
  f <- read.csv('combined_clean.csv',header=T)
  
  f <- f[-which(f$heightmm < 1000),]
  f <- f[-which(f$heightmm > 2500),]
  f <- f[-which(is.na(f$heightmm)),]
  f <- f[-which(f$indexfingerlengthmm<20),]
  
  f$hei <- f$heightmm
  f$fin <- f$indexfingerlengthmm
  
  m <- lm(hei~fin,data=f)
  a0 <- coefficients(m)[[1]]
  a1 <- coefficients(m)[[2]]
  
  alla0 <- reactiveValues(a0s=vector())
  alla1 <- reactiveValues(a1s=vector())
  
  
   
  
  observeEvent(input$clear,{
    
    a0 <- coefficients(m)[[1]]
    a1 <- coefficients(m)[[2]]
    alla0$a0s <- vector()
    alla1$a1s <- vector()
    # clear samp
    samp <- NULL
    samp(samp)
    
    
    
    
  })
  
 # observeEvent(input$clear,{
    #thisSampleMean <- 0
    #allm <- vector()
    #samp <- round(rnorm(0,mean=mu,sd=sd),1)
    #samp(samp)
    #meansamp <- reactiveVal()
    ##allmeansamp$allm = vector()
    #values$total = 0
    #counter$countervalue = 0
    #autorun$auto = 0
    #samp(samp)
  #})
  
   
  
  # 1 sample
  observeEvent(input$sample,{
    
    
    s <- f[sample(nrow(f),size=input$n),]
    
    
    m2 <- lm(hei~fin,data=s)
    alla0$a0s <- c(alla0$a0s,coefficients(m2)[[1]])
    alla1$a1s <- c(alla1$a1s,coefficients(m2)[[2]])
    
    # store sample to show in plot
    samp <- s
    # no idea what this does
    samp(samp)
    
  })  
  
  
  output$thePlot <- renderPlot({
    
     
    plot.data <- data.frame(x = f$fin, y=f$hei)
    #alla0$a0s <- c(alla0$a0s) <- coefficients(m2)[1]
    p <- ggplot(data=plot.data,aes(x=x,y=y)) + geom_point() +
      scale_x_continuous(limits=c(50,100)) +
      scale_y_continuous(limits=c(1300,2100)) +
      ylab('height (mm)') +
      xlab('finger length (mm)') 
    
    if (!is.null(samp()) ) {
      p <- p +
       geom_point(data=samp(),aes(x=fin,y=hei),colour='red')
    }
    col <- 'red'
     
    if (input$showalllines) {
      #print(length(alla1$a1s))
      for (i in 1:length(alla1$a1s)) {
        p <- p + geom_abline(intercept=alla0$a0s[i],slope=alla1$a1s[i],colour=col)
      
      }
    } else {
      i <- length(alla1$a1s)
      p <- p + geom_abline(intercept=alla0$a0s[i],slope=alla1$a1s[i],colour=col)
    }
    
    p
    
  }) # end thePlot
  
  
  
}

shinyApp(ui, server)