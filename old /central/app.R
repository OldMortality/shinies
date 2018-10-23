# This is the CLT app, without dashboard.

library(shiny)
library(shinyjs)
library(ggplot2)


# Define UI ----

ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("CLT Large sample normality of means"),
  
  sidebarLayout(
    
    sidebarPanel("",

                 numericInput("xbar","Mean",value = 0),
                 numericInput("SD","Standard Deviation",value=1,min=0.00000),
                 
                 selectInput(inputId = "n",
                             label="Choose sample size",
                             choices=c("1" = 1,"2"= 2,"4"= 4,"8"=8,"32" = 32,"64" = 64),
                             selected = 32),
        
                 
                 actionButton("sample",label="Take Sample"), 
                 actionButton("samples5",label="Take 5 "),
                 actionButton("start",label="Start "),
                 actionButton("stop",label="Stop "),  
                 
                 checkboxInput("add",label="Accumulate",value=TRUE),
                 
                 textOutput("count") # checking how the counter is working 
                 
    ), # end sidebarPanel
    
    
    mainPanel(
      
      tabPanel("CLTplots",
               fluidRow(
                 column(12,plotOutput("CLTplot1")),
                 column(12,dataTableOutput("sampleTable")),
                 column(12,div(style="height:100px;padding: 0px 0px; margin-top:1em"),plotOutput("CLTplot3"))
               )
      )
    )
  ) # end mainPanel
) # end UI


#################################
###### Define server logic ######
#################################

server <- function(input, output,session){
  
  # define reactive variables here
  counter <- reactiveValues(countervalue = 0)
  values <- reactiveValues(total = 0)
  autorun <- reactiveValues(auto = 0)
  
  # eventReactive(input$size, {
  #   counter$countervalue <- 0                      #if the sample size is changed, reset the counter value to zero
  #  })
  
  observeEvent(input$size, {
    counter$countervalue <- 0                     # to do if the sample size is changed, reset the counter value to zero
  })
  
  observeEvent(input$sample, {
    counter$countervalue <- counter$countervalue + 1  # if the sample button is clicked, increment the value by 1 and update it
  })
  
  observeEvent(input$start, {
    print('starting')
    autorun$auto <<- 1
  })
  
  observeEvent(input$stop, {
    print('stopping')
    autorun$auto <<- 0
  })
  
  
  
  output$count <- renderText({
    paste("Counter Value is ", counter$countervalue)   # print the latest value stored in the reactiveValues object (only for error checking and will remove once completed also text output in sideBar UI)
  })
  
  output$total <- renderText({
    values$total
  })
  
  allm <- vector()
  samp <- reactiveVal()
  meansamp <- reactiveVal()
  allmeansamp <- reactiveValues(allm=vector())
  
  
    
  
  
  
  observeEvent(input$sample,{
    samp <- round(rnorm(input$n,mean=input$xbar,sd=input$SD),1)
    samp(samp)
    meansamp <- round(mean(samp),2)
    meansamp(meansamp)
   
    values$total <- c(values$total,meansamp)
    #c(allmeansamp,meansamp)
  })
  
  # click sample every 100 ms
  observe({
    if (autorun$auto == 1) {
      click("sample")
      invalidateLater(100)
    }
  })
  
  observeEvent(input$samples5,{
    for (i in 1:5) {
      click("sample")
      invalidateLater(1500)
    }
    #c(allmeansamp,meansamp)
  })
  
  
  # sampData <- reactive({
  #   # Make action button dependency
  #   input$sample 
  # })
  # 
  # addData <- reactive({
  #   # Make action button dependency
  #   input$add 
  # })
  # end variables
  
  output$CLTplot1 <- renderPlot({
    
    lower <- input$xbar-3*input$SD
    upper <- input$xbar+3*input$SD
    x   <- seq(input$xbar-3*input$SD,input$xbar+3*input$SD,length=1000)
    y   <- dnorm(x,mean=input$xbar, sd=input$SD)
    z   <- rnorm(length(x),input$xbar,input$SD)
    df <- data.frame(x=x,y=y,z=z)
    jitter_y <- max(y)/50
    #plot(x,y, type="l", lwd=1,axes=F,ylab="",xlab="")
    
    p <-  ggplot(df, aes(x = x, y = y))  + geom_point() 
      
    #stat_function(fun=dnorm,
    #              color="blue",
    #              args=list(mean=input$xbar, 
    #                        sd=input$SD)) 
    if (length(samp())>0) {
      # points for the sample
      points <- data.frame(x = samp(),y=rep(0,length(samp)))
    
      #print('hello')
      p <- p + geom_jitter(data=points,aes(colour = 'red'),width=0,
                           height=jitter_y)
      #p <- p + geom_point(data=points,aes(colour = 'red',shape = "."))
      
    }
    p
    
    
    #axis(1,at=c(input$xbar-3*input$SD,input$xbar-2*input$SD,input$xbar-input$SD,input$xbar,input$xbar+input$SD,input$xbar+2*input$SD,input$xbar+3*input$SD),labels=c(paste(input$xbar-3*input$SD),paste(input$xbar-2*input$SD),paste(input$xbar-input$SD),paste(input$xbar),paste(input$xbar+input$SD),paste(input$xbar+2*input$SD),paste(input$xbar+3*input$SD)))
    
    #mtext(bquote(~ mu == .(input$xbar) *","~ sigma == .(input$SD)),col="blue",cex=1.5)
    
  }) # end CLTplot1
  
#  output$CLTplot2 <- renderPlot({
##    
#    par(mfrow=c(2,1))
#    stripchart(samp(), pch=4, offset=1/2, col="blue", lwd=2, las=1, xlim=c(input$xbar-3*input$SD,input$xbar+3*input$SD),main="",ylim=c(0.95,1.5),method='stack')
#    
#    legend("topright",legend=c(expression(paste(n," = ")),input$n),bty="n",horiz=TRUE,cex=1.5,text.col="blue",x.intersp=0,text.width=c(0.25*input$SD,0.25*input$SD),inset=c(0.05,0))
#    
#    stripchart(meansamp(), pch=4, offset=1/2, col="blue", lwd=2, las=1, xlim=c(input$xbar-3*input$SD,input$xbar+3*input$SD),main="",ylim=c(0.95,1.5),method='stack')
    
#    legend("right",legend=c(expression(paste(bar(x)," = ")),meansamp()),bty="n",horiz=TRUE,cex=1.5,text.col="blue",x.intersp=0,text.width=c(0.25*input$SD,0.25*input$SD),inset=c(0.05,0))
    
#    par(mfrow=c(1,1))
    
#  },height=400) # end CLTplot2
  
  output$sampleTable <- renderDataTable(samp(),
                                        options = list(
                                        pageLength = 5,
                                        initComplete = I("function(settings, json) {alert('Done.');}")
                                                               )
  )
  
  
  output$CLTplot3 <- renderPlot({
    
    par(mfrow=c(1,1))
    
    #hist(values$total,30)
    
    df <- data.frame(x = values$total)
    m.hat <- round(100* mean(values$total))/100
    ss <- round(100* var(values$total))/100
    label1 <- paste('mean: ',m.hat,sep=" " )
    label2 <- paste('variance: ',ss,sep=" " )
    #geom_histogram(aes(y=..density..) ) + 
    #ggplot(df, aes(x=x)) + 
    #  geom_density(color="black", fill="white") +
    #  geom_vline(xintercept = m.hat, color = "red", linetype = "dashed") +
    #  annotate("text", label = label1, x = m.hat, y = 0.3, size = 4, colour = "red") +
      #annotate("text", label = label2, x = 0.5, y = 0.1, size = 4, colour = "red") +
    #  stat_function(fun=dnorm,
    #                color="red",
    #                args=list(mean=mean(m.hat), 
    #                          sd=sqrt(ss))) +
    #  xlab('Sample mean') +
    #  ggtitle("Distribution of the sample mean") 
    if (length(values$total) > 1) {
      print(length(values$total))
      ggplot(df,aes(x=x)) + geom_blank() + 
      geom_histogram(aes(x,stat(density)),
                     color="black",
                     binwidth=0.1)
    }
    
    
  },height=400) # end CLTplot3
  
} # end server


# Run the app ----
shinyApp(ui = ui, server = server)#,display.mode="showcase")
