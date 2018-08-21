# document started April 2018
# search to do for work in progress
# updated from demo/App.R May 10th


#rm(list=rm())
search()

#(.packages())
#detach("package:", unload=TRUE)

setwd("/Users/micheldelange/Documents/shiny/") # uni directory

library(shiny)

# Define UI ----

ui <- fluidPage(
  
  titlePanel("CLT Large sample normality of means"),
  
  sidebarLayout(
    
    sidebarPanel("",
                 # radioButtons("Pop",label="Distribution",choices=c("Normal","Skewed"),inline=TRUE), # to do put the skewed version in a tab
                 
                 numericInput("xbar","Mean",value = 0),
                 
                 numericInput("SD","Standard Deviation",value=1,min=0.00000),
                 
                 selectInput("n","size",label="Choose sample size",choices=c("1","2","4","8","32","64")), # to do when this changes counter on sample actionButton resets to zero
                 
                 actionButton("sample",label="Take Sample"), # to do set up a counter 
                 
                 checkboxInput("add",label="Accumulate",value=FALSE),
                 
                 textOutput("count") # checking how the counter is working 
                 
    ), # end sidebarPanel
    
    
    mainPanel(
      tabPanel("CLTplots",
               fluidRow(
                 column(12,plotOutput("CLTplot1")),
                 column(12,div(style="height:100px;padding: 0px 0px; margin-top:-10em"),plotOutput("CLTplot2")),
                 column(12,div(style="height:100px;padding: 0px 0px; margin-top:-10em"),plotOutput("CLTplot3"))
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
  
  # eventReactive(input$size, {
  #   counter$countervalue <- 0                      #if the sample size is changed, reset the counter value to zero
  #  })
  
  observeEvent(input$size, {
    counter$countervalue <- 0                     # to do if the sample size is changed, reset the counter value to zero
  })
  
  observeEvent(input$sample, {
    counter$countervalue <- counter$countervalue + 1  # if the sample button is clicked, increment the value by 1 and update it
  })
  
  output$count <- renderText({
    paste("Counter Value is ", counter$countervalue)   # print the latest value stored in the reactiveValues object (only for error checking and will remove once completed also text output in sideBar UI)
  })
  
  samp <- reactiveVal(0)
  meansamp <- reactiveVal(0)
  allmeansamp <- reactiveVal(0)
  observeEvent(input$sample,{
    samp <- round(rnorm(input$n,mean=input$xbar,sd=input$SD),1)
    samp(samp)
    meansamp <- round(mean(samp),2)
    meansamp(meansamp)
    allmeansamp <- c(allmeansamp,meansamp)
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
    
    x   <- seq(input$xbar-3*input$SD,input$xbar+3*input$SD,length=1000)
    y   <- dnorm(x,mean=input$xbar, sd=input$SD)
    
    plot(x,y, type="l", lwd=1,axes=F,ylab="",xlab="")
    axis(1,at=c(input$xbar-3*input$SD,input$xbar-2*input$SD,input$xbar-input$SD,input$xbar,input$xbar+input$SD,input$xbar+2*input$SD,input$xbar+3*input$SD),labels=c(paste(input$xbar-3*input$SD),paste(input$xbar-2*input$SD),paste(input$xbar-input$SD),paste(input$xbar),paste(input$xbar+input$SD),paste(input$xbar+2*input$SD),paste(input$xbar+3*input$SD)))
    
    mtext(bquote(~ mu == .(input$xbar) *","~ sigma == .(input$SD)),col="blue",cex=1.5)
    
  }) # end CLTplot1
  
  output$CLTplot2 <- renderPlot({
    
    par(mfrow=c(2,1))
    stripchart(samp(), pch=4, offset=1/2, col="blue", lwd=2, las=1, xlim=c(input$xbar-3*input$SD,input$xbar+3*input$SD),main="",ylim=c(0.95,1.5),method='stack')
    
    legend("topright",legend=c(expression(paste(n," = ")),input$n),bty="n",horiz=TRUE,cex=1.5,text.col="blue",x.intersp=0,text.width=c(0.25*input$SD,0.25*input$SD),inset=c(0.05,0))
    
    stripchart(meansamp(), pch=4, offset=1/2, col="blue", lwd=2, las=1, xlim=c(input$xbar-3*input$SD,input$xbar+3*input$SD),main="",ylim=c(0.95,1.5),method='stack')
    
    legend("right",legend=c(expression(paste(bar(x)," = ")),meansamp()),bty="n",horiz=TRUE,cex=1.5,text.col="blue",x.intersp=0,text.width=c(0.25*input$SD,0.25*input$SD),inset=c(0.05,0))
    
    par(mfrow=c(1,1))
    
  },height=400) # end CLTplot2
  
  output$CLTplot3 <- renderPlot({
    
    par(mfrow=c(1,1))
    hist(allmeansamp,30)
    par(mfrow=c(1,1))
    
  },height=400) # end CLTplot2
  
} # end server


# Run the app ----
shinyApp(ui = ui, server = server)#,display.mode="showcase")
