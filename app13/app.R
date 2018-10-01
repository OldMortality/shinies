# app 13.
# height vs finger length - your group
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)


ui <- dashboardPage(
  
  
  
  dashboardHeader(title = "Height vs finger length - your group",
                  titleWidth = 550),
  dashboardSidebar(useShinyjs(),
                   actionButton("clear",label="Clear"),
                   actionButton("sample",label="Take 1 sample (n=10)"),
                   checkboxInput("showalllines", "Show all red lines", FALSE)),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 12,
             
             box(title="Enter your group's data",
                 rHandsontableOutput("hot")
             ),
             box( 
               title="", 
               plotOutput("thePlot") )
      )
    )
  )
)


server <- function(input, output) {
  
  df <- data.frame(x=rnorm(100))
  
  cache_tbl = NULL
  
  #onRestore(function(state) {
  #  tmp = state$input$hot
  #  tmp$data = jsonlite::fromJSON(
  #    jsonlite::toJSON(tmp$data), simplifyVector = FALSE)
  #  cache_tbl <<- tmp
  #})
  
  datasetInput <- reactive({
    result <- data.frame(finger.length=c(70,rep(NA,9)),
                         height=c(1750,rep(NA,9)))
    result
  })
  
  
  data = reactive({
    
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      
    } else { 
      #if (!is.null(cache_tbl)) {
      #  DF = hot_to_r(cache_tbl)
      #  cache_tbl <<- NULL
      #} else {
        
        DF = datasetInput()
      }
    DF
  })
  
  output$hot <- renderRHandsontable({
    DF = data()
    
    if (!is.null(DF) ) {
      rhandsontable(DF, width = 200, height = 300) %>%
        hot_cols(fixedColumnsLeft = 1) %>%
        hot_rows(fixedRowsTop = 1)
    }
  })
  
  
  
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
  
 
  
  
  
  # 1 sample
  observeEvent(input$sample,{
    
    
    s <- f[sample(nrow(f),size=10),]
    
    
    m2 <- lm(hei~fin,data=s)
    alla0$a0s <- c(alla0$a0s,coefficients(m2)[[1]])
    alla1$a1s <- c(alla1$a1s,coefficients(m2)[[2]])
    
    # store sample to show in plot
    samp <- s
    # no idea what this does
    samp(samp)
    
  })  
  
  
  output$thePlot <- renderPlot({
    
    if (is.null(input$hot)) {
      hot.data <- data.frame()
    } else {
      hot.data <- hot_to_r(input$hot)
    }
    
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
    
    
    # show the hot table data
    if (dim(hot.data)[1] >0) {
      p <- p + geom_point(data=hot.data,aes(x=finger.length,
                                           y=height),colour='blue',
                          shape = 4,
                          size = 5)
    }
    if (dim(hot.data)[1] >2) {
      # plot regression line
      m <- lm(height~finger.length,data=hot.data)
      a <- coefficients(m)[1]
      b <- coefficients(m)[2]
      p <- p + geom_abline(intercept=a,slope=b,colour='blue')
    
    }
    
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