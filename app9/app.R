# app 9
# Plots of 2 variables
#   

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)
library(gridExtra)

data <- read.csv('Lab3data.csv',header=T)


ui <- dashboardPage(
  
  dashboardHeader(title = paste("Lab 4: Experiment 2.",
                  "Voluntary movement in response to sensory stimulus"  ),
                  titleWidth = 850),
  dashboardSidebar(useShinyjs(),
                   
                   
                   selectInput("plot.type", h3("Graph type"), 
                               choices = list("Histogram" = 1, 
                                              "Scatter" = 2,
                                              "Boxplot" = 3
                               ), selected = 1),
                   uiOutput("B_ui"),
                   checkboxInput("showmean", "Show population mean (red)", FALSE),
                   checkboxInput("showgroup", "Show your group mean (blue)", FALSE)
                   
                   
                   
                   
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 12,
             box(title="Lab data (N=615)", width=NULL,
                 plotOutput("distPlot", height = 300)
             )
          )),
      fluidRow(
        column(width = 6,
               box(title="Enter data for your group",width=NULL,
                   rHandsontableOutput("hot", width = 800)
                   
               )),
        column(width = 6,
               box(title="Summary",width=NULL)
               )
        )
      )
    )
    
  




server <- function(input, output) {
  
   
  cache_tbl = NULL
  
  onRestore(function(state) {
    tmp = state$input$hot
    tmp$data2 = jsonlite::fromJSON(
      jsonlite::toJSON(tmp$data2), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })
  
  
  data2 = reactive({
    
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else 
      if (!is.null(cache_tbl)) {
        DF = hot_to_r(cache_tbl)
        cache_tbl <<- NULL
      } else {
        
        DF = datasetInput()
      }
    DF
  })
  
  output$hot <- renderRHandsontable({
    DF = data2()
    
    if (!is.null(DF) ) {
      rhandsontable(DF, width = 800, height = 300) %>%
        hot_cols(fixedColumnsLeft = 1) 
      #%>%hot_rows(fixedRowsTop = 1)
    }
  })
  
  listvars <- reactive(  { 
    
      v1 = 1
      v2 = 2
      v3 = 3
      v4 = 4
      result <- list("Reflex path length"=v1,
                     "Reflex latency"=v2,
                     "Voluntary path length"=v3,
                     "Voluntary latency"=v4)
    
    return( result)
  })
  
  
  listvars2 <- reactive(  { 
    
    v1 = 1
    v2 = 2
    result <- list("path length"=v1,
                   "latency"=v2)
    
    return( result)
  })
  
  
  
  output$B_ui <- renderUI({
    if (input$plot.type == "1"  ) {
      selectInput("variable", h3("Variable"), 
                  choices = listvars()
                  , selected = 1)
    } else {
      if (input$plot.type == "3" ) {
        # boxplot
        selectInput("variable", h3("Variable"), 
                    choices = listvars2()
                    , selected = 1)
      } else
      if (input$plot.type == "2" ) {
        tagList(
          selectInput("variable.x", h4("variable 1"), 
                      choices = listvars()
                      , selected = 1),
          selectInput("variable.y", h4("Variable 2"), 
                      choices = listvars()
                      , selected = 2)
        )
      }
    }
  })
      
      
  cache_tbl = NULL
  
  onRestore(function(state) {
    tmp = state$input$hot
    tmp$data2 = jsonlite::fromJSON(
      jsonlite::toJSON(tmp$data2), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })
  
  datasetInput <- reactive({
    result <- data.frame(reflex.path.length=c(1750,rep(NA,2)),
                         reflex.latency=c(47,rep(NA,2)),
                         voluntary.path.length = c(3000,rep(NA,2)),
                         voluntary.latency = c(200,rep(NA,2))
                         )
    result
  })
  
  
  
  
  output$distPlot <- renderPlot({
    
    
    # histogram
     if (input$plot.type == 1) {
      colNumber <- as.numeric(input$variable) 
      if (length(colNumber) == 0) {
        colNumber <- 1
      }
      colName <- colnames(data)[colNumber]
      colName.pretty <- ""
      if (colNumber==1) { colName.pretty = 'Reflex path length (mm)'}
      if (colNumber==2) { colName.pretty = 'Mean reflex latency (ms)'}
      if (colNumber==3) { colName.pretty = 'Voluntary path length (mm)'}
      if (colNumber==4) { colName.pretty = 'Mean voluntary latency (ms)'}
      theColumnData <- as.numeric(data[,colNumber])
      df <- data.frame(x = theColumnData)
      #print(head(df))
      p <- ggplot(df,aes(x=x)) + geom_histogram(color="black", fill="white") +
        xlab(colName)
      t <- paste('Histogram of',colName.pretty,
                 '(N=615)',sep=' ')
      p <- p +
        ggtitle(t)
      mu <- mean(theColumnData,na.rm=T)
      sd <- sqrt(var(theColumnData,na.rm=T))
      x.breaks <- c(mu-3*sd,mu-2*sd,mu-sd,mu,mu+sd,mu+2*sd,mu+3*sd)
      x.breaks <- round(x.breaks)
      p <- p + 
        scale_x_continuous(breaks=x.breaks)
      if (input$showmean){
        mean.str <- paste('mean=',round(mu),sep=' ')
      p <- p + 
        geom_vline(xintercept=mu,colour='red') 
      
      #p <- p +
      #  geom_segment(x=mu,y=5,xend=mu+sd,yend=5,colour='red',
      #               size=1,arrow=arrow()) +
      #  geom_segment(x=mu,y=3,xend=mu-sd,yend=3,colour='red',
      #               size=1,arrow=arrow()) +
      ##
      #  annotate("text", label = "1 SD", 
      #           x = mu+round(sd/3), 
      #           y= 10, hjust=0,
      #           size = 5, colour = "red") +
      #  annotate("text", label = "1 SD", 
      #           x = mu-round(sd), 
      #           y= 10, hjust=0,
      #           size = 5, colour = "red")
      p <- p +
        annotate("text", label = mean.str, x = mu+1, 
                 y= 70, hjust=0,
                 size = 5, colour = "red")  }
      p <- p +
        xlab(colName.pretty)
      
      ## Add the group data (from the hot table)
      if (input$showgroup) {
        if(is.null(input$hot)) return(NULL)
          data2 <- hot_to_r(input$hot)
          myColumn <- data2[,colNumber]
          dropm <- which(is.na(myColumn))
          if (length(dropm) > 0 ) {
            myColumn <- myColumn[-dropm]
          } 
          if (length(myColumn)>0) {
            myColumn <- as.numeric(data2[,colNumber])  
          }
          thePoints <-data.frame(x=myColumn,
                             y=rep(0,length(myColumn)))
          p <- p + geom_point(data=thePoints,aes(x=x,y=y),
                          colour='blue',
                          shape = 4,
                          size = 5)
        }
       
      print(p)
      
       
    }
    
    # scatter 
    if (input$plot.type == 2  ) {
      colNumber.x <- as.numeric(input$variable.x)
      colNumber.y <- as.numeric(input$variable.y) 
      if (length(colNumber.x) == 0) {
        colNumber.x <- 1
      }
      if (length(colNumber.y) == 0) {
        colNumber.y <- 2
      }
      colName.x <- colnames(data)[colNumber.x]
      colName.y <- colnames(data)[colNumber.y]
     
      colName.x.pretty <- ""
      if (colNumber.x==1) { colName.x.pretty = 'Reflex path length (mm)'}
      if (colNumber.x==2) { colName.x.pretty = 'Mean reflex latency (ms)'}
      if (colNumber.x==3) { colName.x.pretty = 'Voluntary path length (mm)'}
      if (colNumber.x==4) { colName.x.pretty = 'Mean voluntary latency (ms)'}
      colName.y.pretty <- ""
      if (colNumber.y==1) { colName.y.pretty = 'Reflex path length (mm)'}
      if (colNumber.y==2) { colName.y.pretty = 'Mean reflex latency (ms)'}
      if (colNumber.y==3) { colName.y.pretty = 'Voluntary path length (mm)'}
      if (colNumber.y==4) { colName.y.pretty = 'Mean voluntary latency (ms)'}
      
      t <- paste('Scatterplot of ',colName.y.pretty,'vs',
                 colName.x.pretty,
                 '(N=615)',sep=' ')
      
      df <- data.frame(x=as.numeric(data[,colNumber.x]),
                         y=as.numeric(data[,colNumber.y])
                         )
      mu.x <- mean(df$x,na.rm=T)
      sd.x <- sqrt(var(df$x,na.rm=T))
      mu.y <- mean(df$y,na.rm=T)
      sd.y <- sqrt(var(df$y,na.rm=T))
      
      ## fitted line
      m <- lm(y~x,data=df)
      a0 <- coefficients(m)[1]
      a1 <- coefficients(m)[2]
       
        
      p <- ggplot(df,aes(x=x,y=y)) + geom_point() + 
           ylab(colName.y) +
           xlab(colName.x) +
           title(main="Scatter")
      p <- p +
        ggtitle(t)
      x.breaks <- 
        c(mu.x-3*sd.x,mu.x-2*sd.x,mu.x-sd.x,mu.x,mu.x+sd.x,mu.x+2*sd.x,mu.x+3*sd.x)
      x.breaks <- round(x.breaks)
      y.breaks <- 
        c(mu.y-3*sd.y,mu.y-2*sd.y,mu.y-sd.y,mu.y,mu.y+sd.y,mu.y+2*sd.y,mu.y+3*sd.y)
      y.breaks <- round(y.breaks)
      
      p <- p +
        geom_abline(intercept=a0,slope=a1,colour='darkred')
      
      p <- p + 
        scale_x_continuous(breaks=x.breaks) +
        scale_y_continuous(breaks=y.breaks)
      p <- p +
        xlab(colName.x.pretty) +
        ylab(colName.y.pretty)
      
      if (input$showmean) {
        p <- p +
          geom_hline(yintercept=mu.y,colour='red') +
          geom_vline(xintercept = mu.x,colour='red')
      }
      
      ## add the group data
      ## Add the group data (from the hot table)
      if (input$showgroup){
        if(is.null(input$hot)) return(NULL)
        data2 <- hot_to_r(input$hot)
        myColumn.x <- data2[,colNumber.x]
        myColumn.y <- data2[,colNumber.y]
        dropm.x <- which(is.na(myColumn.x))
        dropm.y <- which(is.na(myColumn.y))
        dropm <- c(dropm.x,dropm.y)
        if (length(dropm) > 0 ) {
          myColumn.x <- myColumn.x[-dropm]
          myColumn.y <- myColumn.y[-dropm]
        } 
        if (length(myColumn.x)>0) {
          myColumn.x <- as.numeric(data2[,colNumber.x])  
          myColumn.y <- as.numeric(data2[,colNumber.y])  
        
        }
        thePoints <-data.frame(x=myColumn.x,
                             y=myColumn.y)
        p <- p + geom_point(data=thePoints,aes(x=x,y=y),
                          colour='blue',size=4,shape=4)
      }
      ##
      
      print(p)
      
    }
    # boxplot
    if (input$plot.type == 3 ) {
       
      colNumber <- as.numeric(input$variable) 
      if (length(colNumber) == 0) {
        colNumber <- 1
      }
      colName <- colnames(data)[colNumber]
      if (colNumber == 1) {
        # length
        length.limits <- c(1000,4000)
        df1 <-data.frame(voluntary=data$voluntarypathlength)
        p1 <- ggplot(df1,aes(y=voluntary)) + geom_boxplot()
        p1 <- p1 +
          xlab('Voluntary') +
          ylab('Path length (mm)') +
          scale_y_continuous(limits=length.limits) + 
          scale_x_continuous(breaks=NULL,limits=c(-3,3))
        df2 <- data.frame(reflex=data$reflexpathlength)
        p2 <- ggplot(df2,aes(y=reflex)) + geom_boxplot()
        p2 <- p2 + 
          xlab('Reflex') +
          ylab('') +
          scale_y_continuous(limits=length.limits) + 
          scale_x_continuous(breaks=NULL,limits=c(-3,3))
        #grid.arrange(p1, p2, ncol=2,heights=c(0,0))
        df1$type <- 'voluntary'
        df1$var <- df1$voluntary
        df2$type <- 'reflex'
        df2$var <- df2$reflex
        
        df3 <- rbind(df1[,c(2,3)],df2[,c(2,3)])
        
        t <- paste('Boxplot of reflex path length (mm)',
                   '(N=615)',sep=' ')
        
        p <- ggplot(df3,aes(y=var,x=type)) + geom_boxplot() +
          ggtitle(t) +
          xlab('Path length') +
          ylab('') +
          scale_y_continuous(limits=length.limits) +
          scale_x_discrete()
          
      } else {
        if (colNumber == 2) {
          # latency
          t <- paste('Boxplot of latency (ms)',
                     '(N=615)',sep=' ')
          
          df1 <-data.frame(voluntary=data$meanvoluntarylatency)
          latency.limits <- c(0,300)
          df2 <- data.frame(reflex=data$meanreflexlatency)
          
          df1$type <- 'voluntary'
          df1$var <- df1$voluntary
          df2$type <- 'reflex'
          df2$var <- df2$reflex
          
          df3 <- rbind(df1[,c(2,3)],df2[,c(2,3)])
          p <- ggplot(df3,aes(y=var,x=type)) + geom_boxplot() +
            xlab('Latency') +
            ylab('') +
            scale_y_continuous(limits=latency.limits) + 
            scale_x_discrete()
          
          
        }
      }
      # add data points from the hot table
      if(is.null(input$hot)) return(NULL)
      data2 <- hot_to_r(input$hot)
      if (colNumber==1) {
        # length
        myColumn.1 <- data2[,1]
        myColumn.2 <- data2[,3]
      } else {
        myColumn.1 <- data2[,2]
        myColumn.2 <- data2[,4]
      }
      
      dropm.1 <- which(is.na(myColumn.1))
      dropm.2 <- which(is.na(myColumn.2))
      
      if (length(dropm.1) > 0 ) {
        myColumn.1 <- myColumn.1[-dropm.1]
      }
      if (length(dropm.2) > 0 ) {
        myColumn.2 <- myColumn.2[-dropm.2]
      }
      if (length(myColumn.1)>0) {
        myColumn.1 <- as.numeric(myColumn.1)  
      }
      if (length(myColumn.2)>0) {
        myColumn.2 <- as.numeric(myColumn.2)  
      }
      
      
      if (input$showmean) {
        p <- p +
          stat_summary(fun.y=mean, colour="red", geom="point")
      }
      
      # show individual points in the boxplot
      #   replaced by showing the mean
      #thePoints1 <-data.frame(y=myColumn.1,
      #                        type='reflex')
      #thePoints2 <-data.frame(y=myColumn.2,
      #                        type='voluntary')
      #thePoints <- rbind(thePoints1,thePoints2)
      
      #p <- p + 
      #  geom_point(data=thePoints,
      #             aes(x=type,y=mean(y,na.rm=T)),
      #             colour='blue',size=5,shape=4) 
      if (input$showgroup) {
        thePoints1 <-data.frame(y=mean(myColumn.1,na.rm=T),
                                type='reflex')
        thePoints2 <-data.frame(y=mean(myColumn.2,na.rm=T),
                                type='voluntary')
        thePoints <- rbind(thePoints1,thePoints2)
        p <- p + 
            geom_point(data=thePoints,
                       aes(x=type,y=y),
                       colour='blue',size=5,shape=4) 
        
        
      }
      # show the boxplot
      print(p)
    }# boxplot
  }) 
  
}
  
  


shinyApp(ui, server)