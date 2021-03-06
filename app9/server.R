# app 9
# Plots of 2 variables
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)

#library(gridExtra)
library(shinyTable)

 

shinyServer <- function(input, output) {
  

  #observe({
  #  if (!is.null(input$variable.x) && 
  #      (input$variable.x == 5||input$variable.x == 6 ))  {
  #    shinyjs::hide("variable.y")
  #  } else {
  #      if (!is.null(input$variable.y) && 
  #        (input$variable.x == 5 || input$variable.x == 6 ))  {
  #      #shinyjs::hide("variable.x")
  #    }
  #  }
  #})
  
  
    data <- read.csv('Lab3data.csv',header=T)
  
  
  rv <- reactiveValues(cachedTbl = NULL)  
  
  all.pathlength <- c(data$reflexpathlength, data$voluntarypathlength)
  all.latency    <- c(data$meanreflexlatency, data$meanvoluntarylatency)
  
  output$tbl <- renderHtable({ 
     
    if (is.null(input$tbl)){  
      tbl = data.frame(reflex.path.length=c(1750,rep("",9)),
                       reflex.latency=c(47,rep("",9)),
                       voluntary.path.length = c(3000,rep("",9)),
                       voluntary.latency = c(200,rep("",9))
      ) 
      rv$cachedTbl <<- tbl
      return(tbl)
    } else{
      rv$cachedTbl <<- input$tbl
      return(input$tbl)
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
  
  listvars.scatter <- reactive(  { 
    
    v1 = 1
    v2 = 2
    v3 = 3
    v4 = 4
    v5 = 5
    v6 = 6
    result <- list("Reflex path length"=v1,
                   "Reflex latency"=v2,
                   "Voluntary path length"=v3,
                   "Voluntary latency"=v4,
                   "All path lengths"=v5,
                   "All latencies"=v6)
    
    return( result)
  })
  
  
  listvars.boxplot <- reactive(  { 
    
    v1 = 1
    v2 = 2
    result <- list("Path length"=v1,
                   "Latency"=v2)
    
    return( result)
  })
  
  
  
  
  output$checkboxes <- renderUI({
    if (!is.null(input$plot.type)) {
      if (input$plot.type == '2') {
      # scatter
        tagList(
          checkboxInput("showall", "Show HUBS191 data (black)", TRUE),
          checkboxInput("showgroup", "Show your group data (blue)", FALSE)
        )
      } else {
        checkboxInput("showgroup", "Show your group data (blue)", FALSE)
      }
    }
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
                    choices = listvars.boxplot()
                    , selected = 1)
      } else
      if (input$plot.type == "2" ) {
        tagList(
          selectInput("variable.x", h4("Variable 1"), 
                      choices = listvars.scatter()
                      , selected = 1),
          selectInput("variable.y", h4("Variable 2"), 
                      choices = listvars.scatter()
                      , selected = 2)
        )
      }
    }
  })
      
  
   
  getSummary <- function(){  
    
    input$actionButtonID
    
    group.tbl <- isolate(rv$cachedTbl) 
    
    if (!is.null(input$plot.type)) {
    
      if (input$plot.type==1) {
        # histogram
        line0.a <- paste("<b>","HUBS191 data","</b>",sep='')
        line1.a <- paste("The mean ",cname.summary()," is:",
                         theColumnData.mean(),
                         sep=' ')
        line2.a <- paste("The standard deviation of",cname.summary() ,"is:",
                         round(sqrt(theColumnData.var()),1),
                         sep=' ')
        result.a <- paste(line0.a,line1.a,line2.a,sep="<br>")
        
        line0.g <- paste("<br><b>","Your group data","</b>",sep='')
        line1.g <- paste("The mean ",cname.summary()," is:",
                         theColumnData.group.mean(),
                         sep=' ')
        line2.g <- paste("The standard deviation of",cname.summary() ,"is:",
                         round(sqrt(theColumnData.group.var()),1),
                         sep=' ')
        result.g <- paste(line0.g,line1.g,line2.g,sep="<br>")
        
        result <- ""
        if (!is.null(input$showmean) && input$showmean) {
          result <- result.a
        }
        if (!is.null(input$showgroup) && input$showgroup) {
          result <- paste(result,result.g)
        }
        
        return(result)
      }
      if (input$plot.type==2) {
        # scatter
        result.a <- ""
        if ((!is.null(a0()) && !is.na(a1()))) {
          line0.a <- paste("<b>","All data","</b><p>",sep='')
          
          line3.a <- paste(cname.y(),"=",round(a0(),1),
                         "+",round(a1(),4),"*",cname.x(),sep=' ') 
          result.a <- paste(line0.a,line3.a)
        }
        
        result.b <- NULL 
        if (!is.null(a1.g()) && !is.na(a1.g())) { 
          # enough data for regression line through group data
          line0.g <- paste("<p><b>","Your data","</b><p>",sep='')
          
          line3.a <- paste(cname.y(),"=",round(a0.g(),1),
                           "+",round(a1.g(),4),"*",cname.x(),sep=' ')
          result.b <- paste(line0.g,line3.a)
          
        }
        if (!is.null(result.b)) {
          result <- paste(result.a,result.b,sep="<p>")  
        } else {
          result <- result.a
        }
        result <- paste(result.a,result.b,sep=" ")
        return(result)
      }
      if (input$plot.type==3) {
        #boxplot
        
        colNumber <- as.numeric(input$variable) 
        if (length(colNumber) == 0) {
          colNumber <- 1
        }
        colName <- colnames(data)[colNumber]
        if (colNumber == 1) {
          # length
          d1.ref <- data$reflexpathlength
          d1.vol <- data$voluntarypathlength

          l0 <- paste("<b>","HUBS191 data","</b><br>")
          l1 <- paste("The mean reflex path length is",
                      round(mean(d1.ref,na.rm=T),1),sep=' ')
          l2 <- paste("The mean voluntary path length is",
                      round(mean(d1.vol,na.rm=T),1),sep=' ')
          
          l0.g <- paste("<b>","Your group data","</b><br>")
          
          #l1.g <- paste("The mean reflex path length is",
          #            round(mean(
          #              group.tbl$reflexpathlength,na.rm=T),1),sep=' ')
          #l2.g <- paste("The standard deviation of reflex path length is",
          #              round(sqrt(var(group.tbl$reflexpathlength,na.rm=T)),1),sep=' ')
          #l.g <- paste(l0.g,l1.g,l2.g,sep="<br>")
          result <- paste(l0,l1,l2,sep="<br>")
          
        } else {
          # latency
          
          d1.ref <- data$meanreflexlatency
          d1.vol <- data$meanvoluntarylatency
          l0 <- paste("<b>","All data","</b>")
          l1 <- paste("The mean reflex latency length is",
                      round(mean(d1.ref,na.rm=T),1),sep=' ')
          l2 <- paste("The mean voluntary latency length is",
                      round(mean(d1.vol,na.rm=T),1),sep=' ')
          
          result <- paste(l0,l1,l2,sep="<br>")
          
        }
        
        return(result)
      }
      
      
    }
  }
  
  a0 <- reactiveVal()
  a1 <- reactiveVal()
  a0.g <- reactiveVal()
  a1.g <- reactiveVal()
  group.regression <- reactiveVal()
  cname.x <- reactiveVal()
  cname.y <- reactiveVal()
  cname.summary <- reactiveVal()
  theColumnData.mean <- reactiveVal()
  theColumnData.var <- reactiveVal()
  theColumnData.group.mean <- reactiveVal()
  theColumnData.group.var <- reactiveVal()
  
  
  
  output$summary <- renderText( 
    
    getSummary()
  )
      
   
  
  
  
  
  
  
  output$distPlot <- renderPlot({
    
    input$actionButtonID
    group.tbl <- isolate(rv$cachedTbl) 
     
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
     
      # for use in summary
      cname.summary <- colName.pretty
      cname.summary(cname.summary)
      theColumnData <- as.numeric(data[,colNumber])
      theColumnData.mean <- round(mean(theColumnData,na.rm=T),1)
      theColumnData.var <- round(var(theColumnData,na.rm=T),1)
      #print(theColumnData.mean)
      #print(theColumnData.var)
      # update reflective values
      theColumnData.mean(theColumnData.mean)
      theColumnData.var(theColumnData.var)
      
      theColumnData.group.mean <- 
        mean(as.numeric(as.character(group.tbl[,colNumber])),na.rm=T)
      theColumnData.group.var <- 
        var(as.numeric(as.character(group.tbl[,colNumber])),na.rm=T)
      
      # update reflective values
      theColumnData.group.var(theColumnData.group.var)
      theColumnData.group.mean(theColumnData.group.mean)
      df <- data.frame(x = theColumnData)
      p <- ggplot(df,aes(x=x))
        p <-  p + geom_histogram(color="black", fill="white") +
          xlab(colName) +
          ylab('count')
        t <- paste('Histogram of',colName.pretty,
                   '(N=615)',sep=' ')
        p <- p +
          ggtitle(t) + 
          theme(plot.title = element_text(size=22)) +
          theme_gray((base_size=22))
        
      
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
      
      
      p <- p +
        annotate("text", label = mean.str, x = mu+1, 
                 y= 70, hjust=0,
                 size = 5, colour = "red")  }
      p <- p +
        xlab(colName.pretty)
      
      ## Add the group data  
      if (!is.null(input$showgroup) && input$showgroup) {
           
          data2 <- group.tbl 
          myColumn <- data2[,colNumber]
          dropm <- which(is.na(myColumn))
          if (length(dropm) > 0 ) {
            myColumn <- myColumn[-dropm]
          } 
          if (length(myColumn)>0) {
            myColumn <- as.numeric(as.character(data2[,colNumber]) )
             
          }
          thePoints <-data.frame(x=myColumn,
                             y=rep(0,length(myColumn)))
          p <- p + geom_point(data=thePoints,aes(x=x,y=y),
                          colour='blue',
                          shape = 24,
                          fill='blue',
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
      
      #if (colNumber.x==5) {
      #  colNumber.y <- 6
      #} else {
      #  if (colNumber.x == 6) {
      #    colNumber.y = 5
      #  }
      #}
      #if (colNumber.y==5) {
      #  colNumber.x <- 6
      #} else {
      #  if (colNumber.y == 6) {
      #    colNumber.x = 5
      #  }
      #}
      
      
     
      colName.x.pretty <- ""
      if (colNumber.x==1) { colName.x.pretty = 'Reflex path length (mm)'}
      if (colNumber.x==2) { colName.x.pretty = 'Mean reflex latency (ms)'}
      if (colNumber.x==3) { colName.x.pretty = 'Voluntary path length (mm)'}
      if (colNumber.x==4) { colName.x.pretty = 'Mean voluntary latency (ms)'}
      if (colNumber.x==5) { colName.x.pretty = 'Path length (mm)'}
      if (colNumber.x==6) { colName.x.pretty = 'Latency (ms)'}
      colName.y.pretty <- ""
      if (colNumber.y==1) { colName.y.pretty = 'Reflex path length (mm)'}
      if (colNumber.y==2) { colName.y.pretty = 'Mean reflex latency (ms)'}
      if (colNumber.y==3) { colName.y.pretty = 'Voluntary path length (mm)'}
      if (colNumber.y==4) { colName.y.pretty = 'Mean voluntary latency (ms)'}
      if (colNumber.y==5) { colName.y.pretty = 'Path length (mm)'}
      if (colNumber.y==6) { colName.y.pretty = 'Latency (ms)'}
      
      # for use in the summary only
      cname.x <- colName.x.pretty
      cname.y <- colName.y.pretty
      cname.x(cname.x)
      cname.y(cname.y)
      
      t <- paste('Scatterplot of ',colName.y.pretty,'vs',
                 colName.x.pretty,
                 '(N=615)',sep=' ')
      print(colNumber.x)
      print(colNumber.y)
      print('pppp')
      if (colNumber.x %in% c(1,2,3,4) && colNumber.y %in% c(1,2,3,4)) {
        df <- data.frame(x=as.numeric(data[,colNumber.x]),
                         y=as.numeric(data[,colNumber.y])
                         )
      } else {
        if ((colNumber.x==5) ) {
          
          df <- data.frame(x=as.numeric(all.pathlength),
                           y=as.numeric(all.latency) )
        } else {
         
            if (colNumber.x==6 ) {
              # the reverse
              df <- data.frame(y=as.numeric(all.pathlength),
                               x=as.numeric(all.latency)) 
            }
          }
        }
      print('......')
      print(head(df))
      print('......')
      mu.x <- mean(df$x,na.rm=T)
      sd.x <- sqrt(var(df$x,na.rm=T))
      mu.y <- mean(df$y,na.rm=T)
      sd.y <- sqrt(var(df$y,na.rm=T))
      
      ## fitted line
      m <- lm(y~x,data=df)
      a0 <- coefficients(m)[1]
      a1 <- coefficients(m)[2]
      a0(a0)
      a1(a1)
      sz = 1
      if (!is.null(input$showall) && !input$showall) {
        # invisible
       sz=-1
      }  
        
      p <- ggplot(df,aes(x=x,y=y)) +
           ylab(colName.y) +
           xlab(colName.x) +
            geom_point(size=sz) 
      
      p <- p +
        ggtitle(t) + 
        theme(plot.title = element_text(size=22)) +
        theme_gray((base_size=22))
      x.breaks <- 
        c(mu.x-3*sd.x,mu.x-2*sd.x,mu.x-sd.x,mu.x,mu.x+sd.x,mu.x+2*sd.x,mu.x+3*sd.x)
      x.breaks <- round(x.breaks)
      y.breaks <- 
        c(mu.y-3*sd.y,mu.y-2*sd.y,mu.y-sd.y,mu.y,mu.y+sd.y,mu.y+2*sd.y,mu.y+3*sd.y)
      y.breaks <- round(y.breaks)
      
      if (!is.null(input$showall) && input$showall) {
        p <- p +
        geom_abline(intercept=a0(),slope=a1(),colour='black')
      }
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
      
      
      ## Add the group data (from the hot table)
        
      if (!is.null(input$showgroup) && input$showgroup) { 
        data2 <- group.tbl
        if (colNumber.x %in% c(1,2,3,4) && colNumber.y %in% c(1,2,3,4)) {
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
            myColumn.x <- as.numeric(as.character(data2[,colNumber.x]))
            myColumn.y <- as.numeric(as.character(data2[,colNumber.y]))  
          }
          thePoints <-data.frame(x=myColumn.x,
                                 y=myColumn.y)
        } else { # we have column 5 or 6
            if (colNumber.x == 5 ) {
            
              # x is path length, y is latency (all of them)
              x. <- c(as.numeric(as.character(data2[,1])),
                      as.numeric(as.character(data2[,3])))
              y. <- c(as.numeric(as.character(data2[,2])),
                      as.numeric(as.character(data2[,4])))
                      
              myColumn.x <- x.
              myColumn.y <- y.
              thePoints <-data.frame(x=myColumn.x,
                                     y=myColumn.y)
            } else {
                if (colNumber.x == 6 ) {
                  # x is latency, y is path length
                  x. <- c(as.numeric(as.character(data2[,2])),
                          as.numeric(as.character(data2[,4])))
                  y. <- c(as.numeric(as.character(data2[,1])),
                          as.numeric(as.character(data2[,3])))
                  myColumn.x <- x.
                  myColumn.y <- y.
                  thePoints <-data.frame(x=myColumn.x,
                                         y=myColumn.y)
                } #if (6,5) 
              } #else (6,5)
            } # we have column 5 or 6
        
        p <- p + geom_point(data=thePoints,aes(x=x,y=y),
                          colour='blue',fill='blue',size=4,shape=24,fill='blue')
        
        # add the regression line for the group data
        group.regression <- F
        if (dim(thePoints)[1] > 1) {
          group.regression <- T
          
          m.group <- lm(y~x,data=thePoints)
          a0.g <- coefficients(m.group)[1]
          a1.g <- coefficients(m.group)[2]
          a0.g(a0.g)
          a1.g(a1.g)
          p <- p + geom_abline(intercept=a0.g,
                               slope=a1.g,
                               colour='blue')
        }  
        group.regression(group.regression)
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
        t <- 'Boxplot of reflex path length (N=615)'
        length.limits <- c(1000,4000)
        df1 <-data.frame(voluntary=data$voluntarypathlength)
        p1 <- ggplot(df1,aes(y=voluntary)) + geom_boxplot() +
          ggtitle(t) + 
          theme(plot.title = element_text(size=22)) +
          theme_gray((base_size=22))
        
        p1 <- p1 +
          xlab('Voluntary') +
          ylab('Path length (mm)') +
          scale_y_continuous(limits=length.limits) + 
          scale_x_continuous(breaks=NULL,limits=c(-3,3))
        df2 <- data.frame(reflex=data$reflexpathlength)
        
        df1$type <- 'voluntary'
        df1$var <- df1$voluntary
        df2$type <- 'reflex'
        df2$var <- df2$reflex
        
        df3 <- rbind(df1[,c(2,3)],df2[,c(2,3)])
        
        t <- paste('Boxplot of Reflex and Voluntary path length (mm)',
                   '(N=615)',sep=' ')
        
        p <- ggplot(df3,aes(y=var,x=type)) + geom_boxplot(width=0.2) +
          ggtitle(t) +
          theme(plot.title = element_text(size=22)) +
          theme_gray((base_size=22)) +
          xlab('') +
          ylab('') +
          scale_y_continuous(limits=length.limits) +
          scale_x_discrete(
            labels=c("reflex"='Reflex path length',
                     "voluntary"='Voluntary path length')
          ) 
        # end of path length  
      } else {
        if (colNumber == 2) {
          # latency
          t <- paste('Boxplot of reflex and voluntary latency (ms)',
                     '(N=615)',sep=' ')
          
          df1 <-data.frame(voluntary=data$meanvoluntarylatency)
          latency.limits <- c(0,300)
          df2 <- data.frame(reflex=data$meanreflexlatency)
          
          df1$type <- 'voluntary'
          df1$var <- df1$voluntary
          df2$type <- 'reflex'
          df2$var <- df2$reflex
          
          df3 <- rbind(df1[,c(2,3)],df2[,c(2,3)])
          p <- ggplot(df3,aes(y=var,x=type)) + geom_boxplot(width=0.2) +
            xlab('') +
            ylab('') +
            ggtitle(t) +
            theme(plot.title = element_text(size=22)) +
            theme_gray((base_size=22)) +
            scale_y_continuous(limits=latency.limits) + 
            scale_x_discrete(
              labels=c("reflex"="Reflex latency",
                       "voluntary"="Voluntary latency")
              )
          
        }
      }
      
      
      # add data points from the group to boxplot
      if (!is.null(input$showgroup) && input$showgroup) {
         
        data2 <- group.tbl
         
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
        if (length(myColumn.1)>0 | length(myColumn.2)>0) {
          myColumn.1 <- as.numeric(as.character(myColumn.1))
          myColumn.2 <- as.numeric(as.character(myColumn.2))
          
          thePoints1 <-data.frame(y=myColumn.1,
                                  type='reflex')
          thePoints2 <-data.frame(y=myColumn.2,
                                  type='voluntary')
          thePoints <- rbind(thePoints1,thePoints2)
          p <- p + 
            geom_point(data=thePoints,
                       aes(x=type,y=y),
                       colour='blue',size=5,shape=24,fill='blue')
        }
        
        
        
      
      }
      
      if (input$showmean){
      # show population mean
      thePoints1 <-data.frame(y=mean(df1$voluntary,na.rm=T),
                              type='voluntary')
      thePoints2 <-data.frame(y=mean(df2$reflex,na.rm=T),
                              type='reflex')
      thePoints <- rbind(thePoints1,thePoints2)
      
      p <- p + 
        geom_point(data=thePoints,
                   aes(x=type,y=y),
                   colour='red',size=5,shape=4) 
      
     
        
         
        
        
      }
      # show the boxplot
      print(p)
    }# boxplot
  }) 
  
}