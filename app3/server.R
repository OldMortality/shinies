# app 3.
# You enter height of your group members, and shows how this
#   compares to the population, and to other groups
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2) 
library(shinyBS)

shinyServer <- function(input, output) {
  
  
  #addTooltip(session, id = "shownormal", title = "This is an input.",
  #           placement = "left", trigger = "hover")
 
  mu <- 1711
  sd <- 92
   
  
  getSummary <- function() {
    str0 <- paste('Population mean height is',mu,'mm',sep=' ')
    str1 <- paste('Population standard deviation is',sd,'mm',sep=' ') 
    result <- paste(str0,str1,sep='<br>')
    return(result)
  }
  
  getSummary2 <- function() {
    groupmeans <- c(input$othergroup1,input$othergroup2,input$othergroup3)
    groupmeans <- sort(as.numeric(groupmeans))
    str0 <- "Each sample mean provides an estimate of the population mean."
    str1 <- "Sample means:"
    strl0 <- "<ul>"
    strl1 <- paste("<li>",groupmeans[1],sep='')
    strl2 <- paste("<li>",groupmeans[2],sep='')
    strl3 <- paste("<li>",groupmeans[3],sep='')
    strl4 <- "</ul>"
    meansList <- paste(strl0,strl1,strl2,strl3,strl4,sep='')
    result <- paste(str0,"<br>",str1,"<br>",meansList)
    return(result) 
  }
    
  
  
  upp <- mu + 3 * sd
  low <- mu - 3 * sd
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
  
  
  output$plot1 <- renderPlot({ 
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)) +
      stat_function(fun = dnorm, show.legend=F,colour='red',n = 101, args = list(mean = mu, sd = sd)) + ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                         limits=c(0,0.005)) +
      xlab("Height (mm)") +
      geom_segment(x=mu+2*sd,xend=mu+3*sd,
                   y=0.004,yend=0.004,
                   colour='black',
                   arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")
      ) +
      annotate("text", label = "1 standard deviation", 
               x = mu+ 2.1*sd, 
               y= 0.0042, hjust=0,
               size = 5, colour = "black")
    
    
    if (input$showmean) {
      p <- p + geom_segment(x=mu,xend=mu,
                            y=-0.1,yend=0.00449,
                            colour='red') +
        annotate("text", label = "mean", 
                 x = mu-9.1, 
                 y= 0.00475, hjust=0,
                 size = 5, colour = "red")
      
    }
    if (input$showyourgroupheight) {
      groupHeights <- input$yourgroupheight
      
      vHeights <- as.numeric(strsplit(groupHeights,' ')[[1]])
      mnG <- round(mean(vHeights))
      
      above.or.below <- 'below'
      if (mnG > mu) {
        above.or.below <- 'above'
      }
      d <- (mnG-mu)/sd
      d <- abs((round(100*d))/100)
      str <- paste(d,'standard deviations',
                   above.or.below,'the mean',sep=' ')
       
      pointdata <- data.frame(
        #x = c(as.numeric(input$yourgroupheight)), 
        x <- mnG,
        ypos = c(0)
      ) 
      p <- p + geom_point(data = pointdata, colour='blue',
                          mapping = 
                            aes(x = x, y = ypos ),
                          show.legend=F,
                          shape = 24,
                          size = 5,
                          fill='blue')
                          
     
      
    }
    if (input$showothergroups) {
      h1 <- as.numeric(input$othergroup1)
      h2 <- as.numeric(input$othergroup2)
      h3 <- as.numeric(input$othergroup3)

      vHeights <- c(h1,h2,h3) 
      pointdata2 <- data.frame( 
          x2 <- vHeights,
          ypos2 = rep(0,length(vHeights))
        )
      if (length(vHeights) > 0) {
        p <- p + geom_point(data = pointdata2, colour='green',
                            mapping = 
                              aes(x = x2, y = ypos2 ),
                            show.legend=F,
                            shape = 24,
                            size = 5,
                            fill='green')
      }
      
    }
    
    p  
  }) # end plot1
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  output$summary2 <- renderText(
    paste("<font size=4>",getSummary2(),
          "</font>")
  )
  
}
