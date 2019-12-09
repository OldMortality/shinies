# app1
library(shiny)
shinyServer <- function(input, output) {
  
  xbar <- 1711
  sd <- 93
  upp <- xbar + 3 * sd
  low <- xbar - 3 * sd
  x.breaks <- round(seq(low,upp,sd))
  
  getSummary <- function() {
    ht <- as.numeric(input$yourheight) 
    absdiff <- abs(xbar - ht)
    d <- round((absdiff)/sd,2)
    str0 <- paste('Population mean height =',xbar,'mm',sep=' ')
    str1 <- paste('Population standard deviation =',sd,'mm',sep=' ') 
    str2 <- paste('Your height =',input$yourheight,'mm',sep=' ')
    str3 <- paste('Distance from the mean =',absdiff,'mm', sep=' ')
    str4 <- paste('Distance from the mean in standard deviations =',
                  '(distance from the mean)/(standard deviation) =' ,
                  paste(absdiff,'/',sd,'=',sep=''),
                  d,sep=' ')
    result <- paste(str0,str1,str2,str3,str4,
                    sep="<br>")
    return(result)
  } 
  
  
  output$plot1 <- renderPlot({ 
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
    ) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = xbar, sd = sd)) + 
      ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                         limits=c(0,0.005)) +
      xlab("Height (mm)") + 
      theme_grey((basesize=20))
    
    if (input$showsd) {
      
      p <- p +
        geom_segment(x=xbar+2*sd,xend=xbar+3*sd,
                   y=0.004,yend=0.004,
                   colour='black',
                   arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")
      ) +
        annotate("text", label = "1 standard deviation", 
                 x = xbar+ 2.1*sd, 
                 y= 0.0042, hjust=0,
                 size = 5, colour = "black") +
        annotate("text", label = '   This distance is', 
                 x = xbar+ 2.1*sd, 
                 y= 0.0045, hjust=0,
                 size = 5, colour = "black") 
    }
    
    ht <- as.numeric(input$yourheight)
    
    
    if (input$showmean) { 
      p <- p + geom_segment(x=xbar,xend=xbar,
                            y=-0.1,yend=0.00449,
                            colour='red') +
        annotate("text", label = "mean", 
                 x = xbar-10, 
                 y= 0.00475, hjust=0,
                 size = 5, colour = "red")
      
    }
    if (input$showyourheight) {
      above.or.below <- 'below'
      if (as.numeric(input$yourheight) > xbar) {
        above.or.below <- 'above'
      }
      d <- (as.numeric(input$yourheight)-xbar)/sd
      d <- abs((round(100*d))/100)
      str <- paste(d,'standard deviations',
                   above.or.below,'the mean',sep=' ')
      
      pointdata <- data.frame(
        x = c(as.numeric(input$yourheight)), 
        ypos = c(0)
      )
      if (ht < 2510 & ht > 1200) {
        
        p <- p + geom_point(data = pointdata, colour='blue',
                            mapping = 
                              aes(x = x, y = ypos ),
                            show.legend=F,
                            shape = 24,
                            fill='blue',
                            size = 5
                            
        ) +
          annotate("text", label = str, x = pointdata$x, 
                   y= 0.0005, hjust=0,
                   size = 5, colour = "blue") 
         
      }
    }
    p  
  }) # end plot1
  
  
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  output$text1 <- renderText({ 
    paste("input is","<font color=\"#FF0000\"><b>", input$n, "</b></font>") })
  
}