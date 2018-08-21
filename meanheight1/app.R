# app 1
# Showing the population height distribution,
#   and allows you to find your own height

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


ui <- dashboardPage(
  
  
  
  dashboardHeader(title = "Students' height",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                  
      checkboxInput("showmean", "Show population mean (red)", FALSE),
      checkboxInput("showyourheight", "Show your height (blue)", FALSE),
      textInput("yourheight", label = h4("Enter your height (mm)")
                ,value=1750)
      ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      column(width = 12,
             box(
               title="All students", width=NULL,
               plotOutput("CLTplot1", height = 250),
               box(
                 
                 title="", 
                 width=NULL,
                 htmlOutput('summary')
             ),
             box(
               
               title="", 
               width=NULL,
               htmlOutput('summary2')
             )
             ) 
      )
      
      
      )
    )

  
    )



server <- function(input, output) {
  
  
  getSummary <- function() {
    mn <- 1711
    sd <- 93
    str0 <- paste('Your height is',input$yourheight,'mm',sep=' ')
    str1 <- paste('The population mean is',mn,'mm',sep=' ')
    str2 <- paste('The population standard deviation is',sd,'mm',sep=' ') 
    #above.or.below <- 'below'
    #if (input$yourheight > mn) {
    #  above.or.below <- 'above'
    #}
    #d <- (as.numeric(input$yourheight)-mn)/sd
    #d <- abs((round(100*d))/100)
    #str3 <- paste('Your height is',d,'standard deviations',above.or.below,
    #              'the mean',sep=' ')
   
    result <- paste(str0,'<br>',str1)
    
    return(result)
  }
  
  getSummary2 <- function() {
    
    ht <- as.numeric(input$yourheight)
    mn <- 1711
    sd <- 93
    
    diff <- mn - ht
    absdiff <- abs(diff)
    str0 <- paste('Your height is',absdiff,'mm',sep=' ')
    str1 <- paste('The population mean is',mn,'mm',sep=' ')
    str2 <- paste('The population standard deviation is',sd,'mm',sep=' ') 
    above.or.below <- 'below'
    if (ht > mn) {
      above.or.below <- 'above'
    }
    d <- (absdiff)/sd
    d <- round(100*d)/100

    str0 <- paste('Your height is',absdiff,'mm',above.or.below,
                  'the mean', sep=' ')
    str1 <- paste('One standard deviation is',sd,'mm',sep=' ')
    str2 <- paste(absdiff,'/',sd,'=',d,sep='')
    
    result <- paste(str0,'<br>',str1,'<br>',str2)
    
    return(result)
  }
   
    
  
  xbar <- 1711
  sd <- 93
  upp <- xbar + 3 * sd
  low <- xbar - 3 * sd
  x.breaks <- round(seq(xbar-3*sd,xbar+3*sd,sd))
    
    
  output$CLTplot1 <- renderPlot({ 
     
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)
                ) +
      stat_function(fun = dnorm, show.legend=F,
                    colour='red', 
                    args = list(mean = xbar, sd = sd)) + 
                    ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      xlab("Height")
       
    ht <- as.numeric(input$yourheight)
    
    
    if (input$showmean) {
      pointdata <- data.frame(
        x = c(xbar), 
        ypos = c(0)
      ) 
       
      
      p <- p + geom_vline(xintercept=xbar,colour='red') +
          annotate("text", label = "mean", x = pointdata$x, 
                   y= 0.00025, hjust=0,
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
                          shape = 4,
                          size = 5
                          
      ) +
        annotate("text", label = str, x = pointdata$x, 
                 y= 0.0005, hjust=0,
             size = 5, colour = "blue") 
      }
    }
    p  
  }) # end CLTplot1
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  )
  
  output$summary2 <- renderText({
    paste("<font size=4>",getSummary2(),
          "</font>")
  }
  )
  
   
  output$text1 <- renderText({ 
    paste("hello input is","<font color=\"#FF0000\"><b>", input$n, "</b></font>") })
  
}

shinyApp(ui, server)