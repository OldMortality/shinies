# app 1
# Showing the population height distribution,
#   and allows you to find your own height
#
library(shinydashboard)
library(ggplot2) 

ui <- dashboardPage(  
  dashboardHeader(title = "Task A.1 and A.2 How tall are HUBS191 students?",
                  titleWidth = 800),
  dashboardSidebar(
      checkboxInput("showmean", "Show population mean (red)", FALSE),
      checkboxInput("showyourheight", "Show your height (blue)", FALSE),
      textInput("yourheight", label = h4("Enter your height (mm)")
                ,value=1750)
      ), 
  dashboardBody( 
    fluidRow( 
      column(width = 12,
              box(
                title="Distribution of height for the population of HUBS191 students", 
                width=NULL,
                plotOutput("plot1", height = 350)), 
              box( 
                title="", 
                width=NULL,
                htmlOutput('summary')
              )
            )
      )
  )
)

server <- function(input, output) {
  
  xbar <- 1711
  sd <- 93
  upp <- xbar + 3 * sd
  low <- xbar - 3 * sd
  x.breaks <- round(seq(xbar-3*sd,xbar+3*sd,sd))
  
  getSummary <- function() {
    ht <- as.numeric(input$yourheight) 
    diff <- xbar - ht
    absdiff <- abs(diff)
    d <- (absdiff)/sd
    d <- round(100*d)/100 
    str0 <- paste('Population mean height is',xbar,'mm',sep=' ')
    str1 <- paste('Population standard deviation is',sd,'mm',sep=' ') 
    str2 <- paste('Your height is',input$yourheight,'mm',sep=' ')
    str3 <- paste('Distance from the mean is',absdiff,'mm', sep=' ')
    str4 <- paste('Distance from the mean in standard deviations is',
                  paste(absdiff,'/',sd,'=',sep=''),
                  d,sep=' ')
    result <- paste(str0,'<br>',str1,'<br>',str2,'<br>',str3,'<br>',str4)
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
      geom_segment(x=xbar+2*sd,xend=xbar+3*sd,
                   y=0.004,yend=0.004,
                   colour='black',
                   arrow = arrow(length=unit(0.30,"cm"), ends="both", type = "closed")
                   ) +
      annotate("text", label = "1 standard deviation", 
               x = xbar+ 2.1*sd, 
               y= 0.0042, hjust=0,
               size = 5, colour = "black")
       
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
    paste("hello input is","<font color=\"#FF0000\"><b>", input$n, "</b></font>") })
  
}

shinyApp(ui, server)