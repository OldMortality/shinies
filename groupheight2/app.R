# app 3.
# You enter height of your group members, and shows how this
#   compares to the population, and to other groups

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(shinyBS)


ui <- dashboardPage(
  
  dashboardHeader(title = "Group height",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   
                   checkboxInput("showmean", "Show population mean (red)", FALSE),
                   
                   checkboxInput("showyourgroupheight", "Show your group mean height (blue)", FALSE),
                   checkboxInput("showothergroups", "Show the mean of other groups (green)", FALSE),
                   bsPopover(id = "showyourgroupheight", title = "This is a popover",
                             content = paste0("You should read the ", 
                                              a("tidy data paper", 
                                                href = "http://vita.had.co.nz/papers/tidy-data.pdf",
                                                target="_blank")),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")
                   ),
                   textInput("yourgroupheight", label = h4("Enter the mean height of your group (mm)"), value = "1800"),
                   textInput("othergroup1", label = h5("Enter the mean of 3 other groups (mm)"), value = "1790"),
                   textInput("othergroup2", label = NULL, value = "1791"),
                   textInput("othergroup3", label = NULL, value = "1792")),

                   
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 12,
             box(
               title="All students", width=NULL,
               plotOutput("CLTplot1", height = 250)
             ))),
    fluidRow(
      column(width = 6,      
             box( 
                 title="", 
                 width=NULL,
                 htmlOutput('summary')
               )),
      column(width = 6,      
             box( 
               title="", 
               width=NULL,
               htmlOutput('summary2')
             ))
    )
    
  )
  
  
)



server <- function(input, output) {
  
  
  #addTooltip(session, id = "shownormal", title = "This is an input.",
  #           placement = "left", trigger = "hover")
 
  mu <- 1711
  sd <- 92
   
  
  getSummary <- function() {
    groupHeights <- input$yourgroupheight
    print(groupHeights)
    print("=====")
    # vector of heights input
    vHeights <- as.numeric(strsplit(groupHeights,' ')[[1]])
    
    # population
    
    # group
    str_h3G <- "<b>Your group</b><br>"
    if (length(vHeights > 0)) {
      mnG <- round(mean(vHeights))
      sdG <- round(sqrt(var(as.numeric(vHeights))))
      
      str0G <- paste('Your group mean height is',mnG,'mm',sep=' ')
      #str2G <- paste('Your group standard deviation is',sdG,'mm',sep='  ')
      
      above.or.below <- 'below'
      if (mnG > mu) {
        above.or.below <- 'above'
      }
      d <- (mnG-mu)/sd
      d <- abs((round(100*d))/100)
      #str3 <- paste('Your group height is',d,'standard deviations',above.or.below,
      #              'the mean of all students',sep=' ')
      
      
    } else {
      str0G <- ""
      str2G <- ""
      #str3 <- ""
    }
    str_h3 <- "<br><b>Population</b><br> "
    str0 <- paste('The mean height of all students is',mu,'mm',sep=' ')
    
    str2 <- paste('The standard deviation is',sd,'mm',sep=' ') 
    result <- paste(str_h3G,'<br>',
                    str0G,'<br>',
                    #str2G,'<br>',
                    str_h3,'<br>',
                    str0,'<br>',
     
                    str2,'<br>'
                    #,str3
    )
    
    return(result)
  }
  
  getSummary2 <- function() {
    str0 <- "<ul>"
    str1 <- "<li>Each sample mean provides an estimate of the population mean."
    str2 <- "<li>Note they can be quite variable."
    str3 <- "</ul>"
    result <- paste(str0,"<br>",str1,"<br>",str2,"<br>",str3)
    return(result) 
  }
    
  
  
  upp <- mu + 3 * sd
  low <- mu - 3 * sd
  x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))
  
  
  output$CLTplot1 <- renderPlot({ 
    
    p <- ggplot(data = data.frame(x = c(low, upp)), aes(x)) +
      stat_function(fun = dnorm, show.legend=F,colour='red',n = 101, args = list(mean = mu, sd = sd)) + ylab("") +
      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      xlab("Height")
    
    
    if (input$showmean) {
      pointdata <- data.frame(
        x = c(mu), 
        ypos = c(0)
      ) 
      
      p <- p + geom_vline(xintercept=mu,colour='red') +
        #annotate("text", label = "mean", x = pointdata$x, 
        #         y= 0.00025, hjust=0,
        #         size = 5, colour = "red") +
          annotate("text", label = "mean", x = pointdata$x, 
                   y= 0., hjust=0,
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
      print(str)
      pointdata <- data.frame(
        #x = c(as.numeric(input$yourgroupheight)), 
        x <- mnG,
        ypos = c(0)
      ) 
      p <- p + geom_point(data = pointdata, colour='blue',
                          mapping = 
                            aes(x = x, y = ypos ),
                          show.legend=F,
                          shape = 4,
                          size = 5
                          
      ) #+
        #annotate("text", label = str, x = pointdata$x, 
        #         y= 0.00025, hjust=0,
        #         size = 3, colour = "blue") 
      
    }
    if (input$showothergroups) {
      h1 <- as.numeric(input$othergroup1)
      h2 <- as.numeric(input$othergroup2)
      h3 <- as.numeric(input$othergroup3)

      vHeights <- c(h1,h2,h3)
      print(vHeights)
      print(length(vHeights))
      
      pointdata2 <- data.frame(
           
          x2 <- vHeights,
          ypos2 = rep(0,length(vHeights))
        )
      if (length(vHeights) > 0) {
        p <- p + geom_point(data = pointdata2, colour='green',
                            mapping = 
                              aes(x = x2, y = ypos2 ),
                            show.legend=F,
                            shape = 4,
                            size = 5)
      }
      
    }
    
    p  
  }) # end CLTplot1
  
  output$summary <- renderText(
    getSummary()
    
  )
  
  output$summary2 <- renderText(
    getSummary2()
    
  )
  
}

shinyApp(ui, server)