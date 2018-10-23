# app 3. old
# You enter height of your group members, and shows how this
#   compares to the population

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(shinyBS)


ui <- dashboardPage(
  
  dashboardHeader(title = "Group height",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   
                   checkboxInput("shownormal", "Show Normal (red)", TRUE),
                   bsTooltip(id = "shownormal", 
                             title = "<u>Select to see the normal distribution in the plot.</u>", 
                             placement = "right", trigger = "hover",
                             options=list(datahtml="true" )),
                   checkboxInput("showmean", "Show mean (black)", TRUE),
                   
                   checkboxInput("showyourheight", "Show your group mean height (blue)", TRUE),
                   bsPopover(id = "showyourheight", title = "This is a popover",
                             content = paste0("You should read the ", 
                                              a("tidy data paper", 
                                                href = "http://vita.had.co.nz/papers/tidy-data.pdf",
                                                target="_blank")),
                             placement = "right", 
                             trigger = "hover", 
                             options = list(container = "body")
                   ),
                   checkboxInput("showmeanothers", "Show other groups mean height (yellow)", FALSE),
                   checkboxInput("showmembers", "Show your group members height (green)", TRUE),
                   
                   textInput("groupheights", label = h3("Enter heights (mm)"), value = "1800 1750")), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      column(width = 12,
             box(
               title="All students", width=NULL,
               plotOutput("CLTplot1", height = 250),
               box(
                 
                 title="Summary", 
                 width=NULL,
                 htmlOutput('summary')
               )) 
      )
      
      
    )
    
  )
  
  
)



server <- function(input, output) {
  
  
  #addTooltip(session, id = "shownormal", title = "This is an input.",
  #           placement = "left", trigger = "hover")
  # labdata.R creates combined.csv and removes NAs etc
  lab.data <- read.csv("combined.csv") 
  
  ## make up group means
  ss <- sample(1:10,size=nrow(lab.data),replace=TRUE,prob=rep(0.1,10))
  group.means <- vector()
  for (i in 1:10) {
    group.means[i] <- mean(lab.data[which(ss==i),"ht"])
  }
  
  
  getSummary <- function() {
    groupHeights <- input$groupheights
    print(groupHeights)
    print("=====")
    # vector of heights input
    vHeights <- as.numeric(strsplit(groupHeights,' ')[[1]])
    
    # population
    mn <- round(mean(as.numeric(lab.data$ht)))
    md <- round(median(as.numeric(lab.data$ht)))
    sd <- round(sqrt(var(as.numeric(lab.data$ht))))
    
    # group
    str_h3G <- "<h3>Your group</h3>"
    if (length(vHeights > 0)) {
      mnG <- round(mean(vHeights))
      mdG <- round(median(as.numeric(vHeights)))
      sdG <- round(sqrt(var(as.numeric(vHeights))))
      
      str0G <- paste('Your group mean height is',mnG,'mm',sep=' ')
      str1G <- paste('Your group median is',mdG,'mm',sep=' ')
      str2G <- paste('Your group standard deviation is',sdG,'mm',sep='  ')
      
      above.or.below <- 'below'
      if (mnG > mn) {
        above.or.below <- 'above'
      }
      d <- (mnG-mn)/sd
      d <- abs((round(100*d))/100)
      str3 <- paste('Your group height is',d,'standard deviations',above.or.below,
                    'the mean of all students',sep=' ')
      
      
    } else {
      str0G <- ""
      str1G <- ""
      str2G <- ""
      str3 <- ""
    }
    str_h3 <- "<h3>All students</h3>"
    str0 <- paste('The mean height of all students is',mn,'mm',sep=' ')
    str1 <- paste('The median height is',md,'mm',sep=' ')
    str2 <- paste('The standard deviation is',sd,'mm',sep=' ')
    
    
    
    
    result <- paste(str_h3G,'<br>',
                    str0G,'<br>',
                    str1G,'<br>',
                    str2G,'<br>',
                    str_h3,'<br>',
                    str0,'<br>',
                    str1,'<br>',
                    str2,'<br>',
                    str3
                    )
    
    return(result)
  }
  
  
  
  
  
  output$CLTplot1 <- renderPlot({
    
    ht <- lab.data$ht
    xbar <- mean(ht,na.rm=T)
    sbar=sqrt(var(ht,na.rm=T))
    
    df <- data.frame(ht=ht)
    print(df)
    p <- ggplot(df,aes(x=ht))  + 
      geom_density(color="black") +
      labs(x = "Height (mm)")
    if (input$shownormal) {
      p <- p + stat_function(fun=dnorm,
                             color="red",
                             args=list(mean=xbar,sd=sbar 
                             ))
    }
    if (input$showmean) {
      p <- p + geom_vline(xintercept=xbar, colour="black") 
    }
    if (input$showmeanothers) {
      for (i in 1:length(group.means)) {
        p <- p + geom_vline(xintercept=group.means[i], colour="yellow") 
      }
      p <- p + geom_vline(xintercept=xbar, colour="black") 
    }
    if (input$showyourheight) {
      groupHeights <- input$groupheights
      vHeights <- as.numeric(strsplit(groupHeights,' ')[[1]])
      mnG <- round(mean(vHeights))
      p <- p + geom_vline(xintercept=mnG, colour="blue") 
    }
    if (input$showmembers) {
      groupHeights <- input$groupheights
      vHeights <- as.numeric(strsplit(groupHeights,' ')[[1]])
      if (length(vHeights > 0)) {
        for (j in 1:length(vHeights)) {
          p <- p + geom_vline(xintercept=vHeights[j], colour="green")   
        }
      }
      
    }
    
    
    p  
  }) # end CLTplot1
  
  output$summary <- renderText(
    getSummary()
  )
  
}

shinyApp(ui, server)