# app 14
# enter 10 velocities.show dot plot and summary
#   

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(rhandsontable)



ui <- dashboardPage(
  
  dashboardHeader(title = "Group velocity",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs()
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
             box(title="Enter your group data",width=NULL,
                 rHandsontableOutput("hot", width = 200)
             ),
             box(  
               title="Summary", 
               width=NULL,
               htmlOutput('summary',height = 300),
               height=300
               
             )
             
            ),
      
      column(width = 6,       
             box(
               title="Dotplot", 
               width=NULL,
                plotOutput("dotplot", height = 300)
                
             ))
       
         
             
       
      
    )
  )
)


server <- function(input, output) {
  
  df <- data.frame(x=rnorm(100))
  
  cache_tbl = NULL
  
  onRestore(function(state) {
    tmp = state$input$hot
    tmp$data = jsonlite::fromJSON(
      jsonlite::toJSON(tmp$data), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })
  
  datasetInput <- reactive({
    # make up historical data
    result <- data.frame(velocity=c(50,rep(NA,9)))
    
    result
  })
  
  
  data = reactive({
    
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
    DF = data()
    
    if (!is.null(DF) ) {
      rhandsontable(DF, width = 200, height = 300) %>%
        hot_cols(fixedColumnsLeft = 1) 
      #%>%hot_rows(fixedRowsTop = 1)
    }
  })
  
  
  getSummary <- function() {
    
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot)
    d <- data.frame(data)
    d.m <- round(mean(data$velocity,na.rm=T),1)
    d.v <- round(var(data$velocity,na.rm=T),1)
    d.sd <- round(sqrt(d.v),1)
    d.me <- round(median(data$velocity,na.rm=T),1)
    d.min <- round(range(data$velocity,na.rm=T)[1],1)
    d.max <- round(range(data$velocity,na.rm=T)[1],1)
    n <- length(which(!is.na(data$velocity)))
    
    l.ci <- ""
    if (n>1) {
      ci.low <- d.m + qt(0.025,df=n) * d.sd/sqrt(n)
      ci.upp <- d.m + qt(0.925,df=n) * d.sd/sqrt(n)
      ci.low <- round(ci.low,1)
      ci.upp <- round(ci.upp,1)
      l.ci <- paste("A 95% confidence interval for the mean velocity is",
                    "from",ci.low,"to",ci.upp)
     
    }
    
    l1 <- paste("The mean is:",d.m,sep=' ')
    l2 <- paste("The variance is:",d.v,sep=' ')
    l3 <- paste("The standard deviation is:",d.sd,sep=' ')
    l4 <- paste("The median is:",d.me,sep=' ')
    l5 <- paste("The lowest velocity is:",d.min,sep=' ')
    l6 <- paste("The highest velocity is:",d.max,sep=' ')
    l7 <- l.ci
    
    result <- paste(l1,l2,l3,l4,l5,l6,l7,sep="<br>")
    return(result)
    
    return("result")
  }
  
  output$summary <- renderText(
    getSummary()
  )
  
  
  
  output$dotplot <- renderPlot({
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot) 
    d <- data.frame(data)
    p <- ggplot(d, aes(velocity)) + 
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
      geom_dotplot()
    
    
    p 
    
    
  }) # end dotplot
  
  
  
  
}

shinyApp(ui, server)