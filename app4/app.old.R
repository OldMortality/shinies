# app 4
# height plots and table, 90 people with 
#   2 errors and 1 true outlier
#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)



ui <- dashboardPage(
  
  dashboardHeader(title = "Task A4. Heights for a lab stream (n=90)",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(), 
                   checkboxInput("showhist", "Histogram", FALSE),
                   checkboxInput("showbox", "Boxplot", FALSE),
                   checkboxInput("showdot", "Dotplot", FALSE) 
  ), 
  dashboardBody(
    fluidRow(
      column(width = 6,
             box(title="Heights of 90 HUBS191 students",width=NULL,
                 rHandsontableOutput("hot", width = 200),
                 htmlOutput('summary', height = 100)
             ),
             
             box(
               title="Histogram of height", width=NULL,
               conditionalPanel(
                 condition = "input.showhist",
                 plotOutput("histogram", height = 300)
               )
             )
      ),
      column(width=6,
             box(
               title="Boxplot of height", width=NULL,
               conditionalPanel(
                 condition = "input.showbox",
                 plotOutput("boxplot", height = 373)
               )
             ),
             box(
               title="Dotplot of height", width=NULL,
               conditionalPanel(
                 condition = "input.showdot",
                 plotOutput("dotplot", height = 300)
               )
             ) 
      )
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
    result <- data.frame(height=c(round(rnorm(90,1710,90))))
    # 2 people who put in cm instead of mm
    result$height[22] <- 170
    result$height[77] <- 3500
    # a hobbit
    result$height[55] <- 1300
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
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot) 
      n <-sum(!is.na(DF$height))
      mn <- round(mean(DF$height,na.rm=T),1)
      sd <- round(sqrt(var(DF$height,na.rm=T)),1)
      line0 <- paste("Sample size n =",n,sep=" ")
      line1 <- paste("Sample mean =",mn,sep=" ")
      line2 <- paste("Sample standard deviation =",sd,sep=" ")      
      result <- paste(line0,line1,line2,sep='<br>')
      return(result)
    }
  }
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  
  output$histogram <- renderPlot({
    
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot)
    d <- data.frame(data)
    p <- ggplot(d, aes(height)) +
      geom_histogram(binwidth=25,fill="white",colour='black') +
      scale_x_continuous(limits=c(1500,2100),breaks=seq(1500,2100,100)) +
      scale_y_continuous(breaks = seq(0,20),minor_breaks=NULL) +
      ylab("Frequency") +
      xlab("Height (mm)")
    p 
    #hist(data$height,main="",xlab="height")
    
  }) # end histogram
  
  output$boxplot <- renderPlot({
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot)
    d <- data.frame(data)
    p <- ggplot(data, aes(y=height)) + 
      geom_boxplot(width=0.2) +
      scale_x_continuous(breaks = NULL,minor_breaks=NULL,
                         limits=c(-0.5,0.5)
      )  +
      ylab("Height (mm)")
    
    p 
    
    #boxplot(data$height,main="",xlab="height")
    
    
  }) # end boxplot
  
  output$dotplot <- renderPlot({
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot) 
    d <- data.frame(data)
    p <- ggplot(d, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot() +
        ylab("Frequency") +
        xlab("Height (mm)")  
    p  
  }) # end dotplot
  
  
  
  
}

shinyApp(ui, server)