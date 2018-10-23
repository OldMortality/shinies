# app 4
# height plots and table, 90 people with 
#   2 errors and 1 true outlier
#   

library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)



ui <- dashboardPage(
  
  dashboardHeader(title = "Plots of group height",
                  titleWidth = 450),
  dashboardSidebar(useShinyjs(),
                   
                   checkboxInput("showhist", "Histogram", FALSE),
                   checkboxInput("showbox", "Boxplot", FALSE),
                   checkboxInput("showdot", "Dotplot", FALSE)                   
                   
                   
  ), 
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width = 6,
             box(title="Height",width=NULL,
                 rHandsontableOutput("hot", width = 200)
             ),
             
             box(
               title="histogram", width=NULL,
               conditionalPanel(
                 condition = "input.showhist",
                 plotOutput("histogram", height = 300)
               )
             )
      ),
      column(width=6,
             box(
               title="boxplot", width=NULL,
               conditionalPanel(
                 condition = "input.showbox",
                 plotOutput("boxplot", height = 300)
               )
             ),
             box(
               title="dotplot", width=NULL,
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
  
  
  output$histogram <- renderPlot({
    
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot)
    d <- data.frame(data)
    p <- ggplot(d, aes(height)) + geom_histogram() +
      scale_y_continuous(breaks = NULL,minor_breaks=NULL)  
    
    p 
    #hist(data$height,main="",xlab="height")
    
  }) # end histogram
  
  output$boxplot <- renderPlot({
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot)
    d <- data.frame(data)
    p <- ggplot(d, aes(y=height)) + geom_boxplot() +
      scale_x_continuous(breaks = NULL,minor_breaks=NULL)  
    
    
    p 
    
    #boxplot(data$height,main="",xlab="height")
    
    
  }) # end boxplot
  
  output$dotplot <- renderPlot({
    if(is.null(input$hot)) return(NULL)
    data <- hot_to_r(input$hot)
    #dotchart(data$height,main="",xlab="height")
    d <- data.frame(data)
    p <- ggplot(d, aes(height)) + 
        scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
        geom_dotplot()
    
    
    p 
    
    
  }) # end boxplot
  
  
  
  
}

shinyApp(ui, server)