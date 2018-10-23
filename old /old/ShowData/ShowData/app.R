library(shiny)
library(stringr)
library(rhandsontable)

data1 <- read.csv('weightheight.csv',header=T)
data2 <- cars
data3 <- iris

# Define UI for application that draws a histogram
ui <- function(request) {
    fluidPage(
    
   # Application title
   titlePanel("Plotting data"),
   
     
   sidebarLayout(
      sidebarPanel( 
        selectInput("data", h3("Data"), 
                     choices = list("Weight & height" = 1, 
                                    "Cars" = 2,
                                    "Iris" = 3
                     ), selected = 1),
        selectInput("plot.type", h3("Graph type"), 
                    choices = list("Histogram" = 1, 
                                 "Scatter" = 2,
                                 "boxplot" = 3
                    ), selected = 1),
      
        uiOutput("B_ui")
      ),
      mainPanel(
        fluidRow(
          rHandsontableOutput("hot", width = 150),
          plotOutput(outputId = "distPlot")
        )
      )
   )
)
}
# end of UI
  
  
# Define server logic required 
server <- function(input, output, session) {
  
  cache_tbl = NULL
  
  onRestore(function(state) {
    tmp = state$input$hot
    tmp$data = jsonlite::fromJSON(
      jsonlite::toJSON(tmp$data), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })
  
  
  datasetInput <- reactive({
    #switch(input$data,
    #       "1" = data1,
    #       "2" = data2,
    #       "3" = data3)
    result <- data1
    if (input$data == "2") {
      result <- data2
    }
    if (input$data == "3") {
        result <- data3
    }
    result$highlight <- F
    result
  })
  
  
  
  prevdata <- 1
  
  dataChanged = reactive({
    if (input$data != prevdata) {
      prevdata <<- input$data
      result <- T
    } else {
      result <- F
    }
    result
  })
  
  
  data = reactive({
    if (dataChanged() ) { 
      DF = datasetInput()
      print('i am here')
    }
    else
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else 
    if (!is.null(cache_tbl)) {
      DF = hot_to_r(cache_tbl)
      cache_tbl <<- NULL
    } else {
      print('final else')
      DF = datasetInput()
    }
    DF
  })
  
  
  #observeEvent(input$data, {
  #  
  #  data = datasetInput()
  #  
  #})
  
  listvars <- reactive(  { 
    if (input$data == 1) {
      h = 1
      w = 2
      s = 3
      result <- list(height=h,weight=w,sex=s)
    } else if (input$data == 2) {
      s = 1
      d = 2
      result <- list(speed=s,dist=d)
    } else {
      sl = 1
      sw = 2
      pl = 3
      pw = 4
      sp = 5
      result <- list(sepal.length=sl,
                     sepal.width=sw,
                     petal.length=pl,
                     petal.width=pw,
                     species=sp)
    }
    return( result)
  }
  )
  # end of listvars
  
  output$hot <- renderRHandsontable({
    DF = data()
   
    if (!is.null(DF) ) {
      rhandsontable(DF, width = 600, height = 150) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1)
    }
  })
 
  
  output$B_ui <- renderUI({
    if (input$plot.type == "1") {
      selectInput("variable", h3("Variables"), 
                choices = listvars()
                , selected = 1)
    } else {
      if (input$plot.type == "2") {
      tagList(
      selectInput("variable.x", h4("x variable"), 
                  choices = listvars()
                  , selected = 1),
      selectInput("variable.y", h4("y variable"), 
                  choices = listvars()
                  , selected = 2)
      )
      } else {
        if (input$plot.type == "3") {
          # boxplot
          tagList(
            selectInput("variable.x", h4("x variable"), 
                        choices = listvars()
                        , selected = 1),
            selectInput("variable.y", h4("y variable"), 
                        choices = listvars()
                        , selected = 2)
          ) 
        }
      }
    }
  })
  
  output$distPlot <- renderPlot({
    #print('redrawing the plot')
    #data <- data()
    data <- hot_to_r(input$hot)
    # histogram
    if (input$plot.type == 1) {
      colNumber <- as.numeric(input$variable) 
      if (length(colNumber) == 0) {
        colNumber <- 1
      }
      colName <- colnames(data)[colNumber]
      hist(data[,colNumber],30,xlab= colName,
             main=str_c('Histogram of ',colName))
      abline(v=data[which(data$highlight==T),colName],col='red')
      #print(which(data$show == T))
    }
     
    # scatter or boxplot
    if (input$plot.type == 2 | input$plot.type == 3 ) {
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
      if (input$plot.type == 2) {
        plot(data[,colNumber.y]~data[,colNumber.x],
            xlab= colName.x,
            ylab= colName.y,
            main=str_c('Scatter ',colName.y,' ~ ', colName.x))  
      # highlighted ones
      highlighted <- data[which(data$highlight==T),c(colName.x,colName.y)]
      xs <- highlighted[,colName.x]
      ys <- highlighted[,colName.y]
      
      for (i in 1:dim(highlighted)[1]) {
        
        points(xs[i],ys[i],col='red',pch='X')
        
      }
      } else {
        boxplot(data[,colNumber.y]~data[,colNumber.x],
             xlab= colName.x,
             ylab= colName.y,
             main=str_c('Boxplot ',colName.y,' ~ ', colName.x)) 
        # highlighted ones
        highlighted <- data[which(data$highlight==T),c(colName.x,colName.y)]
        xs <- highlighted[,colName.x]
        ys <- highlighted[,colName.y]
        
        for (i in 1:dim(highlighted)[1]) {
          
          points(xs[i],ys[i],col='red',pch='X')
          
        }
      }
    }
  }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)


