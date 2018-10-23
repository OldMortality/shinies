library(shiny)
library(shinydashboard)
library(rhandsontable)


header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody(
  
  fluidRow(
    box(plotOutput("plot1")),
    box( title = "Controls", width=3,sliderInput("slider", "Number of observations:", 1, 10, 5) )),
  fluidRow( box(title = "Handsontable",rHandsontableOutput("hot") )
            
  ))

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  values  <-  reactiveValues()
  setHot  <-  function(x) values[["hot"]]  <-  x
  
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  output$hot = renderRHandsontable({
    if (!is.null(input$hot)) {
      DF  <-  hot_to_r(input$hot)
      setHot(DF)
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    } else {
      DF  <-  mtcars
      cols <- colnames(DF)
      DF$brand <- row.names(mtcars)
      DF <- DF[,c("brand", cols)]
      rhandsontable(DF, rowHeaders=FALSE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = TRUE, fixedColumnsLeft=1) %>%
        hot_validate_numeric(col = "mpg", allowInvalid=FALSE)
    }
  })
}

shinyApp(ui, server)