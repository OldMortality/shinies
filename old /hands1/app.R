library(shiny)
library(rhandsontable)

ui = shinyUI(fluidPage(
  
  titlePanel("Handsontable"),
  
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      rHandsontableOutput("hot"),
      plotOutput("hist")
    )
  )
))

server = function(input, output) {
  
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) {
      DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
                      dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                      stringsAsFactors = F)
    } else {
      DF = hot_to_r(input$hot)
    }
    rhandsontable(DF, useTypes = T) #%>%
      # hot_table(readOnly = TRUE) # ok
      #hot_col(col = 1, readOnly = FALSE) 
  })
  
  output$hist <- renderPlot({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      hist(DF$val)
    }
  })
}

shinyApp(ui = ui, server = server)