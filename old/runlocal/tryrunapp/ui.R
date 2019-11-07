library(shiny)
library(rstudioapi)
ui <- fluidPage(
  
  actionButton("app1", "app1"),
  actionButton("app2", "app2")
  
)

server <- function(input, output) {
  observeEvent(input$app1, {
    source('~/Documents/shiny/tryrunapp/runapp1.R')
  })
  observeEvent(input$app2, {
    #rstudioapi::jobRunScript(path = '~/Documents/shiny/tryrunapp/runapp1.R')
    source('~/STATS/rscripts/helloworld.R')
  })
}

shinyApp(ui, server)