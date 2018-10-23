
library(shiny)

# an app, with links to my shiny appss

ui = fluidPage(
    titlePanel("Shiny apps "),
    uiOutput("showdata"),
    uiOutput("central"),
    uiOutput("central2")
  )

server = function(input, output, session){
    url1 <- a("Show data", href="https://oldmortality.shinyapps.io/ShowData/")
    output$showdata <- renderUI({
      tagList("Plotting data:", url1)
    })
    url2 <- a("CLT", href="https://oldmortality.shinyapps.io/central/")
    output$central <- renderUI({
      tagList("Central limit theorem:", url2)
    })
    url3 <- a("CLT", href="https://oldmortality.shinyapps.io/CLTh/")
    output$central2 <- renderUI({
      tagList("Central limit theorem v2:", url3)
    })
    
}

shinyApp(ui = ui, server = server)
