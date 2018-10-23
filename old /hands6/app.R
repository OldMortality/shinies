
library(shiny)
library(rhandsontable)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      rHandsontableOutput("hot") 
    )
  )
)


server <- function(input, output) {
  
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) {
      DF = data.frame(height=c(1750,rep(NA,9)))
    } else {
      DF = hot_to_r(input$hot)
    }
    rhandsontable(DF,selectCallback=F)  
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

