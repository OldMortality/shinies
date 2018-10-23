
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
      rHandsontableOutput("hot") #,
      #plotOutput("histogram")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) {
      DF = data.frame(height=c(1750,rep(NA,9)))
    } else {
      DF = hot_to_r(input$hot)
    }
    rhandsontable(DF)  
  })
  
  xbar <- 1711
  sd <- 93
  x.breaks <- round(seq(xbar-3*sd,xbar+3*sd,sd))
  
  
  #output$histogram <- renderPlot({
  #  
  #  if (!is.null(input$hot)) {
  #    DF = hot_to_r(input$hot)
  #    p <- ggplot(DF, aes(height)) + 
  #      geom_histogram(bins=5) +
  #      scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
  #      scale_y_continuous(breaks = NULL,minor_breaks=NULL)
  #    
  #    p 
  #  }
  #}) # end histogram
}

# Run the application 
shinyApp(ui = ui, server = server)

