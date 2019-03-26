#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        actionButton("line",label="add point")
      ),
      
      # Show plot 
      mainPanel(
         plotOutput("distPlot")
      )
   )
)


server <- function(input, output) {
  
   x <- faithful[,2] 
   p <- ggplot(data=faithful,(aes(x=eruptions,y=waiting)) ) +
     geom_point()

   observeEvent(input$line, {
     
     x <- runif(1,min = 0.5,max=5)
     y <- runif(1,min = 50,max=80)
     print(paste('adding point',x,y,sep=' '))
     p <<- p + geom_point(x=x,y=y,colour='red')
     
   })
   
   output$distPlot <- renderPlot({
      
     input$line
     print('updating plot')
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

