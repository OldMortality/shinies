library(shiny)
library(ggplot2)

ui <- fluidPage(
  actionButton("random", "Random"),
  plotOutput("hist"),
  plotOutput("hist_ggplot")
)

server <- function(input, output, session) {
  values <- reactiveValues(total = 0)
  allvalues <- reactiveValues(all=0)
  
  observeEvent(input$random, {
    values$random <- rnorm(1,0,1)
    allvalues$all <- c(allvalues$all,values$random)
  })
  

  output$hist <- renderPlot({
    
    if (length(values$random > 0)) {
      hist(allvalues$all)
    }
    
    #ggplot(NewThing, aes(x=fred)) + 
    #  geom_histogram(color="black", fill="white")
    
    #p<-ggplot(allmeansamp$df, aes(x=x)) + 
    #  geom_histogram(color="black", fill="white")
    #p
    
    #hist(allvalues$all)
  })
  
  output$hist_ggplot <- renderPlot({
    
    df <- data.frame(x = allvalues$all)
    ggplot(df, aes(x=x)) + 
        geom_histogram(color="black", fill="white") +
        #geom_text() +
        annotate("text", label = "plot mpg vs. wt", x = 0.5, y = 0.1, size = 8, colour = "red")
    
    
    })
}

shinyApp(ui = ui, server = server)