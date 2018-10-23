library(shiny)

ui <- fluidPage(
  "Total:",
  textOutput("total", inline = TRUE),
  actionButton("add1", "Add 1"),
  actionButton("add5", "Add 5"),
  plotOutput("hist")
)

server <- function(input, output, session) {
  values <- reactiveValues(total = 0)
  allvalues <- reactiveValues(all=0)
  
  observeEvent(input$add1, {
    values$total <- values$total + 1
  })
  observeEvent(input$add5, {
    values$total <- values$total + 5
    allvalues$all <- c(allvalues$all,values$total)
  })
  output$total <- renderText({
    values$total
  })
  
  output$hist <- renderPlot({
    NewThing <- data.frame()
    NewThing$ <- reactive(allvalues()$all)
    print(NewThing$fred)
    #NewThing <- reactive({ function(allvalues()$all) })
    #NewThing<-reactive({ function(MyReacitveCSVdata()$colname) })
    ggplot(NewThing, aes(x=fred)) + 
        geom_histogram(color="black", fill="white")
      
      #p<-ggplot(allmeansamp$df, aes(x=x)) + 
      #  geom_histogram(color="black", fill="white")
      #p
      
    hist(allvalues$all)
  })
}

shinyApp(ui = ui, server = server)