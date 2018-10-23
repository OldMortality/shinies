# tryhandsontable
# height vs finger length - your group
library(shinydashboard)
#library(shinyjs)
library(ggplot2)
library(DT)
library(rhandsontable)


ui <- dashboardPage(
  
  
  
  dashboardHeader(title = "Tryhands3",
                  titleWidth = 550),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      column(width = 12,
             
             box(title="Enter your group's data",
                 rHandsontableOutput("hot")
             )#,
             #box( 
            #   title="",
               #plotOutput("thePlot") )
      )
    )
  )
)


server <- function(input, output) {
  
    
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) {
      DF = data.frame(finger.length=c(70,rep(NA,9)),
                      height=c(1750,rep(NA,9)))
    } else {
      DF = hot_to_r(input$hot)
    }
    rhandsontable(DF) 
  })
  
  
  #output$thePlot <- renderPlot({
  ##  if (!is.null(input$hot) ) {
  #    DF = hot_to_r(input$hot)
  #    p <- ggplot(data=DF,aes(x=finger.length,y=height)) + 
  #      geom_point()
  #    p
  #  }
    
  #}) # end thePlot
  
  
  
}

shinyApp(ui, server)