library(shiny)
library(shinyTable)

server <- function(input, output, session) {
  
  
  rv <- reactiveValues(cachedTbl = NULL)
  
  output$theplot <- renderPlot({
    #add dependence on button
    input$actionButtonID
    print('theplot') 
      
    x <- isolate(as.numeric(rv$cachedTbl[,1]))
     
    print(x)
    y <- as.numeric(x)
    hist(y,30)
  })
  
  output$tbl <- renderHtable({
    if (is.null(input$tbl)){
      
      #fill table with 0
      #tbl <- matrix(seq(1,12), nrow=6, ncol=2)
      tbl <- data.frame(list(height=rep(1750,10)))
      # mdl
      rownames=NULL
      #colnames= c('wilma','fred')
      #dimnames(tbl) <- list(rownames, colnames)
      
      
      rv$cachedTbl <<- tbl
      return(tbl)
    } else{
      rv$cachedTbl <<- input$tbl
      return(input$tbl)
    }
  })  
  
  output$tblNonEdit <- renderTable({
    
    #add dependence on button
    input$actionButtonID
    
    #isolate the cached table so it only responds when the button is pressed
    isolate({
      rv$cachedTbl
    })
  })    
}


ui <- shinyUI(pageWithSidebar(
  
  headerPanel("shinyTable with actionButton to apply changes"),
  
  sidebarPanel(
    helpText(HTML("A simple editable matrix with a functioning update button. 
                  Using actionButton not submitButton. 
                  Make changes to the upper table, press the button and they will appear in the lower. 
                  <p>Created using <a href = \"http://github.com/trestletech/shinyTable\">shinyTable</a>."))
  ),
  
  # Show the simple table
  mainPanel(
    #editable tablesh
    htable("tbl",
           colHeaders = 'provided'),
     
    #update button
    actionButton("actionButtonID","apply table edits"),
    #to show saved edits
    tableOutput("tblNonEdit"),
    plotOutput("theplot",height=200)
  )
))

shinyApp(ui = ui, server = server)