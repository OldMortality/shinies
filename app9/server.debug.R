# app 9
# Plots of 2 variables
#   

library(shiny)
library(shinydashboard)


library(shinyTable)

 

shinyServer <- function(input, output) {
  
  
  rv <- reactiveValues(cachedTbl = NULL)   
  
  output$tbl <- renderHtable({ 
     
    if (is.null(input$tbl)){  
      tbl = data.frame(r1=c(1750,rep("",9)) 
      ) 
      rv$cachedTbl <<- tbl
      return(tbl)
    } else{
      rv$cachedTbl <<- input$tbl
      return(input$tbl)
    }
  })   
}