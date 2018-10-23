#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) 

mydata = reactive({
  df = tryCatch(read.csv(text=readLines(input$filepath),header = T),
                error = function(e){
                  return(matrix('Placeholder',1,1))
                }
  )
  analysis = sum(df$weight)
  return = analysis
}

ui <- fluidPage(
inputPanel(
  fileInput("filepath", label = "Full path to appnexus csv export file"#, value = "~/Downloads/rawData.csv"
  ))
)


)

renderUI(
  selectInput("ip_selection",label = "Select Input",
              choices = mydata(),
              selected = mydata()[1]))

renderText(paste0("Reading file: ",input$filepath))
renderText("=====================================")

renderText(input$ip_selection)