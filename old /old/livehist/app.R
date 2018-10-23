# this is the textarea example to
#   replace handsontable

library(ggplot2)

library(shiny)



shinyApp(

fluidPage(

fluidRow(

column(

4,

textAreaInput(

"textArea", "Enter data:", 

width = "100%", height = "500px",

value = paste0(sample(1:10, 10, replace = TRUE), collapse = "\n")

)

),

column(

8,

plotOutput("plot", height = "600px")

)

)

),

function(input, output) {

output$plot <- renderPlot({

# convert input to numeric values

x <- as.numeric(strsplit(input$textArea, "\\n")[[1]])

validate(

need(x, "Enter data into the text area"),

need(length(x) > 1, "Enter at least two rows of data into the text area")

)

dt <- data.frame(x = x)

ggplot(dt, aes(x)) + 

geom_histogram()

})

}

)
