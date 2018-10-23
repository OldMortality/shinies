rm(list = ls())
library(shiny)
ui <- fluidPage(
  # All your styles will go here
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
  
  sliderInput("slider3", "Slider 3",min = 100, max = 20000, value = 5000, step= 200),
  
  sliderInput("slider1", "Slider 1",min = 0.1, max = 1, value = 0.4, step = 0.05),
  sliderInput("slider2", "Slider 2",min = 0.1, max = 1, value = 0.4, step = 0.05)                               
  
)
server <- function(input, output, session){}
shinyApp(ui = ui, server=server)