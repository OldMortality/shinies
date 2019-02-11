# app 3.
# You enter height of your group members, and shows how this
#   compares to the population, and to other groups

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2) 
library(shinyBS)


shinyUI <- dashboardPage(
  
  dashboardHeader(title = "Several sample means",
                  titleWidth = 850),
  dashboardSidebar(useShinyjs(),
                   tags$head(
                     # link to the css stylesheet. It is in the www folder.
                     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                   ),
                   checkboxInput("showmean", "Show population mean (red)", FALSE),
                   checkboxInput("showyourgroupheight", "Show your group mean height (blue)", FALSE),
                   checkboxInput("showothergroups", "Show the mean of other groups (green)", FALSE),
                   #bsPopover(id = "showyourgroupheight", title = "This is a popover",
                  #           content = paste0("You should read the ", 
                  #                            a("tidy data paper", 
                  #                              href = "http://vita.had.co.nz/papers/tidy-data.pdf",
                  #                              target="_blank")),
                  #           placement = "right", 
                  #           trigger = "hover", 
                  #           options = list(container = "body")
                  # ),
                   textInput("yourgroupheight", label = h4("Enter the mean height of your table group (mm)"), value = "1800"),
                   textInput("othergroup1", label = h4("Enter the mean of 3 other groups (mm)"), value = "1790"),
                   textInput("othergroup2", label = NULL, value = "1791"),
                   textInput("othergroup3", label = NULL, value = "1792")),
  dashboardBody(
    # 
    fluidRow(
      column(width = 12,
             box(
               title="Distribution of height for the population of HUBS191 students",
               width=NULL,
               plotOutput("plot1", height = 350)
             ))),
    fluidRow(
      column(width = 6,      
             box( 
                 title="", 
                 width=NULL,
                 htmlOutput('summary'),
                 height=300
               )),
      column(width = 6,      
             box( 
               title="", 
               width=NULL,
               htmlOutput('summary2'),
               height=300
             ))
    )
    
  )
  
  
)


