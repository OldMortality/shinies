# app 10
# 
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)


ui <- dashboardPage( 
  dashboardHeader(title = "Guillain-Barré vs Normal conduction velocity",
                  titleWidth = 650),
  dashboardSidebar(useShinyjs()), 
  dashboardBody(
    
    fluidRow(
      column(width=6,
             box(title="Distal conduction velocity ",width=400,
               DT::dataTableOutput("thetable",height=450)
             )
          ),
      column(width=6,
             box(title="Distal conduction velocity",width=400,
                 plotOutput("GBPlot",height=450)
             )
          )),
    fluidRow(
      column(width=12,
             box(title="Summary",width=800,
                 htmlOutput('summary')
             )
      )
    )
     
  )
)



