# app 10
# 
#
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
             box(title="Guillain-Barré",
               DT::dataTableOutput("disease",height=450)
             )
          ),
      column(width=6,
             box(title="Normal",
               DT::dataTableOutput("normal",height=450)
             )
          )),
     fluidRow(
     column(width=12,
             box(title="Distal conduction velocity",
               plotOutput("GBPlot",height=450)
                )
          )
     
      )
  )
)




server <- function(input, output) {
   
  
  df <- read.csv('velocity.csv',header=T,
                        stringsAsFactors = F)
  df[which(df$severity=="Norm"),"severity"] <- "Normal"
  df[which(df$severity=="Normal"),"severity"] <- "Mild"
  df$velocity <- df$distal_cond_vel
  df[which(df$severity==""),"severity"] <- "not GB"
  # this puts the plot in the right order
  df$severity <- factor(df$severity,
                        levels=c('not GB','Mild','Moderate','Severe'))
  # for the data table in the UI
  df.disease  <- df[which(df$severity !='not GB'),]
  data.disease <- data.frame(velocity=df.disease$distal_cond_vel,
                             type=df.disease$severity)
  data.notgb  <- df[which(df$severity =='not GB'),]
  data.norm <- data.frame(velocity=data.notgb$distal_cond_vel,
                          type=data.notgb$severity)
  
  
  # on reflection, forget about severity. use disease
  
   
  output$disease <- DT::renderDataTable(DT::datatable({
    
    data.disease
  }))
  
  
  output$normal <- DT::renderDataTable(DT::datatable({
    
    data.norm
  }))
  
  output$GBPlot <- renderPlot({ 
    
      p <- ggplot(df,aes(x=disease,y=velocity)) + geom_point() +
        ylab('Distal conduction velocity (ms)') +
        scale_x_discrete(labels=c('GB'='Guillain Barré',
                                  'NGB'='not Guillain Barré'))
      p
    
    })
  
  
}

shinyApp(ui, server)