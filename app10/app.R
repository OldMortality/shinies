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




server <- function(input, output) {
   
  
  df <- read.csv('velocity.csv',header=T,
                        stringsAsFactors = F)
  
  
  data.all <- data.frame(velocity=df$distal_cond_vel,
                         disease=df$disease,
                         stringsAsFactors = F)
  
  data.all[which(data.all$disease=='NGB'),"disease"] <- "Normal"
  data.all[which(data.all$disease=='GB'),"disease"] <- "Guillain Barré"
                          
  
  
  # on reflection, forget about severity. use disease
  
   
  output$thetable <- DT::renderDataTable(DT::datatable({
    
    data.all
  }))
  
  
  getSummary <- function() {
    m1 <- mean(data.all[which(data.all$disease=='Normal'),"velocity"])
    m2 <- mean(data.all[which(data.all$disease=='Guillain Barré'),"velocity"])
    m1r <- round(m1,1)
    m2r <- round(m2,1)
    l1 <- paste("The sample mean of the Guillain Barré group is: ",m1r)
    l2 <- paste("The sample mean of the normal group is: ",m2r)
    diff <- m1r-m2r
    l3 <- paste("The difference is: ",round(diff,1))
    ci <- 'dont know right now'
    s1 <- var(data.all[which(data.all$disease=='Normal'),"velocity"])
    s2 <- var(data.all[which(data.all$disease=='Guillain Barré'),"velocity"])
    s1.n <- s1/length(which(data.all$disease=='Normal'))
    s2.n <- s2/length(which(data.all$disease=='Guillain Barré'))
    se <- sqrt(s1.n + s2.n)
    lower <- round(diff - 1.96 * se,1)
    upper <- round(diff + 1.96 * se,1)
    ci <- paste("[",lower,',',upper,"]")
    l4 <- paste("The 95% confidence interval for the difference is",ci)
    result <- paste(l1,l2,l3,l4,sep="<br>")
  }
  
  output$summary <- renderText(
    getSummary()
  )
  
  
  output$GBPlot <- renderPlot({ 
    
      p <- ggplot(data.all,aes(x=disease,y=velocity)) + geom_point() +
        ylab('Distal conduction velocity (ms)') +
        scale_x_discrete(labels=c('GB'='Guillain Barré',
                                  'NGB'='not Guillain Barré'))
      p
    
    })
  
  
}

shinyApp(ui, server)