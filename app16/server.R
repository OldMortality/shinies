# app 16 
# Task D.2 Joint laxity

#   

library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)



shinyServer <- function(input, output) {
  
  girls.pre.n <- 31
  girls.post.n <-176
  boys.pre.n <- 32 
  boys.post.n <- 66   
  
  g.pre <- data.frame(score = rbinom(girls.pre.n,size=9,prob=2/9),
                      sex = rep('girls',girls.pre.n),
                      col = rep('red',girls.pre.n))
  b.pre <- data.frame(score = rbinom(boys.pre.n,size=9,prob=1.7/9),
                      sex = rep('boys',boys.pre.n),
                      col = rep('blue',boys.pre.n))
             
  
  g.post <- data.frame(score = rbinom(girls.post.n,size=9,prob=3/9),
                      sex = rep('girls',girls.post.n),
                      col = rep('red',girls.post.n))
  b.post <- data.frame(score = rbinom(boys.post.n,size=9,prob=1.3/9),
                      sex = rep('boys',boys.post.n),
                      col = rep('blue',boys.post.n))        
  
               
  pre <- rbind(g.pre,b.pre)
  post <- rbind(g.post,b.post)
   
  
   
  
  
  getSummary <- function() {
    
    input$actionButtonID
    
         
    result <- paste("Gallia est omnis divisa in partes tres")
    return(result)
  }
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  
  output$dot.pre <- renderPlot({
    
      dsize <- 0.5
      p <- ggplot(data=pre, aes(x = score,fill=sex,colour=sex)) +
        geom_dotplot(dotsize=dsize,alpha=0.6) +
        scale_x_continuous(limits=c(0,9),breaks=seq(0,9,1)) +
        scale_y_continuous(breaks=NULL) +
        ylab("") + 
        xlab("laxity score")
    p
     
    
  }) 
  
  
  output$dot.post <- renderPlot({ 
      dsize <- 0.5
      p <- ggplot(data=post, aes(x = score,fill=col,colour=col)) +
        geom_dotplot(dotsize=dsize,alpha=0.6) +
        scale_x_continuous(limits=c(0,9),breaks=seq(0,9,1)) +
        scale_y_continuous(limits=c(0,1),breaks=NULL) +
        ylab("") + 
        xlab("laxity score")
        p
    
  })  
 
  
}
