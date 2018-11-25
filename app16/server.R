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
  set.seed(440)
  
  g.pre <- data.frame(score = rbinom(girls.pre.n,size=9,prob=2/9),
                      sex = rep('girls',girls.pre.n),
                      col = rep('red',girls.pre.n))
  b.pre <- data.frame(score = rbinom(boys.pre.n,size=9,prob=1.7/9),
                      sex = rep('boys',boys.pre.n),
                      col = rep('blue',boys.pre.n))
  # make sure dotplots don't overlap
  g.pre$score <- g.pre$score - 0.08
  b.pre$score <- b.pre$score + 0.08
  
  
  g.post <- data.frame(score = rbinom(girls.post.n,size=9,prob=3/9),
                      sex = rep('girls',girls.post.n),
                      col = rep('red',girls.post.n))
  b.post <- data.frame(score = rbinom(boys.post.n,size=9,prob=1.3/9),
                      sex = rep('boys',boys.post.n),
                      col = rep('blue',boys.post.n))        
  g.post$score <- g.post$score - 0.08
  b.post$score <- b.post$score + 0.08
  
  
               
  pre <- rbind(g.pre,b.pre)
  post <- rbind(g.post,b.post)
   
  
   
  
  
  getSummary.pre <- function() {
    
    input$actionButtonID
    
    line0 <- paste("N is:",length(b.pre$score)+length(g.pre$score),sep=' ')
    
    line1 <- paste("The mean score for boys is:",
                    round(mean(b.pre$score),1),sep=' ')
    line2 <- paste("The mean score for girls is:",
                   round(mean(g.pre$score),1),sep=' ') 
         
    result <- paste(line0,line1,line2,sep="<br>")
    return(result)
  }
  
  getSummary.post <- function() {
    
    input$actionButtonID
    line0 <- paste("N is:",length(b.post$score)+length(g.post$score),sep=' ')
    line1 <- paste("The mean score for boys is:",
                   round(mean(b.post$score),1),sep=' ')
    line2 <- paste("The mean score for girls is:",
                   round(mean(g.post$score),1),sep=' ') 
    
    result <- paste(line0,line1,line2,sep="<br>")
    return(result)
  }
  
  output$summary.pre <- renderText(
    paste("<font size=4>",getSummary.pre(),
          "</font>")
  )
  
  output$summary.post <- renderText(
    paste("<font size=4>",getSummary.post(),
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
      p <- ggplot(data=post, aes(x = score,fill=sex,colour=sex)) +
        geom_dotplot(dotsize=dsize,alpha=0.6) +
        scale_x_continuous(limits=c(0,9),breaks=seq(0,9,1)) +
        scale_y_continuous(limits=c(0,1),breaks=NULL) +
        ylab("") + 
        xlab("laxity score")
        p
    
  })  
 
  
}
