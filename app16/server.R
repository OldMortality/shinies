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
                      sex = rep('females',girls.pre.n),
                      col = rep('red',girls.pre.n))
  b.pre <- data.frame(score = rbinom(boys.pre.n,size=9,prob=1.7/9),
                      sex = rep('non-females',boys.pre.n),
                      col = rep('blue',boys.pre.n))
  
  
  g.post <- data.frame(score = rbinom(girls.post.n,size=9,prob=3/9),
                      sex = rep('females',girls.post.n),
                      col = rep('red',girls.post.n))
  b.post <- data.frame(score = rbinom(boys.post.n,size=9,prob=1.3/9),
                      sex = rep('non-females',boys.post.n),
                      col = rep('blue',boys.post.n))        
  
  pre <- rbind(g.pre,b.pre)
  post <- rbind(g.post,b.post)
   
  
   
  
  
  getSummary.pre <- function() {
    
    input$actionButtonID
    
    line0 <- paste("N =",length(b.pre$score)+length(g.pre$score),sep=' ') 
    line1 <- paste("The mean score for females = ",
                   round(mean(g.pre$score),1),sep=' ') 
    line2 <- paste("The mean score for non-females =",
                   round(mean(b.pre$score),1),sep=' ')
    line3 <- paste("The difference in sample means = ",
                   round(mean(g.pre$score)-mean(b.pre$score),1))
    t <- t.test(g.pre$score,b.pre$score)
    lower <- round(t$conf.int[1],1)
    upper <- round(t$conf.int[2],1)
    line4 <- paste("A 95% Confidence inverval for the difference is (",
                   lower,'to',upper,')',sep=' ')
    result <- paste(line0,line1,line2,line3,line4,sep="<br>")
    return(result)
  }
  
  getSummary.post <- function() {
    
    input$actionButtonID
    line0 <- paste("N =",length(b.post$score)+length(g.post$score),sep=' ')
    line1 <- paste("The mean score for females =",
                   round(mean(g.post$score),1),sep=' ') 
    line2 <- paste("The mean score for non-females =",
                   round(mean(b.post$score),1),sep=' ')
    line3 <- paste("The difference in sample means = ",
                   round(mean(g.post$score)-mean(b.post$score),1))
    t <- t.test(g.post$score,b.post$score)
    lower <- round(t$conf.int[1],1)
    upper <- round(t$conf.int[2],1)
    line4 <- paste("A 95% Confidence inverval for the difference is (",
                   lower,'to',upper,')',sep=' ')
    result <- paste(line0,line1,line2,line3,line4,sep="<br>")
    return(result)
  }
  
  output$summary.pre <- renderText(
    if (input$showpre) {
      paste("<font size=4>",getSummary.pre(),
          "</font>")
    } 
  )
  
  output$summary.post <- renderText(
    if (input$showpost) {
      paste("<font size=4>",getSummary.post(),
          "</font>")
    }
  )
  
  
   
  
  
    output$dot.pre <- renderPlot({
      
      p <- ggplot(data=pre, aes(x = sex,y=score)) +
        geom_dotplot(dotsize=0.40,
                     color = 'blue',
                     fill = 'blue',
                     stackdir="center",
                     binaxis = 'y') +
        xlab("") + 
        ylab("laxity score") +
        scale_y_continuous(
          limits=c(0,9),
          breaks=seq(0,9),
          minor_breaks=seq(0,9)  
        ) +
        theme_gray((base_size = 20))
      p
     
    
  }) 
  
  
 
  
  output$dot.post <- renderPlot({ 
    p <- ggplot(data=post, aes(x = sex,y=score)) +
      geom_dotplot(dotsize=0.40,
                   color = 'blue',
                   fill = 'blue',
                 stackdir="center",
                 binaxis = 'y') +
      xlab("") + 
      ylab("laxity score") +
      scale_y_continuous(
        limits=c(0,9),
        breaks=seq(0,9),
        minor_breaks=seq(0,9) 
      )  +
      theme_gray((base_size = 20))
    p
  })
  
}
