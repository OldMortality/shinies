library(ggplot2)

sd <- 1
mu <- 0
x <- seq(mu-3*sd,mu+3*sd,0.01)
y <- dnorm(x)
z <- rnorm(length(x),mu,sd)
plot(y~x)
df <- data.frame(x=x,y=y,z=z)
ggplot(df, aes(x=x,y=y)) + geom_point() + 
  geom_histogram(aes(y=..density..),color="black", fill="white")
                 
  
ggplot(df, aes(x=x,y=y)) + geom_blank() + 
  geom_histogram(aes(z,stat(density)),
                 color="black",
                 binwidth=0.1)
  
p <- ggplot(df, aes(x=x,y=y)) + geom_point() 
pointdata <- data.frame(
  x = c(0.5,1.5), 
  ypos = c(0,0)
) 

p <- p + geom_point(data = pointdata, colour='red',
                    mapping = 
                      aes(x = x, y = ypos ),
                    show.legend=F,
                    shape = 4,
                    size = 7
                    
)
pointdata <- data.frame(
  x = c(0,1,2,3), 
  ypos = c(0,0,0,0)
)
p <- p + geom_point(data = pointdata, colour='red',
                    mapping = 
                      aes(x = x, y = ypos ),
                    show.legend=F,
                    shape = 4,
                    size = 7
                    
)

p

geom_histogram(color="black", fill="white"
  ) 


 
  
  
  #geom_text() +
  #annotate("text", label = "plot mpg vs. wt", x = 0.5, y = 0.1, size = 8, colour = "red")

p <- ggplot(df, aes(x = x,y=0)) + 
  geom_point(size=3) + theme(legend.position = "none") 
p



result <- data.frame(height=c(round(rnorm(90,1710,90))))
# 2 people who put in cm instead of mm
result$height[22] <- 170
result$height[77] <- 3500
# a hobbit
result$height[55] <- 1300
result

d <- data.frame(result)
p <- ggplot(d, aes(height)) + 
  scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
  geom_dotplot(dotsize=0.3)
p

d$height[77] = 1700
d$height[22] = 1700


mu <- 1711
sd <- 92 
upp <- mu + 3 * sd
low <- mu - 3 * sd
x.breaks <- round(seq(mu-3*sd,mu+3*sd,sd))

thisOne <- c(1750)
df <- data.frame(x=thisOne)
p <- ggplot(df, aes(x = thisOne,y=0)) +
  geom_point(colour='red') +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,
                     limits=c(low,upp)) +
  scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                     limits=c(-0.01,0.01))
p

x.breaks <- c(-3,-2,-1,0,1,2,3)
p <- ggplot(df, aes(x = 0,y=0 )) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = x.breaks,minor_breaks=NULL,limits=c(-10,10)) +
  scale_y_continuous(breaks = NULL,minor_breaks=NULL,
                     limits=c(-10,10)) + 
  ylab("") + s
  xlab("Sample mean") + 
  geom_point(x=0,y=0,colour='red')
p

lo <- rnorm(10)
up <- rnorm(10)
for (i in 1:10) {
  p <- p + geom_segment(x=lo[i],y=i,xend=up[i],yend=i)
}
p


g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g + geom_bar()

d <- data.frame(y=rbinom(1000,size=1,p=0.3),x=rep(1,1000))
p <- ggplot(data=d,aes(y)) + geom_bar() +
  xlab("Excercise") +
  ylab("Count") +
  scale_x_discrete(breaks = c(0,1),limits=c(0,1)) +
  scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
p

samp <- function() {
  return(rbinom(10,size=1,prob=0.5))
}

zero.count = 0
one.count = 0

pts <- data.frame(x = samp,y=rep(0,length(samp)))
for (i in 1:length(samp)) {
  if (samp[i]==0) {
    zero.count = zero.count + 1
    z = zero.count
  } else {
    one.count = one.count + 1
    z = one.count
  }
  print(z)
  onePoint <- data.frame(x=samp[i],y=z*10)
  #print(onePoint)
  p <- p + geom_point(data=onePoint,aes(x= x, y=y),
                       colour='black') 
}
p

df <- data.frame(y=rbinom(1000,size=1,p=0.3))

g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g + geom_bar()

d <- data.frame(x=rnorm(100,0,10),y=rep(1,100))
p <- ggplot(data=d,aes(y=x)) + 
  geom_boxplot(aes(group=y)) +
  scale_x_continuous(limits=c(-3,3),breaks=NULL)
p


data <- c(1750,1850)
d <- data.frame(height=data)
p <- ggplot(d, aes(height)) + 
  geom_histogram(bins=length(data) )+
  scale_y_continuous(breaks = NULL,minor_breaks=NULL)  
p 


thisOne1 <- tail(values$total1,showMean)
thisOne2 <- tail(values$total2,showMean)
df1 <- data.frame(x=rnorm(10,0,1),col='red')
df2 <- data.frame(x=rnorm(10,2,1),col='blue')
df <- rbind(df1,df2)
p <- ggplot(df, aes(x = x,y=0,colour=col )) +
    geom_point() 
p


lo <- -100
up <- 100
x.breaks <- seq(-100,100,10)


  df <- data.frame(x=31)
    p <- ggplot(df, aes(x = x,y=0)) +
      geom_point(size=0.3) +
      scale_x_continuous(limits=c(lo,up),
                         breaks = x.breaks,minor_breaks=NULL) + 
      scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
  p

  
  mu1 = 1800
  mu2 = 1700
  sd1 = 92
  sd2 = 92
  sd.hat <- sqrt(sd1^2 +sd2^2)
  x1 <- rnorm(300,mu1,92)
  x2 <- rnorm(300,mu2,92)
  diff <- x1 - x2
  
  x.breaks <- seq(-300,300,50)
  lo= -300
  up = -300
  df <- data.frame(x = diff)
  #p <- ggplot(df ) +
  #  geom_density(aes(x = x)) 
  #p
  x.breaks <- seq(-250,250,50)
  p <- ggplot(df ) +
    geom_histogram(aes(x = x),binwidth = 10) +
    scale_x_continuous(limits=c(-1250,1250),breaks=x.breaks)
  p                                         
  
  
  
  
  #show the red line
    
    s <- sd/sqrt(as.numeric(input$n))
    print(paste(':::::',sd1^2),sep=' ')
    sd.hat <- sqrt(sd1^2 + sd2^2)/sqrt(as.numeric(input$n))
    print(sd.hat)
    print("......")
    p <- p + stat_function(fun=dnorm,
                           color="red",
                           args=list(mean=mean(mu1-mu2), 
                                     sd=sd.hat)) 
  
    
N<- 10000
#samplesize
size= 10
bin.width=0.05
sd <- sqrt(0.3*0.7/size)
x <- rbinom(N,size=size,prob = 0.3)/10
hist(x,prob=F,1/bin.width)
for (i in seq(0,1,0.05)){
  points(i,bin.width * N * dnorm(i,0.3,sd=sd),col='red')
}



sampleMeans1 <-rnorm(120,1700,91)
sampleMeans2 <-rnorm(120,1600,91)

df1 <- data.frame(x=sampleMeans1)
df1$e <-'red'
df2 <- data.frame(x=sampleMeans2)
df2$e <-'blue'
df <- rbind(df1,df2)


p <- ggplot(df, aes(x = x,fill=e)) +
      scale_x_continuous(limits=c(-4,4)) + 
    scale_y_continuous(limits=c(0,1)) + 
      geom_density(alpha=0.5)
p

p <- ggplot(df, aes(x = x,fill=e,colour=e)) +
  scale_x_continuous(limits=c(-4,4)) + 
  scale_y_continuous(limits=c(0,1)) + 
  geom_dotplot(alpha=0.5) 
p

bin.width = 10
df <- df[which(df$e=='red'),]
p <- ggplot(df, aes(x = x,fill=e,colour=e)) +
  #scale_x_continuous(limits=c(-4,4)) + 
  
  geom_histogram(binwidth = bin.width,alpha=0.5,
                 position="dodge2"
                 ) 
  
  #facet_wrap(~e)
p
head(df)




{
  trueProb <- 0.3
N <- 500
d <- data.frame(y=rbinom(N,size=1,p=trueProb),x=rep(1,N))
p <- ggplot(data=d,aes(y)) + 
  geom_segment(aes(x = 0, y=0, xend = 0, yend = N*(1-trueProb),  colour = "red"),size=2) +
  geom_segment(aes(x = 1, y=0, xend = 1, yend = N*(trueProb),  colour = "red"),size=2) +
  xlab("Excercise") +
  ylab("") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks = c(0,1),limits=c(0,1)) +
  scale_y_continuous(breaks = NULL,minor_breaks=NULL) 


p

samp <- data.frame(y=rbinom(1,size=1,prob=0.3))
p <- p + geom_dotplot(data=samp,dotsize = 0.1)
p

p <- 

  # points for the sample
  zero.count = 0
  one.count = 0
  samp <- rbinom(10,size=1,prob=0.3)
  pts <- data.frame(x = samp,y=rep(0,length(samp)))
  for (i in 1:length(samp) ){
    if (samp[i]==0) {
      zero.count = zero.count + 1
      z = zero.count
    } else {
      one.count = one.count + 1
      z = one.count
    }
    onePoint <- data.frame(x=samp[i],y=z*10)
    p <- p + geom_point(data=onePoint,aes(x= x, y=y),
                        colour='black') 
    p

    
  }


  

{
  N <- 500
  d <- data.frame(y=rbinom(N,size=1,p=trueProb),x=rep(1,N))
  
  d <- data.frame(x=c(0,1),y=c(300,700))
  p <- ggplot(d,aes(x))
  p <- p + geom_bar()
  p
  
  
  
  p <- ggplot(data=d,aes(y)) + 
    geom_segment(aes(x = 0, y=0, xend = 0, yend = N*(1-trueProb),  colour = "red"),size=2) +
    geom_segment(aes(x = 1, y=0, xend = 1, yend = N*(trueProb),  colour = "red"),size=2) +
    xlab("Excercise") +
    ylab("") +
    theme(legend.position = "none") +
    scale_x_discrete(breaks = c(0,1),limits=c(0,1)) +
    scale_y_continuous(breaks = NULL,minor_breaks=NULL) 
  p
}
p <- p + geom_segment(aes(x=0.5,y=0,xend=0.5,yend=0.3,colour='black'))
p
  
df1 <- data.frame(class=c(rep(0,300),rep(1,700)))
g <- ggplot(df1, aes(class))
# Number of cars in each class:
g + geom_bar(width=0.1,colour='red',fill='red') + 
  scale_x_discrete(breaks = c(0,1)) +
  geom_segment(x=0,xend=0,y=0,yend=100,colour='black')


g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g + geom_bar()


df1 <- data.frame(class=c(rep(0,700),rep(1,300)))
p <- ggplot(df1, aes(class,colour='red',fill='red')) + 
  scale_x_discrete(breaks = c(0,1),limits=c(0,1)) +
  scale_y_continuous(breaks = NULL,minor_breaks=NULL) +
  theme(legend.position = "none") +
  ylab("") +
  xlab("Excercise")

p <- p  + geom_bar(width=0.1) 
p

dfs <- data.frame(y=c(0,1,1,1,1,0,0,0,0,1))
#print(dfs)
p <- p + geom_dotplot(data=dfs,aes(x=y),dotsize = 1,colour='black',fill='black')

y0 =30
y1 = 30

p <- ggplot() +
  geom_segment(aes(x = 0, y=0, xend = 0, yend = y0),colour='black') +
  geom_segment(aes(x = 1, y=0, xend = 1, yend = y1),colour='black') 


p    


df <- read.csv('/Users/micheldelange/Documents/shiny/app9/Lab3data.csv',header=T)
colnames(df)
range(df$reflexpathlength)
dropm <- which(df$reflexpathlength < 300)
df <- df[-dropm,]
dropm <- which(df$meanvoluntarylatency > 1000)
df <- df[-dropm,]

range(df$reflexpathlength)

range(df$meanreflexlatency)

range(df$meanvoluntarylatency)

x1 <- c(0,1)
y1 <- c(0,1)
col <- c('blue','red')

df <- data.frame(x=x1,y=y1,col=col)
df
df.blue <- df[1,] 
df.blue

ggplot(df,aes(x=x,y=y,colour=col)) + geom_point()

library(gridExtra)
p1 <- ggplot(df,aes(x=x,y=y)) + geom_point(aes(colour=col))
p2 <- ggplot(df,aes(x=x,y=y)) + geom_point(colour=col)
grid.arrange(p1, p2, ncol=2) 

df <- data.frame(x=rnorm(100))
p <- ggplot(df,aes(x=x)) + geom_histogram()
p <- p + geom_vline(xintercept=c(0,1),colour='red')

p

d <- data.frame(x=c(NA,NA),y=c(1,1))
d
dropm <- which(is.na(d[,1]))

df1 <- data.frame(var=rnorm(100))
df2 <- data.frame(var=rnorm(100,2))
df1$type <- 'V'
df2$type <- 'F'
df3 <- rbind(df1,df2)
ggplot(df3,aes(y=var,x=type)) + geom_boxplot()

