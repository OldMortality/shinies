
mu1 = 1712
mu2 = 1671
sd1 = 92
sd2 = 92 
lower <- mu1 - 3 * sd1
upper <- mu1 + 3 * sd1
x.breaks <- round(seq(lower,upper,sd1))


topPlot <- ggplot(data = data.frame(x = c(lower, upper)), aes(x)) +
  stat_function(fun = dnorm, show.legend=F,
                colour='red', 
                args = list(mean = mu1, sd = sd1)) + 
  stat_function(fun = dnorm, show.legend=F,
                colour='blue', 
                args = list(mean = mu2, sd = sd2)) + 
  ylab("") +
  scale_x_continuous(breaks = x.breaks,minor_breaks=NULL) +
  scale_y_continuous(breaks = NULL,minor_breaks=NULL, 
                     limits=c(0,0.005)) +
  theme(legend.position = "none") +
  xlab("Height") +  
  geom_segment(x=1420,xend=1470,y=0.0048,yend=0.0048,colour='blue') +
  annotate("text", x = 1600, y = 0.0048, size=5,
           label = "Does not exercise frequently") +
  geom_segment(x=1780,xend=1830,y=0.0048,yend=0.0048,colour='red') +
  annotate("text", x = 1929, y = 0.0048, size=5,
           label = "Exercises frequently")  

s1 <- rnorm(10,mu1,sd1)
s2 <- rnorm(10,mu2,sd2)


par(mar= c(.1, .1, .1, .1))
x.breaks <- round(seq(mu1-3*sd1,mu1+3*sd1,sd1))
plot('',xlim=c(lower,upper),ylim=c(0,0.005),
     ylab="",xlab="",xaxt="n",yaxt="n",
     )
adjustcolor("blanchedalmond",alpha.f = 0.3)
axis(1, at = seq(mu1-3*sd1,mu1+3*sd1,by=sd1))
curve(dnorm(x,mean=mu1,sd=sd1),lower,upper,col='red',add=T)
curve(dnorm(x,mean=mu2,sd=sd2),lower,upper,col='blue',add=T)
points(c(s1,s2),rep(0,20),
       pch=21,
       col=c(rep('blue',10),rep('red',10)),
       bg=c(rep('blue',10),rep('red',10))
       )
#rect(-1,0,mu1+4*sd1,0.01,col=adjustcolor("blanchedalmond",alpha.f = 0.3))



par(bg='#EBEBEB')
par(bg='grey')

plot(s1~s2)
