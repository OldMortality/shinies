f <- read.csv("/Users/micheldelange/Documents/shiny/data/combined_clean.csv",header=T)
colnames(f)
f$exercise
f$heightmm
dropm <- which(f$height==3500)
f <- f[-dropm,]
dropm <- which(f$height==3000)
f <- f[-dropm,]
dropm <- which(f$heightmm < 1000)
f <- f[-dropm,]
range(f$heightmm,na.rm=T)
boxplot(f$heightmm~f$exercise)

table(f$exercise)
notex <- f[which(f$exercise=="N"),]
ex <- f[-which(f$exercise=="N"),]
dim(ex)
mean(ex$heightmm,na.rm=T)
sqrt(var(ex$heightmm,na.rm=T))
mean(notex$heightmm,na.rm=T)
sqrt(var(notex$heightmm,na.rm=T))

par(mfrow=c(2,1))
plot(ex$heightmm)
plot(notex$heightmm)

m <- lm(f$heightmm~ f$exercise)
summary(m)
anova(m)

table(f$location)
dim(f)

boxplot(f$heightmm~f$location)
m <- lm(f$heightmm~ f$location)
summary(m)
anova(m)

northies <- f[which(f$location=='N'),]
southies <- f[which(f$location=='S'),]
mean(northies$heightmm,na.rm=T)
sqrt(var(northies$heightmm,na.rm=T))
mean(southies$heightmm,na.rm=T)
sqrt(var(southies$heightmm,na.rm=T))

par(mfrow=c(1,1))

curve(dnorm(x,1712,92),xlim=c(1400,2000))

plot('',xlim=c(1400,2000),ylim=c(0,0.005))

for (i in seq(1400,2000,0.1)) {
  points(i,dnorm(i,1712,92),col='black',pch='.')
  points(i,dnorm(i,1671,114),col='red',pch='.')
}
