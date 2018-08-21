library(ggplot2)

## lab 3

f <- read.csv("/Users/micheldelange/Documents/shiny/data/lab3data.csv",
              header=T)
colnames(f)
table(f$Stream)
hist(f$Reflex.path.length..mm.)
hist(f$Mean.reflex.latency..ms.)
hist(f$Voluntary.path.length..mm.)
hist(f$Mean.voluntary.latency..ms.,30)


## lab 4

path = "/Users/micheldelange/Documents/shiny/data/lab4/csv"
files <- list.files(path)
lab.data <- data.frame()
for (i in 1:length(files)) {
  fname <- files[i]
  f <- read.csv(str_c(path,'/',fname))[,seq(1:7)]
  if (i==1) {
    lab.data <- f
  } else {
    lab.data <- rbind(data,f)
  }
}
colnames(lab.data)
n <- as.numeric(lab.data$Height..mm.)
which(n==3)
lab.data$Height..mm.[772]
min(n,na.rm=T)
lab.data$Height..mm.[772] <- 3000
n <- as.numeric(lab.data$Height..mm.)
min(n,na.rm=T)

lab.data$ht <- as.numeric(as.character(lab.data$Height..mm.))
dropm <- which(is.na(lab.data$ht) | lab.data$ht > 2400 |
                 lab.data$ht < 1200)
lab.data <- lab.data[-dropm,]


write.csv(lab.data,str_c(path,"/","combined.csv"))
# this is the one we use in shiny
path

hist(as.numeric(lab.data$Height..mm.),100)
lab.data[which(lab.data$Height..mm.==""),"Height..mm."] <- NA
lab.data[which(lab.data$Height..mm.=="."),"Height..mm."] <- NA
min(as.numeric(lab.data$Height..mm.),na.rm=T)

height <- as.numeric(lab.data$Height..mm.)
xbar <- mean(height,na.rm=T)
sbar=sqrt(var(height,na.rm=T))


df <- data.frame(height=height)
print(df)
p <- ggplot(df,aes(x=height)) + geom_blank() + 
  geom_histogram(aes(height,stat(density)),
                 color="black",
                 binwidth=0.1) +
  geom_vline(xintercept=xbar, colour="red") +
  annotate("text", label = 'fred', x = xbar, y = 0.3, size = 4, colour = "red") 

p <- ggplot(df,aes(x=height))  + 
  geom_density(color="black") +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=xbar,sd=sbar 
                )) 
p  


p <- p +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=xbar,sd=sbar 
                )) 
p

mn <- mean(df$height,na.rm=T)
sd <- sqrt(var(df$height,na.rm=T))
hist(df$height,freq=F)
for (t in seq(500,2500,10)) {
  points(t,dnorm(t,mean=mn,sd=sd),col='red')
}
x <- rnorm(1000,mean=xbar,sd=sbar)
hist(x,freq=F)



f <- read.csv("/Users/micheldelange/Documents/shiny/rscripts/combined.csv",
              header=T)
colnames(f)
plot(f$ht~f$Do.you.exercise...Never.Rarely..Occasionally..Frequently.)
table(f$Where.are.you.from....North.I.South.I.Other)
table(f$Do.you.exercise...Never.Rarely..Occasionally..Frequently.)
f$where <- f$Where.are.you.from....North.I.South.I.Other
f$island <- NA
f[which(grepl('outh',f$where)),"island"] <- "south"
f[which(grepl('ort',f$where)),"island"] <- "north"
f[which(grepl('nor',f$where)),"island"] <- "north"
f[which(grepl('hawk',f$where)),"island"] <- "north"

table(f$island,useNA='always')
table(f$where,useNA='always')

df <- data.frame(h = f$ht, i = f$island)
plot(h~i,useNA="no",data=df)

f$ex <- f$Do.you.exercise...Never.Rarely..Occasionally..Frequently.
f$EX <- toupper(f$ex)
table(f$EX)
f[which(grepl('CCAS',f$EX)),"EX"] <- "OCCASIONALLY"
      
