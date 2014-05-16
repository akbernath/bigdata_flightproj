###  Andrew Bernath
###  ST599 - Big Data Analysis
###  Project 2 - 25 year flight data

##   Sample graphics
setwd("D:/School/Spring 14/ST599/flightproj/bigdata_flightproj")

library(ggplot2)
library(dplyr)

##  Read in data and refine frame
samp.dat <- read.csv("Data/sampledata.csv")
rownames(samp.dat) <- samp.dat$X
samp.dat <- samp.dat[,-1]
head(samp.dat)

##  Set subset of 25 year only airlines and re-organize frame
airlines.25 <- samp.dat[,c(3,4,7,8,15,16,49,50,51,52,55,56)]
names <- c("AA","AS","DL","UA","US","WN")
data.25 <- data.frame(rep(0,150),rep(0,150),rep(0,150),rep(0,150))
data.25[,1] <- rep(1989:2013,6)
data.25[,2] <- c(rep(names[1],25),rep(names[2],25),
                 rep(names[3],25),rep(names[4],25),
                 rep(names[5],25),rep(names[6],25))
data.25[,3] <- c(airlines.25[,1],airlines.25[,3],
                 airlines.25[,5],airlines.25[,7],
                 airlines.25[,9],airlines.25[,11])
data.25[,4] <- c(airlines.25[,2],airlines.25[,4],
                 airlines.25[,6],airlines.25[,8],
                 airlines.25[,10],airlines.25[,12])
colnames(data.25) <- c("year","carrier","mean","se")
attach(data.25)

## Plot parameters
dodge <- position_dodge(.1)

##  The plot
ggplot(data.25, aes(x=year,y=mean,color=carrier,group=carrier))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0, position=dodge)+
  geom_smooth(method="loess", position=dodge, se=FALSE, span=.2)+
  geom_point(size=4, position=dodge, shape=21, fill="white")+
  labs(title="Sample Means for Arrival Delay with Standard Errors",
       x="Year", y="Mean Arrival Delay (min)")+theme_bw()+
  theme(plot.title=element_text(size=rel(2)),
        axis.title.x=element_text(size=rel(1.5)),
        axis.title.y=element_text(size=rel(1.5)),
        legend.position="bottom")
  
