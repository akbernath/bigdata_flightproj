# 0a: install packages, include necessary ones

# install.packages("RPostgreSQL")
# install.packages("dplyr")
# install.packages("reshape2")

library(dplyr)
library(reshape2)

# 0b: access database

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime",
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)

flights <- tbl(ontime, "flights")
# as.tbl(head(flights))
# 
# # 0c:
# 
# planes <- group_by(flights, uniquecarrier)
# delay <- summarise(planes, count = n())
# tail(select(delay, -2))
# 
# # doesn't work! try "collect"
# 
# col.tst <- proc.time()
# test <- collect(delay)
# tail(test)
# proc.time() - col.tst
# 
# # fortunately, gives number of unique aircraft
# # unfortunately, takes a good while! (91.68s avg, 1 execute)
# # changing (select(delay,-2)) to (delay) shaves ~33 seconds
# # additionally, gives object type VECSXP; cannot use certain
# # handy functions (length(), n_distinct())
# 
# # We set a new goal here: for the largest airline volume
# # (WN), we want to create a table of mean delay time
# # (mean(arrdelay + depdelay)) from 2000-2005 by year.
# # We can then refine this to by month, depending on timing.
# 
# year.time <- proc.time()
# flt.small <- filter(flights, year == 2000 ||
#                       year == 2001, uniquecarrier=="WN")
# flt.yr.small <- group_by(flt.small,year)
# flt.sum.small <- summarise(flt.yr.small,
#                            meandelay = mean(arrdelay+depdelay))
# sum.saved <- collect(flt.sum.small)
# arrange(sum.saved, year)
# proc.time() - year.time
# 
# # mean runtime: 226.24s (2 attempts)
# # now to check for FIVE airlines; is it a linear increase?
# # we naively expect a linear increase of 2.5x, roughly
# # if it takes roughly 600s, we're right
# # if it takes MORE, we have a problem
# # if it takes LESS, we're off to a great start
# 
# year.time <- proc.time()
# flt.small <- filter(flights, year == 2000 ||
#                       year == 2001 || year == 2002 ||
#                       year == 2003 || year == 2004)
# flt.wn.small <- filter(flt.small, uniquecarrier=="WN")
# flt.yr.small <- group_by(flt.wn.small,year)
# flt.sum.small <- summarise(flt.yr.small,
#                            meandelay = mean(arrdelay+depdelay))
# sum.saved <- collect(flt.sum.small)
# arrange(sum.saved, year)
# proc.time() - year.time
# 
# # mean runtime: 338.1s (1 attempt) [OSU wifi]
# # this is significantly less than a linear increase!
# 
# # now we go for the BIG one:

year.time <- proc.time()

flt.grp <- group_by(flights,uniquecarrier,year)
flt.grp.sum <- summarise(flt.grp,
                         arrmean = mean(arrdelay), arrsd = sd(arrdelay))
sum.saved <- collect(flt.grp.sum)
sum.arr <- as.data.frame(arrange(arrange(sum.saved, year),uniquecarrier))
pmu.out <- acast(sum.arr, year~uniquecarrier, value.var="arrmean") 
psd.out <- acast(sum.arr, year~uniquecarrier, value.var="arrsd")
write.csv(pmu.out, "Data/popMean.csv")
write.csv(psd.out, "Data/popSd.csv")

proc.time() - year.time

# mean runtime: 384.87s (1 attempt)
# now to create an aggregate table

PopMeans <- read.csv("Data/popMean.csv")
PopSds <- read.csv("Data/popSd.csv")
SampDat <- read.csv("all_strat.csv")

# using heather's code to find a sorted list of unique carriers:

planes <- group_by(flights, uniquecarrier)
delay <- summarise(planes, count = n())
unique.airline<-collect(delay)
base.coln <-unique.airline$uniquecarrier
base.rown <- PopMeans[,1]

heig <- length(base.rown) - 2
leng <- length(base.coln)*4
matnum <- rep(numeric(heig), leng)
mat <- matrix(matnum, ncol = leng)
final.tab <- as.data.frame(mat)

# now to input our data!

j=0;
while (j < leng/4){
  final.tab[4*j+1] = PopMeans[3:27,j+2]
  final.tab[4*j+2] = PopSds[3:27,j+2]
  final.tab[4*j+3] = SampDat[,2*j+2]
  final.tab[4*j+4] = SampDat[,2*j+3]
  j=j+1;
}

# we have a working data frame! now to make it readable

rownames(final.tab) <- base.rown[3:27]

j=0;
while (j < leng/4) {  
  colnames(final.tab)[4*j+1] = paste(base.coln[j+1], "_pop.mean", sep="")
  colnames(final.tab)[4*j+2] = paste(base.coln[j+1], "_pop.sd", sep="")
  colnames(final.tab)[4*j+3] = paste(base.coln[j+1], "_samp.mean", sep="")
  colnames(final.tab)[4*j+4] = paste(base.coln[j+1], "_samp.sd", sep="")
  j=j+1;
}

write.csv(final.tab, "Data/aggregate.csv")

# we've gotten the data into one place; time to get some GRAPHS
# modifying heather's code to do this:

aggr <- read.csv("Data/aggregate.csv")
aggr[is.na(aggr)] <- 0

# install.packages("Hmisc", dependencies=T)
library("Hmisc")

# for(i in 0:29){
#   name<-base.coln[i+1]
#   pop.mean <- aggr[, 4*i + 2]
#   pop.se <- aggr[, 4*i + 3]
#   samp.mean <- aggr[, 4*i + 4]
#   samp.se <- aggr[, 4*i + 5]
#   d.p = data.frame(
#     year  = base.rown[3:27],
#     mean = pop.mean,
#     sd = pop.se
#   )
#   d.s = data.frame(
#     year = base.rown[3:27],
#     mean = samp.mean,
#     sd = samp.se
#   )
#   Sys.sleep(0.1)
#   plot(d.s$year, d.s$mean, type="n",ylim=c(-5,18),main=paste(name,"Exploratory Plots for Strata Estimates",sep=" "))
#   with (data = d.s
#         ,expr = errbar(year, mean, mean+sd, mean-sd, add=T, pch=1, cap=.05)
#   )
#   Sys.sleep(0.1)
#   plot(d.p$year, d.p$mean, type="n",ylim=c(-5,18),main=paste(name,"Exploratory Plots for Strata Estimates",sep=" "))
#   with (data = d.p
#         ,expr = errbar(year, mean, mean+sd, mean-sd, add=T, pch=1, cap=.05)
#   )
#   lines(base.rown[3:27],samp.mean,lty=2)
# }

# the SD for a population graph is ENORMOUS, enough to cover the display!
# maybe we shouldn't use this part in particular...
# still, it shows how close heather's sampling estimates were!

# moving on to the next section, graphing means:
old.par <- par(mfrow=c(1,2))
for(i in c(0:13,15:29)){
  name<-base.coln[i+1]
  pop.mean <- aggr[, 4*i + 2]
  samp.mean <- aggr[, 4*i + 4]
  mean.pop.final <- pop.mean[2:25]
  mean.pop.initial<-pop.mean[1:24]
  mean.samp.final <- samp.mean[2:25]
  mean.samp.initial <- samp.mean[1:24]
  mean.pop.change <- mean.pop.final - mean.pop.initial
  mean.samp.change <- mean.samp.final - mean.samp.initial
  
  #carry.se<-sqrt(strat.store[,even])
  year  = c(1990:2013)
  mean.p = mean.pop.change
  mean.s = mean.samp.change
  
  # add error bars (without adjusting yrange)
  means <- c(mean.s,mean.p)
  pdf(paste("Graphs/meanDiff_",name,".pdf",sep=""))
  par(mfrow=c(2,1))
  plot(year, mean.s, type="n",ylim=c(min(means),max(means)),
       xlab="Year",ylab="Sample Mean", 
       main=paste("Change in Mean Delay for ",name," Sample",sep=""))
  #with (data = d
  #      ,expr = errbar(year, mean, mean+sd, mean-sd, add=T, pch=1, cap=.05)
  #)
  lines(year,mean.s,lty=2,col="red")
  abline(h=0,lty=2)
  Sys.sleep(0.1)
  plot(year, mean.p, type="n",ylim=c(min(means),max(means)),
       xlab="Year",ylab="Population Mean",
       main=paste("Change in Mean Delay for ",name," Population",sep=""))
  #with (data = d
  #      ,expr = errbar(year, mean, mean+sd, mean-sd, add=T, pch=1, cap=.05)
  #)
  lines(year,mean.p,lty=2,col="red")
  abline(h=0,lty=2)
  Sys.sleep(0.1)
  dev.off()
}
par(old.par)
graphics.off()

# and finally, median change!

all.pop.change <- c()
all.samp.change <- c()
q25 <- c()
q75 <- c()

for(i in 0:29){
  name<-base.coln[i+1]
  pop.mean <- aggr[, 4*i + 2]
  samp.mean <- aggr[, 4*i + 4]
  mean.pop.final <- pop.mean[2:25]
  mean.pop.initial<-pop.mean[1:24]
  mean.samp.final <- samp.mean[2:25]
  mean.samp.initial <- samp.mean[1:24]
  mean.pop.change <- mean.pop.final - mean.pop.initial
  mean.samp.change <- mean.samp.final - mean.samp.initial
  for(j in 1:24){
    if(as.numeric(mean.pop.change[j])==0){
      mean.pop.change[j]=NA
    }
    if(as.numeric(mean.samp.change[j])==0){
      mean.samp.change[j]=NA
    }
  }
  all.pop.change<-c(all.pop.change,round(as.numeric(quantile(mean.pop.change,.5,na.rm=TRUE)),3))
  pop.q25 <- c(q25, round(as.numeric(quantile(mean.pop.change, 0.25, na.rm=TRUE)),3))
  pop.q75 <- c(q25, round(as.numeric(quantile(mean.pop.change, 0.75, na.rm=TRUE)),3))
  all.samp.change<-c(all.samp.change,round(as.numeric(quantile(mean.samp.change,.5,na.rm=TRUE)),3))
  samp.q25 <- c(q25, round(as.numeric(quantile(mean.samp.change, 0.25, na.rm=TRUE)),3))
  samp.q75 <- c(q75, round(as.numeric(quantile(mean.samp.change, 0.75, na.rm=TRUE)),3))
}
med.mat<-cbind(base.coln,all.pop.change,pop.q25, pop.q75, all.samp.change, samp.q25, samp.q75)

# order medians
# by pop.
sort.pop.med <- med.mat[order(all.pop.change),]
# and by samp.
sort.samp.med <- med.mat[order(all.samp.change),]
  
sort.pop.med
sort.samp.med

#only 25 years
med.mat_25<-med.mat[c(2,4,8,25,26,28),]
med.mat_25
#SAMPLE:
#Delt is the best
#Followed by American Airlines, then Alaska Airlines
#Note: the top three are really close 

#POPULATION:
#US is the best
#Followed by United, then Southwest
#Only barely underneath 0; American looks largely above 0!

#Differences in our metric between population and sampling...?

write.csv(sort.pop.med, "Data/medianOutput.csv")