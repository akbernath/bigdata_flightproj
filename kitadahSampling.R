#Heather Kitada
#Code for creating stratified sample 

##run the following to connect to the database
library(dplyr)

endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

#install.packages("RPostgreSQL")
library(RPostgreSQL)

ontime <- src_postgres("ontime", 
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)

flights <- tbl(ontime, "flights")

###use to get list of airline carries 
planes <- group_by(flights, uniquecarrier)
delay <- summarise(planes, count = n())
unique.airline<-collect(select(delay,-2))
#unique is a list of airline abbriviations
unique<-unique.airline$uniquecarrier

###code needed for setting airport sizes

nflights<-read.csv("/Users/heatherhisako1/Documents/bigdata_flightproj/nflights_all.csv",header=TRUE)
averages<-c()
for(i in 1:376){
  averages<-c(averages,mean(as.numeric(nflights[i,2:26]),na.rm=TRUE))
}
avg_nflights<-cbind(nflights,averages)
attach(avg_nflights)
head(avg_nflights)
#orders airports by average traffic
sort.avg<-avg_nflights[order(averages),]
head(sort.avg)
#first third is "small"
small<-as.character(sort.avg[1:126,1])
small.range<-sort.avg[1:126,27]
#middle third is "medium"
med<-as.character(sort.avg[127:251,1])
med.range<-sort.avg[127:251,27]
#last third is "large"
large<-as.character(sort.avg[252:376,1])
large.range<-sort.avg[252:376,27]

###test example: American Airlines (1989)
airline.raw<-read.csv("/Users/heatherhisako1/Documents/bigdata_flightproj/9E_size.csv",header=TRUE)
#need to get rid of header(s)
airline.s<-airline.raw[-1,-1]
airline<-airline.s[,-1]
colnames(airline)<-c(1989:2013)
rownames(airline)<-c("small","med","large")

totals<-colSums(airline)
#store necessary values for csv
years<-c()
y.bar.str<-c()
var.y.bar.str<-c()
# .est is estimate
# .se is sample variance 
small.est<-c()
small.se<-c()
med.est<-c()
med.se<-c()
large.est<-c()
large.se<-c()
store.mat<-matrix(c(rep(0,200)),nrow=25)
colnames(store.mat)<-c("small_est","small_se","med_est","med_est","large_est","large_se","pop_estimate","pop_se")
rownames(store.mat)<-c(1989:2013)
for(i in 1989:2013){
  h=i-1987
  k=h-1
  #we dont want to divide by zero
  #so only go into loop if total flights are greater than 0
  if(totals[k]>0){
    y.bar.h<-c()
    var.y.bar.h<-c()
    for(j in 1:3){
      if(j==1){
        ports<-small
      }
      if(j==2){
        ports<-med
      }
      if(j==3){
        ports<-large
      }
      how.many<-round((airline[j,k]/totals[k])*1000)
      dec<-how.many/airline[j,k]
      dec<-as.numeric(2*dec)
      if(how.many>0){
        orig.now<-filter(flights, (year == as.integer(i) )&(uniquecarrier=="9E" )&(origin%in% ports)&(random()< dec))
        #orig.now_random_order<-arrange(orig.now,random())
        #orig.now_random_order<-filter(orig.now,random()< dec)
        orig.now_samp<-collect(orig.now)
        #we know that by "chance" if we just give the decimal cut off we might not get the right number in sample
        #so we'll just trim off the extra
        orig.now_trim<-orig.now_samp[1:how.many,]
        orig.now_sum<-summarise(orig.now_trim,n_samp = n(),avg_delay=mean(arrdelay,na.rm=TRUE),sd_delay=sd(arrdelay,na.rm=TRUE))
        y.bar.h<-c(y.bar.h,(airline[j,k]/totals[k])*orig.now_sum$avg_delay)
        #(airline[j,k]/totals[k])*orig.now_sum$avg_delay 
        #this is the stratfied mean
        #note: this is a weighted mean
        var.y.bar.h<-c(var.y.bar.h,(1-(how.many/airline[j,k]))*((airline[j,k]/totals[k])^2)*((orig.now_sum$sd_delay^2)/how.many))
        #(1-(how.many/airline[j,h]))*((airline[j,k]/totals[k])^2)*((orig.now_sum$sd_delay^2)/how.many)
        #this is the stratified variance 
        #note this variance is weighted
      }else{
        y.bar.h<-c(y.bar.h,NA)
        var.y.bar.h<-c(var.y.bar.h,NA)
      }
    }
  }else{
    y.bar.h<-c(NA)
    var.y.bar.h<-c(NA)
  }
  print(i)
y.bar.str<-c( y.bar.str,sum( y.bar.h,na.rm=TRUE))
var.y.bar.str<-c(var.y.bar.str,sum(var.y.bar.h,na.rm=TRUE))
small.est<-c(small.est,y.bar.h[1])
small.se<-c(small.se,var.y.bar.h[1])
med.est<-c(med.est,y.bar.h[2])
med.se<-c(med.se,var.y.bar.h[2])
large.est<-c(large.est,y.bar.h[3])
large.se<-c(large.se,var.y.bar.h[3])
}
#store into csv
store.mat[,1]<-small.est
store.mat[,2]<-small.se
store.mat[,3]<-med.est
store.mat[,4]<-med.se
store.mat[,5]<-large.est
store.mat[,6]<-large.se
store.mat[,7]<-y.bar.str
store.mat[,8]<-var.y.bar.str
write.csv(store.mat, file = paste("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/9E_strat.csv",sep=""))
write.csv(store.mat, file = paste("/Users/heatherhisako1/Documents/bigdata_flightproj/9E_strat.csv",sep=""))

###Put in BIG LOOP for all airlines over the last 25 years!

for(n in 1:length(unique)){
  name<-unique[n]
  airline.raw<-read.csv(paste("/Users/heatherhisako1/Documents/bigdata_flightproj/",name,"_size.csv",sep=""),header=TRUE)
  #need to get rid of header(s)
  airline.s<-airline.raw[-1,-1]
  airline<-airline.s[,-1]
  colnames(airline)<-c(1989:2013)
  rownames(airline)<-c("small","med","large")
  
  totals<-colSums(airline)
  years<-c()
  y.bar.str<-c()
  var.y.bar.str<-c()
  small.est<-c()
  small.se<-c()
  med.est<-c()
  med.se<-c()
  large.est<-c()
  large.se<-c()
  store.mat<-matrix(c(rep(0,200)),nrow=25)
  colnames(store.mat)<-c("small_est","small_se","med_est","med_est","large_est","large_se","pop_estimate","pop_se")
  rownames(store.mat)<-c(1989:2013)
  for(i in 1989:2013){
    h=i-1987
    k=h-1
    if(totals[k]>0){
      y.bar.h<-c()
      var.y.bar.h<-c()
      for(j in 1:3){
        if(j==1){
          ports<-small
        }
        if(j==2){
          ports<-med
        }
        if(j==3){
          ports<-large
        }
        how.many<-round((airline[j,k]/totals[k])*1000)
        dec<-how.many/airline[j,k]
        dec<-as.numeric(2*dec)
        if(how.many>0){
          orig.now<-filter(flights, (year == as.integer(i) )&(uniquecarrier==name )&(origin%in% ports)&(random()< dec))
          #orig.now_random_order<-arrange(orig.now,random())
          #orig.now_random_order<-filter(orig.now,random()< dec)
          orig.now_samp<-collect(orig.now)
          orig.now_trim<-orig.now_samp[1:how.many,]
          orig.now_sum<-summarise(orig.now_trim,n_samp = n(),avg_delay=mean(arrdelay,na.rm=TRUE),sd_delay=sd(arrdelay,na.rm=TRUE))
          y.bar.h<-c(y.bar.h,(airline[j,k]/totals[k])*orig.now_sum$avg_delay)
          #(airline[j,k]/totals[k])*orig.now_sum$avg_delay 
          #this is the stratfied mean
          #note: this is a weighted mean
          var.y.bar.h<-c(var.y.bar.h,(1-(how.many/airline[j,k]))*((airline[j,k]/totals[k])^2)*((orig.now_sum$sd_delay^2)/how.many))
          #(1-(how.many/airline[j,h]))*((airline[j,k]/totals[k])^2)*((orig.now_sum$sd_delay^2)/how.many)
          #this is the stratified variance 
          #note this variance is weighted
        }else{
          y.bar.h<-c(y.bar.h,NA)
          var.y.bar.h<-c(var.y.bar.h,NA)
        }
      }
    }else{
      y.bar.h<-c(NA)
      var.y.bar.h<-c(NA)
    }
    print(i)
    y.bar.str<-c( y.bar.str,sum( y.bar.h,na.rm=TRUE))
    var.y.bar.str<-c(var.y.bar.str,sum(var.y.bar.h,na.rm=TRUE))
    small.est<-c(small.est,y.bar.h[1])
    #note: .se is not standard error.. it is actually variance 
    small.se<-c(small.se,var.y.bar.h[1])
    med.est<-c(med.est,y.bar.h[2])
    #note: .se is not standard error.. it is actually variance 
    med.se<-c(med.se,var.y.bar.h[2])
    large.est<-c(large.est,y.bar.h[3])
    #note: .se is not standard error.. it is actually variance 
    large.se<-c(large.se,var.y.bar.h[3])
  }
  store.mat[,1]<-small.est
  store.mat[,2]<-small.se
  store.mat[,3]<-med.est
  store.mat[,4]<-med.se
  store.mat[,5]<-large.est
  store.mat[,6]<-large.se
  store.mat[,7]<-y.bar.str
  store.mat[,8]<-var.y.bar.str
  write.csv(store.mat, file = paste("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/",name,"_strat.csv",sep=""))
  write.csv(store.mat, file = paste("/Users/heatherhisako1/Documents/bigdata_flightproj/",name,"_strat.csv",sep=""))
}

#####we're going to need plots
strat.store<-c()
for(n in 1:length(unique)){
  name<-unique[n]
  airline.raw<-read.csv(paste("/Users/heatherhisako1/Documents/bigdata_flightproj/",name,"_strat.csv",sep=""),header=TRUE)
  this.strat<-cbind(airline.raw$pop_estimate,airline.raw$pop_se)
  colnames(this.strat)<-c(paste(name,"_est",sep=""),paste(name,"_var",sep=""))
  strat.store<-cbind(strat.store,this.strat)
  
}
write.csv(strat.store, file = paste("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/all_strat.csv",sep=""))
write.csv(strat.store, file = paste("/Users/heatherhisako1/Documents/bigdata_flightproj/all_strat.csv",sep=""))
  
years<-c(1989:2013)
plot(years,strat.store[,odd],type="n",ylim=c(-5,18))
for(i in 1:30){
  odd=2*i-1
lines(years,strat.store[,odd],lty=as.numeric(odd))
}

##we should look at changes 

install.packages("Hmisc", dependencies=T)
library("Hmisc")

for(i in 1:30){
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-strat.store[,odd]
  carry.se<-sqrt(strat.store[,even])
  year  = c(1989:2013)
  mean = carry.mean
d = data.frame(
  year  = c(1989:2013),
  mean = carry.mean,
  sd = carry.se
)

# add error bars (without adjusting yrange)
plot(d$year, d$mean, type="n",ylim=c(-5,18),main=paste(name,"Exploratory Plots for Strata Estimates",sep=" "))
with (data = d
      ,expr = errbar(year, mean, mean+1.96*sd, mean-1.96*sd, add=T, pch=1, cap=.05)
)

lines(year,mean,lty=2)
}

###lets look at changes


for(i in 1:30){
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-strat.store[,odd]
  mean.final<-carry.mean[2:25]
  mean.initial<-carry.mean[1:24]
  mean.change<-mean.final-mean.initial
  
  #carry.se<-sqrt(strat.store[,even])
  year  = c(1990:2013)
  mean = mean.change
  
  # add error bars (without adjusting yrange)
  plot(year, mean, type="n",ylim=c(min(mean),max(mean)),main=paste(name,"Exploratory Plots for Strata Estimates Changes",sep=" "))
  #with (data = d
  #      ,expr = errbar(year, mean, mean+sd, mean-sd, add=T, pch=1, cap=.05)
  #)
  lines(year,mean,lty=2,col="red")
  abline(h=0,lty=2)
}

###what about median change overall?

all.change<-c()
#use quantiles to have some understanding of variability 
q25<-c()
q75<-c()

for(i in 1:30){
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-strat.store[,odd]
  mean.final<-carry.mean[2:25]
  mean.initial<-carry.mean[1:24]
  mean.change<-mean.final-mean.initial
  for(j in 1:24){
    if(as.numeric(mean.change[j])==0){
      mean.change[j]=NA
    }
  }
  all.change<-c(all.change,round(as.numeric(quantile(mean.change,.5,na.rm=TRUE)),3))
  q25<-c(q25,round(as.numeric(quantile(mean.change,.25,na.rm=TRUE)),3))
  q75<-c(q75,round(as.numeric(quantile(mean.change,.75,na.rm=TRUE)),3))
}
med.mat<-cbind(unique,all.change,q25,q75)

#order medians
sort.med<-med.mat[order(all.change),]

#only 25 years
med.mat_25<-med.mat[c(2,4,8,25,26,28),]
#Delt is the best
#Followed by American Airlines, then Alaska Airlines
#Note: the top three are really close 

######now makes plots for population parameters 

pop<-read.csv("/Users/heatherhisako1/Documents/bigdata_flightproj/Data/popMean.csv",header=TRUE)


for(i in 1:30){
  j=i+1
  pop.mean<-pop[3:27,j]
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-strat.store[,odd]
  carry.se<-sqrt(strat.store[,even])
  year  = c(1989:2013)
  mean = carry.mean
  d = data.frame(
    year  = c(1989:2013),
    mean = carry.mean,
    sd = carry.se
  )
  
  # add error bars (without adjusting yrange)
  plot(d$year, d$mean, type="n",ylim=c(-5,18),main=paste(name,"Exploratory Plots for Strata Estimates",sep=" "))
  with (data = d
        ,expr = errbar(year, mean, mean+2*sd, mean-2*sd, add=T, pch=1, cap=.0)
  )
  lines(year,mean,lty=2)
  points(year,pop.mean,pch=13,col="red")
}

##lets assess coverage 
count<-0
total<-0
for(i in 1:30){
  j=i+1
  pop.mean<-pop[3:27,j]
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-strat.store[,odd]
  carry.se<-sqrt(strat.store[,even])
  year  = c(1989:2013)
  mean = carry.mean
  for(k in 1:25){
    if(carry.se[k]>0){
      total<-total+1
    if((carry.mean[k]-2*carry.se[k])<pop.mean[k]&(carry.mean[k]+2*carry.se[k])>pop.mean[k]){
      count<-count+1
    }
    }
    
  }
}
count/total

#coverage = .76 
#thats not very good :(

##Its only fair to compare airlines that existed during the same time period 
##The following airlines had flights during the whole 25 years
##2,4,8,25,26,28
color<-c("red","darkorange","darkgoldenrod1","forestgreen","dodgerblue","darkorchid3")
plot(d$year, d$mean, type="n",ylim=c(-7,18),main="Comparing Population Mean and Stratified Sampling Mean Estimates",xlab="Year",ylab="Mean Arrival Delay")
k=-0.3
h=0
for(i in c(2,4,8,25,26,28)){
  k=k+0.05
  h=h+1
  j=i+1
  pop.mean<-pop[3:27,j]
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-strat.store[,odd]
  carry.se<-sqrt(strat.store[,even])
  year  = c(1989:2013)
  mean = carry.mean
  d = data.frame(
    year  = c(1989:2013),
    mean = carry.mean,
    sd = carry.se
  )
  # add error bars (without adjusting yrange)
  with (data = d
        ,expr = errbar(year+k, mean, mean+2*sd, mean-2*sd, add=T, pch=1, cap=.0,col=color[h],errbar.col=color[h])
  )
  points(year+k,pop.mean,pch=16,col=color[h])
  #lines(year+k,pop.mean,col=color[h])
}
legend(locator(1), unique[c(2,4,8,25,26,28)], pch = 20,col=color,ncol=2,title="Unique Carriers",bty="n",seg.len=.5,x.intersp=.5,text.width=.25)

###plots all lines on the same plot

plot(years,mean.change,type="n",ylim=c(-15,10),xlab="Year",ylab="Change in Sample Mean Delay",main="Change in Sample Mean Delay for Airlines Existing Over the Past 25 Years ")
years<-c(1989:2012)
h=0
for(i in c(2,4,8,25,26,28)){
  h=h+1
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-strat.store[,odd]
  mean.final<-carry.mean[2:25]
  mean.initial<-carry.mean[1:24]
  mean.change<-mean.final-mean.initial
lines(years,mean.change,col=color[h],lty=1)
}
legend(locator(1), unique[c(2,4,8,25,26,28)], lty=1,col=color,ncol=2,title="Unique Carriers",bty="n",seg.len=.5,x.intersp=.5,text.width=.25)
abline(h=0)

###makes the same plot for population 
pop<-read.csv("/Users/heatherhisako1/Documents/bigdata_flightproj/Data/popMean.csv",header=TRUE)
head(pop)
plot(years,mean.change,type="n",ylim=c(-15,10),xlab="Year",ylab="Change in Population Mean Delay",main="Change in Population Mean Delay for Airlines Existing Over the Past 25 Years ")
years<-c(1989:2012)
h=0
for(i in c(2,4,8,25,26,28)){
  j=i+1
  h=h+1
  name<-unique[i]
  odd=2*i-1
  even=2*i
  carry.mean<-pop[3:27,j]
  mean.final<-carry.mean[2:25]
  mean.initial<-carry.mean[1:24]
  mean.change<-mean.final-mean.initial
  lines(years,mean.change,col=color[h],lty=1)
  #carry.mean2<-strat.store[,odd]
  #mean.final2<-carry.mean2[2:25]
  #mean.initial2<-carry.mean2[1:24]
  #mean.change2<-mean.final2-mean.initial2
  #lines(years,mean.change2,col=color[h],lty=2)
}
legend(locator(1), unique[c(2,4,8,25,26,28)], lty=1,col=color,ncol=2,title="Unique Carriers",bty="n",seg.len=.5,x.intersp=.5,text.width=.25)
abline(h=0)

###the following is for mean change in population mean 
###we also use iqr to assess variability 
all.change<-c()
q25<-c()
q75<-c()

for(i in 1:30){
  k=i+1
  name<-unique[i]
  carry.mean<-pop[3:27,k]
  mean.final<-carry.mean[2:25]
  mean.initial<-carry.mean[1:24]
  mean.change<-mean.final-mean.initial
  #print(mean.change)
  for(j in 1:24){
    if(as.numeric(mean.change[j])==NA){
      mean.change[j]=NA
    }
  }
  all.change<-c(all.change,round(as.numeric(quantile(mean.change,.5,na.rm=TRUE)),3))
  q25<-c(q25,round(as.numeric(quantile(mean.change,.25,na.rm=TRUE)),3))
  q75<-c(q75,round(as.numeric(quantile(mean.change,.75,na.rm=TRUE)),3))
}
med.mat<-cbind(unique,all.change,q25,q75)

#order medians
sort.med<-med.mat[order(all.change),]

