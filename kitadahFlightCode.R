# Heather Kitada Code 

# Charlotte's code to get data
library(dplyr)

#' PostgreSQL is a database management system
#' http://en.wikipedia.org/wiki/PostgreSQL
#' (other database management systems include MySQL and Oracle)
#' SQL is a database query language that a number of database 
#' systems use to talk to databases http://en.wikipedia.org/wiki/SQL
#' For statisticians, it's probably enough to master the SELECT
#' statement.  It's helpful to also know about indexes.

#' A database is a collection of tables.  Our database is called 
#' `ontime`.  It has one table: `flights`.  Tables are just
#' like data.frames, variables in columns, observations in rows.

# --- setting up parameters to access the data base --- #
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

###The following code counts the number of flights at a given origin location 
### for a given year. 
### this is for the sampling frame

origins<-group_by(flights, origin)
airports<- summarise(origins, count = n())
unique.port<-collect(select(airports,-2))
#-2 selects the second column 
str(unique.port)
ports<-unique.port$origin
length(ports)

years<-c()
for(i in 1989:2013){
  years<-c(i, years)
}

look.at<-matrix(c(rep(0,9802)),nrow=377)
look.at[2:377,1]<-ports

i=1989
filter.year<-filter(flights, year == i)
group.origin<- group_by(filter.year, origin)
unique.per.year <- summarise(group.origin, n_origins = n())
unique.port<-collect(select(unique.per.year))
origins<-unique.port$origin
num.ori<-unique.port$n_origin
look.at[1,2]=i
for(j in 1:length(origins)){
  for(k in 2:377){
    if(origins[j]==look.at[k,1]){
      look.at[k,2]=num.ori[j]
    }
  }
}
look.at[,1:2]

### lets put it in a loop now... 
for(i in 1989:2013){
  h=i-1987
  filter.year<-filter(flights, year == i)
  group.origin<- group_by(filter.year, origin)
  unique.per.year <- summarise(group.origin, n_origins = n())
  unique.port<-collect(select(unique.per.year))
  origins<-unique.port$origin
  num.ori<-unique.port$n_origin
  look.at[1,h]=i
  for(j in 1:length(origins)){
    for(k in 2:377){
      if(origins[j]==look.at[k,1]){
        look.at[k,h]=num.ori[j]
      }
    }
  }
}

#write.csv(look.at, file = "/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/nflights89_92.csv")
write.csv(look.at, file = "/Users/heatherhisako1/Documents/bigdata_flightproj/nflights_all.csv")

###We're also going to need to know how many flights a unique airline did in a given
###year for the sampling frame (ie we need the big N)
planes <- group_by(flights, uniquecarrier)
delay <- summarise(planes, count = n())
unique.airline<-collect(select(delay,-2))
airline<-unique.airline$uniquecarrier
length(airline)

years<-c()
for(i in 1989:2013){
  years<-c(i, years)
}

look.at<-matrix(c(rep(0,806)),nrow=31)
look.at[2:31,1]<-airline

i=1989
filter.year<-filter(flights, year == i)
group.carrier<- group_by(filter.year, uniquecarrier)
unique.per.year.carrier <- summarise(group.carrier, n_carrier = n())
unique.carrier<-collect(select(unique.per.year.carrier))
carriers<-unique.carrier$uniquecarrier
num.cari<-unique.carrier$n_carrier
look.at[1,2]=i
for(j in 1:length(carriers)){
  for(k in 2:31){
    if(carriers[j]==look.at[k,1]){
      look.at[k,2]=num.cari[j]
    }
  }
}
look.at[,1:2]

##Now we need to create a new dataframe for all 25 years 
for(i in 1989:2013){
  h=i-1987
  filter.year<-filter(flights, year == i)
  group.carrier<- group_by(filter.year, uniquecarrier)
  unique.per.year.carrier <- summarise(group.carrier, n_carrier = n())
  unique.carrier<-collect(select(unique.per.year.carrier))
  carriers<-unique.carrier$uniquecarrier
  num.cari<-unique.carrier$n_carrier
  look.at[1,h]=i
  for(j in 1:length(carriers)){
    for(k in 2:31){
      if(carriers[j]==look.at[k,1]){
        look.at[k,h]=num.cari[j]
      }
    }
  }
}

write.csv(look.at, file = "/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/ncarriers.csv")
write.csv(look.at, file = "/Users/heatherhisako1/Documents/bigdata_flightproj/ncarriers.csv")

####we really need a different table for all of the unique carriers 
####since we are stratifying over origins 
str(airline)

for(a in 1:length(airline)){
  name<-airline[a]
  look.at<-matrix(c(rep(0,9802)),nrow=377)
  look.at[2:377,1]<-ports
  for(i in 1989:2013){
    h=i-1987
    filter.year<-filter(flights, (year == i) & (uniquecarrier==name))
    group.origin<- group_by(filter.year, origin)
    unique.per.year <- summarise(group.origin, n_origins = n())
    unique.port<-collect(select(unique.per.year))
    origins<-unique.port$origin
    num.ori<-unique.port$n_origin
    look.at[1,h]=i
    if(dim(filter.year)[1]>0){
    for(j in 1:length(origins)){
      for(k in 2:377){
        if(origins[j]==look.at[k,1]){
          look.at[k,h]=num.ori[j]
        }
      }
    }
  }
  }
  write.csv(look.at, file = paste("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/",name,".csv",sep=""))
  write.csv(look.at, file = paste("/Users/heatherhisako1/Documents/bigdata_flightproj/",name,".csv",sep=""))
}

### Next steps:
### 1) We need to sample from the flight data for each of the airlines
### 2) We had decided to stratify based on flight origin 
### 3) Using the csv files generated we can decide how many sampling units we need from 
### each origin 
### 4) Then we sample 
#### a) we need to keep in mind the sampling weights 
#### b) we will need to keep tract of the fpc 
### 5) Find a weighted estimate of the mean and standard errors 

airline<-read.csv("/Users/heatherhisako1/Documents/bigdata_flightproj/AA.csv",header=TRUE)
head(airline)
#totals<-c()
#for(i in 2:26){
#  totals<-c(totals,sum(airline[,i]))
#}
#totals
totals<-colSums(airline[-1])
years<-c()
y.bar.str<-c()
var.y.bar.str<-c()
for(i in 1989:2013){
  h=i-1987
  k=h-1
  if(totals[k]>0){
    y.bar.h<-c()
    var.y.bar.h<-c()
    for(j in 1:376){
      port<-ports[j]
      how.many<-round((airline[j,h]/totals[k])*1000)
      if(how.many>0){
      orig.now<-filter(flights, (year == i & uniquecarrier=="AA" & origin==port))
      orig.now_random_order<-arrange(orig.now,random())
      orig.now_samp<-collect(orig.now_random_order,n=as.integer(how.many))
      orig.now_sum<-summarise(orig.now_samp,n_samp = n(),avg_delay=mean(arrdelay,na.rm=TRUE),sd_delay=sd(arrdelay,na.rm=TRUE))
      y.bar.h<-c(y.bar.h,(airline[j,h]/totals[k])*orig.now_sum$avg_delay)
      #(airline[j,h]/totals[k])*orig.now_sum$avg_delay 
      #this is the stratfied mean
      #note: this is a weighted mean
      var.y.bar.h<-c(var.y.bar.h,(1-(how.many/airline[j,h]))*((airline[j,h]/totals[k])^2)*((orig.now_sum$sd_delay^2)/how.many))
      #(1-(how.many/airline[j,h]))*((airline[j,h]/totals[k])^2)*((orig.now_sum$sd_delay^2)/how.many)
      #this is the stratified variance 
      #note this variance is weighted
      }else{
      y.bar.h<-c(y.bar.h,NA)
      var.y.bar.h<-c(var.y.bar.h,NA)
    }
    print(j)
    }
  }else{
    y.bar.h<-c(NA)
    var.y.bar.h<-c(NA)
  }
  years<-c(years,i)
  y.bar.str<-c( y.bar.str,sum( y.bar.h,na.rm=TRUE))
  var.y.bar.str<-c(var.y.bar.str,sum(var.y.bar.h,na.rm=TRUE))
  
}
##ran this for AA (american airlines) in 1989 
##mean_str=6.175097
##var_str=0.6658089
##took ~2.25 hours to sample 
##we need to figure out a better way to do this
##I think the strata are too small...
##The sampling is definitely the rate limiting step 


#Lets stratify on airport "size" 
nflights<-read.csv("/Users/heatherhisako1/Documents/bigdata_flightproj/nflights_all.csv",header=TRUE)
head(nflights)
dim(nflights)
averages<-c()
for(i in 1:376){
  averages<-c(averages,mean(as.numeric(nflights[i,2:26]),na.rm=TRUE))
}
avg_nflights<-cbind(nflights,averages)
attach(avg_nflights)
head(avg_nflights)
sort.avg<-avg_nflights[order(averages),]
head(sort.avg)
small<-as.character(sort.avg[1:126,1])
small.range<-sort.avg[1:126,27]
med<-as.character(sort.avg[127:251,1])
med.range<-sort.avg[127:251,27]
large<-as.character(sort.avg[252:376,1])
large.range<-sort.avg[252:376,27]

### Need new frame for the airport sizes... 
for(a in 18:length(airline)){
  name<-airline[a]
  look.at<-matrix(c(rep(0,104)),nrow=4)
  look.at[,1]<-c("","Small","Med","Large")
  for(i in 1989:2013){
    k=i-1987
look.at[1,k]<-i
for(h in 1:3){
  if(h==1){#small airports
    filter.size<-filter(flights, (year == i) & (uniquecarrier==name)&(origin %in% small))
    unique.per.size <- summarise(filter.size, n_size = n())
    unique.size<-collect(select(unique.per.size))
    look.at[2,k]<-unique.size$n_size
  }
  if(h==2){#med airports
    filter.size<-filter(flights, (year == i) & (uniquecarrier==name)&(origin %in% med))
    unique.per.size <- summarise(filter.size, n_size = n())
    unique.size<-collect(select(unique.per.size))
    look.at[3,k]<-unique.size$n_size
  }
  if(h==3){#large airports
    filter.size<-filter(flights, (year == i) & (uniquecarrier==name)&(origin %in% large))
    unique.per.size <- summarise(filter.size, n_size = n())
    unique.size<-collect(select(unique.per.size))
    look.at[4,k]<-unique.size$n_size
  }
 
}
}
write.csv(look.at, file = paste("/Users/heatherhisako1/Desktop/OSU/Second Year/Spring 2014/ST 599/",name,"_size.csv",sep=""))
write.csv(look.at, file = paste("/Users/heatherhisako1/Documents/bigdata_flightproj/",name,"_size.csv",sep=""))
}
