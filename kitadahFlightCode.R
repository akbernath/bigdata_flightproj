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

install.packages("RPostgreSQL")
library(RPostgreSQL)

ontime <- src_postgres("ontime", 
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)

flights <- tbl(ontime, "flights")

###The following code counts the number of flights at a given origin location 
### for a given year. 


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