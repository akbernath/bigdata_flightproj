##  ST599 - Big Data Analysis
##  Group Project 2: Flight Data
##  Andrew Bernath

#   Standard libraries
library(dplyr)
library(plyr)

#   Provided code for data access
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

#   Test pull of UA March 2008 data
UA <- filter(flights, (year=="2008"&month==3)&uniquecarrier=="UA")
UA_loc <- collect(UA)

#   Group the carriers together
carrier <- group_by(flights, uniquecarrier)
medians
help(matrix)
medians <- matrix(nrow=9000,ncol=4)
colnames(medians) <- c("year","month","carrier","median")
for(i in 1989:2013){
  for(j in 1:12){
    tmp <- filter(flights, (year==i & month==j))
    tmp_loc <- collect(tmp)
    medians[4,] <- summarise(tmp,uniquecarrier,median(carrierdelay))
  }
  
}

tmp <- filter(flights, year=="1989" & month=="12" & uniquecarrier=="UA")
tmp_loc <- collect(tmp)
days <- collect(group_by(tmp_loc,dayofmonth))
collect(days)
head(days$carrierdelay)
summarise(days,median(carrierdelay))
