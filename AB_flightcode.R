### ST599 - Big Data Analysis and Computing
### Project 2 - Flight Delay Improvements
### 



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
as.tbl(head(flights))


###  Modifying Ethan's code for airline/aircraft only delay

year.time <- proc.time()
flt.grp <- group_by(flights,uniquecarrier,year)
flt.grp.sum <- summarise(flt.grp,
               meandelay = mean(carrierdelay+lateaircraftdelay))
sum.saved <- collect(flt.grp.sum)
sum.arr <- as.data.frame(arrange(arrange(sum.saved, year),uniquecarrier))
sum.tab <- acast(sum.arr, uniquecarrier~year, value.var="meanairlinedelay")
write.csv(round(sum.tab,2), "carrierdelaySummaryTable.csv")
proc.time() - year.time