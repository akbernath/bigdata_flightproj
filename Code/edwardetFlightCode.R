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