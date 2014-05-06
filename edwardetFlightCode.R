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

# 0c:

planes <- group_by(flights, uniquecarrier)
delay <- summarise(planes, count = n())
tail(select(delay, -2))

# doesn't work! try "collect"

col.tst <- proc.time()
test <- collect(delay)
tail(test)
proc.time() - col.tst

# fortunately, gives number of unique aircraft
# unfortunately, takes a good while! (91.68s avg, 1 execute)
# changing (select(delay,-2)) to (delay) shaves ~33 seconds
# additionally, gives object type VECSXP; cannot use certain
# handy functions (length(), n_distinct())

# We set a new goal here: for the largest airline volume
# (WN), we want to create a table of mean delay time
# (mean(arrdelay + depdelay)) from 2000-2005 by year.
# We can then refine this to by month, depending on timing.

year.time <- proc.time()
flt.small <- filter(flights, year == 2000 ||
                      year == 2001, uniquecarrier=="WN")
flt.yr.small <- group_by(flt.small,year)
flt.sum.small <- summarise(flt.yr.small,
                           meandelay = mean(arrdelay+depdelay))
sum.saved <- collect(flt.sum.small)
arrange(sum.saved, year)
proc.time() - year.time

# mean runtime: 226.24s (2 attempts)
# now to check for FIVE airlines; is it a linear increase?
# we naively expect a linear increase of 2.5x, roughly
# if it takes roughly 600s, we're right
# if it takes MORE, we have a problem
# if it takes LESS, we're off to a great start

year.time <- proc.time()
flt.small <- filter(flights, year == 2000 ||
                      year == 2001 || year == 2002 ||
                      year == 2003 || year == 2004)
flt.wn.small <- filter(flt.small, uniquecarrier=="WN")
flt.yr.small <- group_by(flt.wn.small,year)
flt.sum.small <- summarise(flt.yr.small,
                           meandelay = mean(arrdelay+depdelay))
sum.saved <- collect(flt.sum.small)
arrange(sum.saved, year)
proc.time() - year.time

# mean runtime: 338.1s (1 attempt) [OSU wifi]
# this is significantly less than a linear increase!
# whoop whoop

# now we go for the BIG one:

year.time <- proc.time()
flt.grp <- group_by(flights,uniquecarrier,year)
flt.grp.sum <- summarise(flt.grp,
                         meandelay = mean(arrdelay+depdelay))
sum.saved <- collect(flt.grp.sum)
sum.arr <- as.data.frame(arrange(arrange(sum.saved, year),uniquecarrier))
sum.tab <- acast(sum.arr, uniquecarrier~year, value.var="meandelay")
write.csv(round(sum.tab,2), "delaySummaryTable.csv")
proc.time() - year.time

# mean runtime: 249.34s (2 attempts)??!