# 0a: install packages, include necessary ones

# install.packages("RPostgreSQL")

library(dplyr)

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
test <- collect(select(delay, -2))
tail(test)
proc.time() - col.tst

# fortunately, gives number of unique aircraft
# unfortunately, takes a good while! (91.68s avg, 1 execute)
# additionally, gives object type VECSXP; cannot use certain
# handy functions (length(), n_distinct())