library(dplyr)
library(ggplot2)
library(reshape2)

# let's start by making the most obvious graph
# (and see how to make it not awful) for 25 years:

aggr <- read.csv("Data/aggregate.csv")

pdf("Graphs/25MeansPop.pdf")
ggplot(data=aggr) +
  geom_line(aes(y = AA_pop.mean, x=X, colour="American")) +
  geom_line(aes(y = AS_pop.mean, x=X, colour="Alaskan")) +
  geom_line(aes(y = DL_pop.mean, x=X, colour="Delta")) +
  geom_line(aes(y = WN_pop.mean, x=X, colour="Southwest")) +
  geom_line(aes(y = UA_pop.mean, x=X, colour="United")) +
  geom_line(aes(y = US_pop.mean, x=X, colour="US")) +
  ggtitle("Mean Delay By Year (25 year, population)") +
  theme_minimal() + xlab("Year") + ylab("Mean Delay (minutes)")
dev.off()

# interesting - there's definite patterns here
# start adding more airlines, see if the same definite 
# pattern rises up?

# let's do all airlines for which we have 10+ years of data:
# finding the airlines first:

air.ten <- c()
for (i in 1:30) {
  yrs.act <- 25-sum(is.na(aggr[,2+4*(i-1)]))
  if (yrs.act >= 10) {
        air.ten <- c(air.ten, 2+4*(i-1))
  }
}

colnames(aggr[,air.ten])

# changing NAs to 0 so they all show on same graph
# NOT IN FINAL PRODUCT; WE DO NOT WANT THIS
aggr[is.na(aggr)] <- 0

pdf("Graphs/10MeansPop.pdf")
ggplot(data=na.exclude(aggr)) +
  geom_line(aes(y = FL_pop.mean, x=X, colour="Airtran")) +
  geom_line(aes(y = HP_pop.mean, x=X, colour="America West")) +
  geom_line(aes(y = AA_pop.mean, x=X, colour="American")) +
  geom_line(aes(y = MQ_pop.mean, x=X, colour="American Eagle")) +
  geom_line(aes(y = AS_pop.mean, x=X, colour="Alaskan")) +
  geom_line(aes(y = EV_pop.mean, x=X, colour="Atlantic SE/ExpressJet")) +
  geom_line(aes(y = B6_pop.mean, x=X, colour="JetBlue")) +
  geom_line(aes(y = CO_pop.mean, x=X, colour="Continental")) +
  geom_line(aes(y = DL_pop.mean, x=X, colour="Delta")) +
  geom_line(aes(y = HA_pop.mean, x=X, colour="Hawaiian")) +
  geom_line(aes(y = NW_pop.mean, x=X, colour="Northwest")) +
  geom_line(aes(y = OO_pop.mean, x=X, colour="SkyWest")) +
  geom_line(aes(y = WN_pop.mean, x=X, colour="Southwest")) +
  geom_line(aes(y = TW_pop.mean, x=X, colour="Trans World")) +
  geom_line(aes(y = UA_pop.mean, x=X, colour="United")) +
  geom_line(aes(y = US_pop.mean, x=X, colour="US")) +
  ggtitle("Mean Delay By Year (10 year, population)") +
  theme_minimal() + xlab("Year") + ylab("Mean Delay (minutes)")
dev.off()