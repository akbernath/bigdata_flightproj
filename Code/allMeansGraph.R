library(dplyr)
library(ggplot2)
library(reshape2)

# let's start by making the most obvious graph
# (and see how to make it not awful) for 25 years:

aggr <- read.csv("Data/aggregate.csv")

pdf("Graphs/25MeansPop.pdf")
ggplot(data=aggr) +
  geom_line(aes(x=X, y=AA_pop.mean, colour="American")) +
  geom_line(aes(y = DL_pop.mean, x=X, colour="Delta")) +
  geom_line(aes(y = AS_pop.mean, x=X, colour="Alaskan")) +
  geom_line(aes(y = WN_pop.mean, x=X, colour="Southwest")) +
  geom_line(aes(y = US_pop.mean, x=X, colour="US")) +
  geom_line(aes(y = UA_pop.mean, x=X, colour="United")) +
  ggtitle("Mean Delay By Year (25 year, population)") +
  theme_minimal() + xlab("Year") + ylab("Mean Delay (minutes)")
dev.off()

# interesting - there's definite patterns here
# start adding more airlines, see if the same definite 
# pattern rises up?