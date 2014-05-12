###  Andrew Bernath
###  ST599 - Big Data Analysis
###  Project 2 - 25 year flight data

##   Population graphics code

setwd("D:/School/Spring 14/ST599/flightproj/bigdata_flightproj")

library(ggplot2)
library(gridExtra)
library(dplyr)

df_flights <- read.csv("Data/aggregate.csv")
df_flights[is.na(df_flights)] <- 0

head(df_flights)

#  Create bias matrixbias
name.vect <- c("9E", "AA", "AQ", "AS", "B6", "CO",
               "DH", "DL", "EA", "EV", "F9", "FL",
               "HA", "HP", "ML(1)", "MQ", "NW", "OH",
               "OO", "PA(1)", "PI", "PS", "TW", "TZ",
               "UA", "US", "VX", "WN", "XE", "YV")
bias <- matrix(ncol=30,nrow=25)
colnames(bias) <- name.vect


for(i in 1:30){
  bias[,i] <- df_flights[,4*i]-df_flights[,4*i-2]
}

mean.bias <- colMeans(bias, na.rm=TRUE)

hist(bias, breaks=25)



