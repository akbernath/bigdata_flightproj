###  Andrew Bernath
###  ST599 - Big Data Analysis
###  Project 2 - 25 year flight data

##   Population graphics code

setwd("D:/School/Spring 14/ST599/flightproj/bigdata_flightproj")
df_flights <- read.csv("Data/aggregate.csv")

#  Create bias matrix
name.vect <- c("9E", "AA", "AQ", "AS", "B6", "CO",
               "DH", "DL", "EA", "EV", "F9", "FL",
               "HA", "HP", "ML(1)", "MQ", "NW", "OH",
               "OO", "PA(1)", "PI", "PS", "TW", "TZ",
               "UA", "US", "VX", "WN", "XE", "YV")
bias <- matrix(ncol=30,nrow=25)
colnames(bias) <- name.vect

# Loop captures bias for each year's mean delay for every carrier
for(i in 1:30){
  bias[,i] <- df_flights[,4*i]-df_flights[,4*i-2]
}

# Mean bias for each carrier
mean.bias <- colMeans(bias, na.rm=TRUE)
grand.mean <- round(mean(bias, na.rm=TRUE),3)
write.csv(mean.bias, "Data/mean_bias.csv")


# Histogram of all bias
# 2nd commit, cleaned up code and revised histogram
pdf(paste("Bias Histogram.pdf"))
hist(bias, breaks=23, main="Frequency Distribution of Sampling Bias
     for Mean Arrival Delay per Year per Airline",
     xlab=c("Sampling Bias
            (Grand Mean = 0.335 minutes"),
     xlim=c(-7,7), ylim=c(0,50), col="light blue")
dev.off()
