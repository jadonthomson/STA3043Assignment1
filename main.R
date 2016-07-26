# Assignment 1
# Jadon
# 2016-07-25


# Initiate
path  <-  getwd()
require(dplyr)
require(MASS)
require(fitdistrplus)


if (file.exists(paste(path,"\\data.csv",sep=""))){
  data  <-  read.csv(paste(path, "\\data.csv", sep=""))
  attachboxplot(claimsize~veh_value)
} else {
  # Use the Claims data set.
  data <- read.csv(paste(path, "\\Claims.csv", sep=""))
  
  # Select only the nonzero claims.
  data <- data[data$claim != 0,]
  
  # Randomly sample 1000 observations from that data set using your date of birth as a seed.
  set.seed(13)
  mysample  <- sample(1:nrow(data),replace=F,size=1000)
  data      <- data[mysample,]
  data      <- select(data, -clm)
  write.csv(data, paste(path, "\\data.csv", sep=""))
  
}

###################### EXPLORATORY DATA ANALYSIS ##########################

variables <- colnames(data)
sapply(data, data.class)

# Looking at continuous variables
hist(data$veh_value)
hist(data$exposure)




# Looking at Discrete variables against the log of the claimsizes
boxplot(log(claimsize) ~ veh_body, xlab = "Vehicle Body", ylab = "Claim Size")
boxplot(log(claimsize) ~ veh_age, xlab = "Vehicle Age", ylab = "Claim Size")
boxplot(log(claimsize) ~ gender, xlab = "Gender", ylab = "Claim Size")
boxplot(log(claimsize) ~ area, xlab = "Area", ylab = "Claim Size")
boxplot(log(claimsize) ~ agecat, xlab = "Age Category", ylab = "Claim Size")


# Look at the nature of the dependent variable
hist(data$claimsize)
hist(log(data$claimsize))
# try to fit a gamma to the claimsizes by using MME
# in R, gamma has shape parameter k > 0 and scale parameter l > 0
sh = as.numeric(mmedist(data$claimsize, "gamma")$estimate[1])
sc = as.numeric(mmedist(data$claimsize, "gamma")$estimate[2])
lines(dgamma(1:13, shape = sh, scale = sc))

plot(dgamma(1:100, sh, sc))
# Fit different distributions to the claim size variable and choose the most appropriate one.  

# Build a model containing the best selection of variables, validate the model and interpret the final model.

# Present your analysis in the form of a short report.

# Get Data




