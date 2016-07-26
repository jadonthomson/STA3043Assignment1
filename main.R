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
  attach(data)
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
  attach(data)
}

###################### EXPLORATORY DATA ANALYSIS ##########################

variables <- colnames(data)
sapply(data, data.class)

# Looking at continuous variables
par(mfrow=c(2,1))
hist(data$veh_value, main="Histogram of Vehicle Value", xlab = "Vehicle Value")
plot(veh_value, log(claimsize), xlab = "Vehicle Value", ylab = "Log of Claim Size") # random?
par(mfrow=c(2,1))
hist(data$exposure)
plot(exposure, log(claimsize), xlab = "Vehicle Value", ylab = "Log of Claim Size")  # not



# Looking at Discrete variables against the log of the claimsizes
par(mfrow=c(1,1))
boxplot(log(claimsize) ~ veh_body, xlab = "Vehicle Body", ylab = "Claim Size")  #looks significant
boxplot(log(claimsize) ~ veh_age, xlab = "Vehicle Age", ylab = "Claim Size")    #maybe
boxplot(log(claimsize) ~ gender, xlab = "Gender", ylab = "Claim Size")          #not
boxplot(log(claimsize) ~ area, xlab = "Area", ylab = "Claim Size")              #maybe
boxplot(log(claimsize) ~ agecat, xlab = "Age Category", ylab = "Claim Size")    #yes


# Look at the nature of the dependent variable
hist(data$claimsize, xlab="Claim Size", main = "Claim Size")
hist(log(data$claimsize), xlab = "log(Claim Size)", main = "Log of Claim Size")
# try to fit a gamma to the claimsizes by using MME
# in R, gamma has shape parameter k > 0 and scale parameter l > 0
sh = as.numeric(mmedist(log(data$claimsize), "gamma")$estimate[1])
sc = 1/as.numeric(mmedist(log(data$claimsize), "gamma")$estimate[2])


x <- seq(0, 13, length=1000)
hx <- 650*dgamma(x, shape = sh, scale = sc)
lines(x, hx, col = "red")          # definitely gamma for the log of the claims


# Fit different distributions to the claim size variable and choose the most appropriate one.  
model1 <- glm(claimsize~veh_body+veh_age+area+agecat, family = Gamma(link = "identity"))
model2 <- glm(claimsize~veh_body+veh_age+area+agecat, family = Gamma(link = "identity"))
model3 <- glm(claimsize~veh_body+veh_age+area+agecat, family = Gamma(link = "identity"))
model4 <- glm(claimsize~veh_body+veh_age+area+agecat, family = Gamma(link = "identity"))
model5 <- glm(claimsize~veh_body+veh_age+area+agecat, family = Gamma(link = "identity"))

# Build a model containing the best selection of variables, validate the model and interpret the final model.

# Present your analysis in the form of a short report.

# Get Data




