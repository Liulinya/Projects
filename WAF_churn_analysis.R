#This is a data challenge code for Wharton Analytics Fellow application Fall 2019
#Reference: https://rstudio-pubs-static.s3.amazonaws.com/304282_e31a6262bf0b4ada9b9e17dd4859abb2.html

#Step 1: Install packages & Read data from csv files
setwd("/Users/Leah/Downloads/")
install.packages("ggplot2")
install.packages("caret")
install.packages("party")
install.packages("car")
install.packages("gtools")

library(ggplot2)
library(reshape2)
library(corrplot)
library(e1071)
library(caret)
library(rpart)
library(party)
library(partykit)
library(dplyr)
library(car)
library(gtools)

train <- read.csv("Training Data.csv")
test <- read.csv("Test Data.csv")
mydata <- smartbind(train,test)


#Step 2: Transform variables to numeric form (no=1, yes=0)
mydata$cancelled <- as.integer(mydata$cancelled)
mydata$cancelled[mydata$cancelled == "1"] <- 1
mydata$cancelled[mydata$cancelled == "2"] <- 0
mydata$international_plan <- as.integer(mydata$international_plan)
mydata$international_plan[mydata$international_plan == "1"] <- 0
mydata$international_plan[mydata$international_plan == "2"] <- 1
mydata$voice_mail_plan <- as.integer(mydata$voice_mail_plan)
mydata$voice_mail_plan[mydata$voice_mail_plan == "1"] <- 0
mydata$voice_mail_plan[mydata$voice_mail_plan == "2"] <- 1

#remove unwanted variables for analysis
mydata$customer_state <- NULL
#remove observations that are missing from dataset
na.omit(mydata) %>%
+ head()


#Step 3: Begin exploratory data analysis
summary(mydata)
sapply(mydata, sd)
cormatrix <- round(cor(mydata),digits=2)
cormatrix
plot.new()
plot(mydata$cancelled ~mydata$total_day_calls)
title('Scatterplot for Total Day Calls')
ggplot(mydata, aes(x=mydata$total_day_calls)) + geom_histogram(binwidth = 1, fill = "white", color = "black")


#Step 4: Randomly split data into train & test set (70% train & 30% test)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(.7,.3))
traindata <- mydata[ind == 1,]
testdata <- mydata[ind == 2,]

#Step 5: Forward elimination
#It's a stepwise regression which begins with a full model and eliminates variables one by one
#AIC (Akaike info criteria) is an estimator for out-of-sample deviance, which lower indicates a better model
forward <- step(glm(cancelled ~ 1, data = traindata), direction = 'forward', scope = ~ account_age_days + international_plan + voice_mail_plan + number_vmail_messages + total_day_calls + total_eve_calls + total_night_calls + total_intl_calls + total_minutes + total_charges + customer_support_calls)


#Step 6: Logit regression
logit <- glm(cancelled ~ voice_mail_plan + total_charges + customer_support_calls, data = traindata, family = "binomial")
summary(logit)
#confidence interval using log-likelihood
confint(logit)
exp(logit$coefficients)
exp(confint(logit)) #odds ratio

#Step 7: Prediction
logistic_model <- predict(logit, test, type = "response")
predict <- function(x) ifelse(logistic_model > x, 1, 0)
confusionMatrix(predict(.5), testdata$cancelled)
#last step should work but has an error of 'Error: `data` and `reference` should be factors with the same levels.' Therefore, use an Excel prediction instead
