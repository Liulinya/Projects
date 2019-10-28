# initial setup
install.packages("pastecs")
install.packages("lmSupport")
install.packages("PerformanceAnalytics")
library(pastecs)
library(lmSupport)
library(car)
library("PerformanceAnalytics")
library(ggplot2)

setwd("/Users/Leah/Downloads")
good <- read.csv("good.csv",,header=TRUE, sep=",")

# descriptive data
mydata <- na.omit(good[,c("mathraw97","AGE97","faminc97","bthwht","WICpreg","HOME97")])
stat.desc(mydata, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
# display correlations and frequencies
chart.Correlation(mydata, histogram=TRUE, pch=19)

summary(lm)
# check linearity
ggplot(mydata, aes(x = faminc97, y = mathraw97))
+   geom_point(size = 0.6) +
+   xlab("Respondent's 1997 Family Income")
+   ylab("Math Achievement")
+   theme_bw()
+   geom_smooth(method = "loess")
ggplot(mydata, aes(x = HOME97, y = mathraw97))
+   geom_point(size = 0.6)
+   xlab("Respondent's Age")
+   ylab("Reading Achievement")
+   theme_bw()
+   geom_smooth(method = "loess")
# check homoscedasticity
mydata.res<- resid(lm)
fitted.res<-fitted(lm)
plot(fitted.res, mydata.res)
abline(0, 0, col= "red") 
lines(lowess(mydata.res ~ fitted.res), col="green")
# check normality by the Normal Q-Q
plot(lm)

# check added variable
lm<-lm(mathraw97 ~ WICpreg + bthwht + AGE97 + faminc97, data = mydata)
summary(lm)
vif(lm)
resid_math <- as.data.frame(lm$residuals)
lm2<-lm(HOME97 ~ WICpreg + bthwht + AGE97 + faminc97, data = mydata)
resid_home <- as.data.frame(lm2$residuals)
sq <- data.frame(resid_math,resid_home)
plot(sq)
abline(lm(lm$residuals~ lm2$residuals), col="red")
lines(lowess(lm$residuals ~ lm2$residuals), col="blue")

# respecifying
min(mydata$faminc97) #check min is 0
mydata$logfaminc <- ifelse(mydata$faminc97 <= 1, 0, ifelse(mydata$faminc97 > 1, log(mydata$faminc97), NA))
mydata$AGE97c <- mydata$AGE97 - mean(mydata$AGE97)
mydata$AGE97c2 <- (mydata$AGE97c)^2
lm3 <- lm(mathraw97 ~ logfaminc + AGE97c + AGE97c2 +bthwht+HOME97,data = mydata)
fitted <- lm3$fitted.values
residuals <- lm3$residuals
pred1<-data.frame(fitted,residuals)
plot(mydata $logfaminc , pred1$residuals)
abline(lm(pred1$residuals~ mydata $logfaminc ), col="red")  
lines(lowess(pred1$residuals~ mydata$logfaminc), col="blue")
plot(mydata$HOME97, pred1$residuals)
abline(lm(pred1$residuals~mydata$HOME97), col="red")  
lines(lowess(mydata$HOME97, pred1$residuals), col="blue")
summary(lm3)
vif(lm3)
