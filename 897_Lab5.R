#Dataset: NLSY, Codebook: NLSY
#Using the added variable plot method, assess whether highest grade completed by child’s 
#mother (medu) is an omitted relevant variable in the following regression model: read = magebirth + breastfed

#Step 1: create a new data frame called ‘good2’ that
#include missing observations on the variables of interest
setwd("/Users/Leah/Downloads/Lab 5")
good <- read.csv("NLSY.csv")
good2 <- na.omit(good[,c( "read", "magebirth","breastfed","medu")])

#Step 2: Estimate regression models
AVP <-lm(read ~ magebirth + breastfed , data = good2)
resid_read <- as.data.frame(AVP$residuals)
AVP1 <-lm(medu ~ magebirth + breastfed, data = good2)
resid_medu <- as.data.frame(AVP1$residuals)

#Step 3: Plot the residuals from the initial model against residuals from the model with medu as 
#the dependent variable. Then add a regression line and lowess curve.
plot(AVP1$residuals, AVP$residuals)
abline(lm(AVP$residuals~ AVP1$residuals), col="red")
lines(lowess(AVP$residuals~ AVP1$residuals), col="blue")
