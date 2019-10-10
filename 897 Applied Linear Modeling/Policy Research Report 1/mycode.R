#Dataset: good

#initial setup
install.packages("lmSupport") install.packages("pastecs")
library(lmSupport)
library(pastecs)
setwd("/Users/Leah/Downloads")
good <- read.csv("good.csv",,header=TRUE, sep=",")

#descriptive data
mydata <- cbind(good$readss97,good$AFDCpreg,good$WICpreg,good$bthwht,good$AGE97,good$fami nc97,good$HOME97)
stat.desc(mydata, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

#Q1: Each variables individual effect? 
#Q2: Total variation of child reading achievement accounted for by the model?
#inferential data
lm <- lm(readss97~ WICpreg + AFDCpreg + bthwht + AGE97 + faminc97 + HOME97, data = good)
summary(lm) nrow(good)

#Q3: Unique contribution of each program participations on the variance of child reading achievement?
#semipartial correlation
modelEffectSizes(lm)

#Q4: Which program has a larger effect on child reading achievement?
#standardize parameter estimates
good$readss97_scaled <- scale(good$readss97)
good$WICpreg_scaled <- scale(good$WICpreg)
good$AFDCpreg_scaled <- scale(good$AFDCpreg)
good$bthwht_scaled <- scale(good$bthwht)
good$AGE97_scaled <- scale(good$AGE97)
good$faminc97_scaled <- scale(good$faminc97)
good$HOME97_scaled <- scale(good$HOME97)
linearM<-lm(readss97_scaled~WICpreg_scaled+AFDCpreg_scaled+bthwht_scaled+AGE97_scaled+faminc97_scaled+HOME97_scaled,data=good)
coefficients(linearM) 
