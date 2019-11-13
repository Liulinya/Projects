## Step 1: Start the setup
install.packages("Ecdat") 
install.packages("lmSupport")
install.packages("pastecs")
library("PerformanceAnalytics")
library(ggplot2)
library(lmSupport)
library(car)
library(pastecs)

setwd("/Users/Leah/Downloads")
good <- read.csv("good.csv",,header=TRUE, sep=",")
## Create a dataset with no missing observations
goodR <- na.omit(good[,c("WICpreg","CHRACE","mathraw97","AGE97","faminc97")])
#WICpreg – Women, Infant and Children (WIC) Nutrition Program participant during pregnancy: 0 = No, 1 = Yes.
#Race – Centered Binary Coding of Race: -0.5 = Black, 0.5 = White.
#mathraw97 – Woodcock-Johnson Revised Mathematics Achievement Test Raw Score. Minimum = 0, Maximum = 98.
#age97 – The child’s age in 1997. Minimum = 3, Maximum = 13.
#faminc97 – Total family income in 1997 (in 2002 constant dollars). Minimum = $-72296.26, Maximum = $784610.59.

## Step2: Display descriptive data, correlations and frequencies
stat.desc(goodR, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
chart.Correlation(goodR, histogram=TRUE, pch=19)

## Step2: Perform data transformations for variables that violate OLS assumptions
# Create a centered race varaible, a centered age variable, a squared age term, and log transformed income variable
goodR$race<- ifelse(goodR $CHRACE== 9, NA, ifelse(goodR$CHRACE== 1, .5, ifelse(goodR$CHRACE== 2, -.5, NA)))
goodR$AGE97c <- goodR$AGE97 -mean(goodR$AGE97)
goodR$logfaminc <- ifelse(goodR$faminc97 <= 1, 0, ifelse( goodR$faminc97 > 1, log(goodR$faminc97),NA))
goodR$AGE97c2 <- goodR$AGE97c**2

# Center continuous variables to avoid multicollinearity
goodR$cinc <- goodR$logfaminc - mean(goodR$logfaminc)
goodR$cincWIC <- goodR$cinc * goodR$WICpreg
goodR$raceWIC <- goodR$race * goodR$WICpreg
goodR$AGE97cWIC <- goodR$AGE97c * goodR$WICpreg
goodR$AGE97c2WIC <- goodR$AGE97c2 * goodR$WICpreg

# New Standardized Variables
goodR$zmath <- (goodR$mathraw97-mean(goodR$mathraw97))/sd(goodR$mathraw97)
goodR$zinc <- (goodR$logfaminc-mean(goodR$logfaminc))/sd(goodR$logfaminc)
goodR$zAGE97 <- (goodR$AGE97c - mean(goodR$AGE97c))/sd(goodR$AGE97c)
goodR$zAGE972 <- (goodR$AGE97c2 - mean(goodR$AGE97c2))/sd(goodR$AGE97c2)


## Step 3: Construct Main Effects Model
lm1<-lm(mathraw97 ~ AGE97c + AGE97c2  + race+  cinc  + WICpreg, data=goodR)
summary(lm1)
lm2<-lm(mathraw97 ~ AGE97c + AGE97c2   + race + cinc + WICpreg+ cincWIC  , data= goodR)
summary(lm2)
lm3<-lm(mathraw97 ~ AGE97c + AGE97c2   + race + cinc + WICpreg+ raceWIC  , data= goodR)
summary(lm3)
lm4<-lm(mathraw97 ~ AGE97c + AGE97c2   + race + cinc + WICpreg+ AGE97cWIC+ AGE97c2WIC  , data= goodR)
summary(lm4)

## Step 4: Plot the Interaction Effect & Set Up the Dataset with Predicted Y Values for Model 2
# First we create a new data frame with all of the WIC, chome variables used in lm2 (above),
# as well as the math scores, and untransformed age variable (AGE97)
goodplot2 <- na.omit(goodR[,c("AGE97","race","cinc","cincWIC","WICpreg","mathraw97")])

# We want to control for several other covariates. However, we have to choose at what value/level to hold these covariates constant at.
# In this code, AGE97c  & AGE97c2= 0 (or mean which is ~ 7 year old child). Keep this in mind when interpretting the plot
goodplot2$AGE97c<-0
goodplot2$AGE97c2<-0

# Now we can use the predict function to produce fitted values for our goodplot data set using coefficients from the lm2 model (above)
goodplot2$fit<-predict(lm2,goodplot2)
# Create a duplicate of WICpreg that is a factor
goodplot2$WicR<-factor(goodplot2$WICpreg, levels=c(0,1), labels=c("non participant","participant"))  

# Plot using loess smoother, which is short for "local regression smoother". 
# It is a type of smoother that can be used to fit a smoothed curve through points in a scatter plot.
# Note: We are plotting the untransformed age values (AGE97) against the predicted Y values (fit).
# the scale x/y continuous funcitons allow you to set the minimum/maximum values and major units the x and y axes
# labs is used to set the x/y axis labels
# ggtitle is used to set the title of the plot and the final line centers the title
ggplot(goodplot2,aes(x = cinc, y =fit)) + geom_smooth(method="loess", aes(colour= WicR)) + scale_x_continuous(limits=c(-12, 4), breaks=c(-12,-10,-8,-6,-4,-2,0,2,4)) + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + labs(x="Centered log family income",  y="Math Scores in 1997") + ggtitle("Interaction between WIC and Centered Log Family Income") + theme(plot.title = element_text(hjust = 0.5))


# Plot the Interaction Effect & Set Up the Dataset with Predicted Y Values for Model 3
goodplot3 <- na.omit(goodR[,c("AGE97","race","cinc","raceWIC","WICpreg","mathraw97")])
goodplot3$AGE97c<-0
goodplot3$AGE97c2<-0
goodplot3$fit<-predict(lm3,goodplot3)
goodplot3$WicR<-factor(goodplot3$WICpreg, levels=c(0,1), labels=c("non participant","participant"))  
ggplot(goodplot3,aes(x = cinc, y =fit)) + geom_smooth(method="loess", aes(colour= WicR)) + scale_x_continuous(limits=c(-0.5, 0.5), breaks=c(-0.5,0,0.5)) + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + labs(x="Centered Race",  y="Math Scores in 1997") + ggtitle("Interaction between WIC and Centered Race") + theme(plot.title = element_text(hjust = 0.5))


# Plot the Interaction Effect & Set Up the Dataset with Predicted Y Values for Model 4
goodplot4 <- na.omit(goodR[,c("AGE97","race","cinc","AGE97cWIC","AGE97c2WIC","WICpreg","mathraw97")])
goodplot4$AGE97c<-0
goodplot4$AGE97c2<-0
goodplot4$fit<-predict(lm3,goodplot3)
goodplot4$WicR<-factor(goodplot3$WICpreg, levels=c(0,1), labels=c("non participant","participant"))  
ggplot(goodplot3,aes(x = cinc, y =fit)) + geom_smooth(method="loess", aes(colour= WicR)) + scale_x_continuous(limits=c(-0.5, 0.5), breaks=c(-0.5,0,0.5)) + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) + labs(x="Centered Race",  y="Math Scores in 1997") + ggtitle("Interaction between WIC and Centered Race") + theme(plot.title = element_text(hjust = 0.5))