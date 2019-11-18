setwd("/Users/Leah/Downloads")
good <- read.csv("Arrests.csv",,header=TRUE, sep=",")
goodR<- na.omit(good[,c("released","race","age","gender","employed","citizen","checks")])

#Q1: Create a variable called “checksbinary” that equals 1 if an arrestee’s name appears in a police database for a previous arrest, conviction, or parole and 0 if their name does not appear
checksbinary <- ifelse(goodR$checks>0,1,0)

#Q2: Create a subset of the Arrests data frame called Arrests2 that includes the following variables: a. checksbinary, b. race, c. age
Arrests2 <- na.omit(goodR[,c("race","age")])
Arrests2["checksbinary"] <- NA
Arrests2$checksbinary <- checksbinary

#Q3: Check if the age variable adhere to the assumption of linearity
# First, estimate the logistic regression model with all variables
linearity <- glm(formula = checksbinary ~. , family=binomial(link='logit'),data= Arrests2)
# Next, use the predict function to produce logodds for plotting
logodds <- predict(linearity)
plotlin <- with(goodR, data.frame(age = age, logit = logodds))
ggplot(plotlin, aes(x = age, y = logit))+geom_point()+labs(x = "age", y = "log checks") +geom_smooth(method = "loess", col = "#3e3e3e")+geom_smooth(method = "lm", col = "blue")+scale_x_continuous(limits=c(7, 27))+scale_y_continuous(limits= c(-2,2))
# Estimate the logistic regression model & the null model without predictors
lm1<-glm(checksbinary~age + race, family=binomial(link='logit'), data=goodR)
summary(lm1)
