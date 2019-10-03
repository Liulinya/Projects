#Dataset: Wages and Hours.csv Codebook: Wages and Hours

#Q1: How many variables are included in the Wages and Hours dataset? 
#Q2: How many observations (or rows) are included in the Wages and Hours dataset?
getwd()
setwd("/Users/Leah/Downloads/Lab 2")
good <- read.csv("Wages and Hours.csv")
goodR <- na.omit(good)
dim(goodR)

#Q3: What is the correlation between the average highest grade of school completed and average hours worked 
#during the year? Is this association significant?
cor.test(good$SCHOOL,good$HRS,use="complete.obs",method="pearson")

#Q4: Produce a plot of the average highest grade of school completed (x) and average hours worked during the year (y). 
#Be sure to include a smoothing line.
plot(good$SCHOOL,good$HRS,main="Scatterplot of School Grade & Working Hour")
 
#Q5: Estimate a simple linear regression model where average hours worked during the year (y) is regressed 
#on the highest grade of school completed (x).
lm(good$HRS~good$SCHOOL)
summary(lm(good$HRS~good$SCHOOL))
