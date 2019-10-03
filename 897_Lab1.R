#Q1: What are the names of the variables included in the Nursing Home dataset?
getwd()
setwd("/Users/Leah/Downloads/Lab 1")
read.csv("Nursing Home.csv")
nursinghome <- read.csv("Nursing Home.csv")
str(nursinghome)
colnames(nursinghome)

#Q2: Produce a histogram describing the distribution of the annual nursing salaries.
hist(nursinghome$NSAL)

#Q3: Presume the dataset includes data on all licensed nursing facilities in New Mexico in 1988. 
# Do rural or non-rural nursing facilities have higher annual expenditures? 
aggregate(nursinghome$FEXP~nursinghome$RURAL,FUN=mean)

#Q4: Create a variable called “bed100” that equals 1 for nursing facilities that have 100 or fewer beds and 0 
# if more than 100 beds. Using the table function, how many facilities have more than 100 beds?
bed100 <- ifelse(nursinghome$BED<=100,1,0) 
bed100r <- factor(bed100,levels=c(0,1),labels=c("beds <= 100","beds > 100"))
table(bed100r)
