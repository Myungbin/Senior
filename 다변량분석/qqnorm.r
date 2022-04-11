# qqnorm

mydata <- read.csv('./data/나이와수입.csv')
n <- dim(mydata)[1]
t <- seq(1,n)
age_s <- sort(mydata$age); income_s <- sort(mydata$income)

edf <- (t-0.5)/n  # or t/(n+1)
q <- qnorm(edf)

cbind(t, age_s, edf, q)
cbind(t, income_s, edf, q)

qqnorm(mydata$age)
qqline(mydata$age)


qqnorm(mydata$income)
qqline(mydata$income)

