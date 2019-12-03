# Preliminaries
#-------------------------------------------------
rm(list = ls())
setwd("[SET YOUR WORKING DIRECTORY]")
library(car)
library(rdd)



#-------2LS Estimates-------
census80 <- read.csv('census80.csv')
#Question 1
summary(census80)
head(census80)


#Question 2
census80$temp[census80$ageq2nd == census80$ageq3rd] <- 1

census80$multiple <- 0
census80$multiple[census80$temp == 1] <- 1

summary(census80$multiple)


#Question 3
census80$samesextemp <- (census80$sex1st == census80$sex2nd)

census80$samesex[census80$samesextemp == FALSE] <- 0
census80$samesex[census80$samesextemp == TRUE] <- 1
summary(census80$samesex)



#Question 4
ols1 <- lm(workedm ~ three + blackm + hispm + othracem, data = census80)
OLS[1, 1] <- ols1$coefficients[2]
pvalue <- summary(ols1)
OLS[2, 1] <- pvalue$coefficients[2, 4]

ols2 <- lm(weeksm ~ three + blackm + hispm + othracem, data = census80)
OLS[1, 2] <- ols2$coefficients[2]
pvalue <- summary(ols2)
OLS[2, 2] <- pvalue$coefficients[2, 4]
OLS



#Question 5 - 6
#First Stage
#------------------------------
firststage1 <- lm(three ~ multiple + blackm + hispm + othracem, data = census80)
FirstStage[1, 1] <- firststage1$coefficients[2]
pvalue <- summary(firststage1)
FirstStage[2, 1] <- pvalue$coefficients[2, 4]

firststage2 <- lm(three ~ samesex + blackm + hispm + othracem, data = census80)
FirstStage[1, 2] <- firststage2$coefficients[2]
pvalue <- summary(firststage2)
FirstStage[2, 2] <- pvalue$coefficients[2, 4]
FirstStage



#Question 7
#IV model using multiple pregnancy
#-----------------------------------------------
iva1 <- ivreg(workedm ~ three + blackm + hispm + othracem |
                blackm + hispm + othracem + multiple, data = census80)
IVa[1, 1] <- iva$coefficients[2]
pvalue <- summary(iva1)
IVa[2, 1] <- pvalue$coefficients[2, 4]

iva2 <- ivreg(weeksm ~ three + blackm + hispm + othracem |
                blackm + hispm + othracem + multiple, data = census80)
IVa[1, 2] <- iva$coefficients[2]
pvalue <- summary(iva2)
IVa[2, 2] <- pvalue $coefficients[2, 4]
IVa



#Question 8
#-------------------------------------------------
ivb1 <- ivreg(workedm ~ three + blackm + hispm + othracem |
                blackm + hispm + othracem + samesex, data = census80)
IVb[1, 1] <- ivb1$coefficients[2]
pvalue <- summary(ivb1)
IVb[2, 1] <- pvalue$coefficients[2, 4]

ivb2 <- ivreg(weeksm ~ three + blackm + hispm + othracem |
                blackm + hispm + othracem + samesex, data = census80)
IVb[1, 2] <- ivb2$coefficients[2]
pvalue <- summary(ivb2)
IVb[2, 2] <- pvalue$coefficients[2, 4]
IVb

