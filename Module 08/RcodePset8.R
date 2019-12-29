# Load data

rm(list = ls())
library(car)

setwd("/Users/raz/Dropbox/14.31 edX Building the Course/Problem Sets/PSET 9")

nlsw88 <- read.csv('nlsw88.csv')

head(nlsw88)



#covariance
cov_yx <- cov(nlsw88$lwage, nlsw88$yrs_school)  
cov_yx

var_x <- var(nlsw88$yrs_school) 
var_x

hatbeta1_0 <- cov_yx / var_x
print(hatbeta1_0)

mean(nlsw88$lwage)
mean(nlsw88$yrs_school)


lm(lwage ~ yrs_school, data = nlsw88)


#simple linear regression
single <- lm(lwage ~ yrs_school, data = nlsw88)

summary(single) # show results
coefficients(single) # model coefficients


ci <- confint(single, level=0.9) 
ci

resid <- residuals(single) # residuals
sum(resid)

# 
dual <- lm(lwage ~ yrs_school + black, data = nlsw88)
summary(dual)



#dummy variables
meanother <- mean(nlsw88$lwage[nlsw88$black == 0])
meanblack <- mean(nlsw88$lwage[nlsw88$black == 1])

meanother
meanblack

meanblack - meanother

dummymodel <- lm(lwage ~ black, data = nlsw88)
summary(dummymodel)



#multivariable regression
multi <- lm(lwage ~ yrs_school + ttl_exp, data = nlsw88)


summary(multi) # show results
coefficients(multi)


anova_unrest <- anova(multi)
anova_unrest



#Restricted model
nlsw88$newvar <- nlsw88$yrs_school + 2*nlsw88$ttl_exp
restricted <- lm(lwage ~ newvar, data = nlsw88)

summary(restricted) # show results

anova_rest <- anova(restricted)
anova_rest


#Test
statistic_test <- (((anova_rest$`Sum Sq`[2]-anova_unrest$`Sum Sq`[3])/1)
                   /((anova_unrest$`Sum Sq`[3])/anova_unrest$Df[3]))
statistic_test

pvalue <- df(statistic_test, 1, anova_unrest$Df[3])
pvalue


#Test
matrixR <- c(0, -2, 1)
linearHypothesis(multi, matrixR)





