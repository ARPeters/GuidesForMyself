rm(list = ls())

# ---- load-packages ------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(AER)
library(lavaan)

# ---- declare-globals ------------------------------------------------------------------------------------------------------------------------
mySeed <- 7866
simN   <- 5000


# ---- simulate-data ------------------------------------------------------------------------------------------------------------------------
set.seed(mySeed)

# X (binary) and Z (continuous), predictors of mediator M
X         <- rbinom(n=simN,size=1,prob=.5)
Z         <- rnorm(n=simN)

linpred_M <- -15 + 15*X + .5*Z*X
M         <- rbinom(n=simN,size=1,plogis(linpred_M))
#M         <- 0 + .5*X + .5*Z + rnorm(n=simN)*sqrt(1)

# Outcome variable Y, regressed onto X, Y, and Z
Y         <- 0 + 0*X + .5*M + .5*Z + rnorm(n=simN)*sqrt(1)

ds        <- data.frame(list(Y=Y,M=M,X=X,Z=Z))


# ---- analyze-data ------------------------------------------------------------------------------------------------------------------------
# Baseline analysese
# Modeling M: Almost correctly specified
summary(glm(M~X,"binomial"))
summary(glm(M~X + Z,"binomial"))

# Modeling M: Correctly specified
summary(glm(M~X + Z + X*Z,"binomial"))

# Modeling Y: Almost Correctly Specified
summary(lm(Y~M))

# Modeling Y: Correctly Specified
summary(lm(Y~X+M+Z))

# cor(ds[,c(4:1)])
# ggplot(ds,aes(x=M)) + geom_histogram() + facet_grid(~X)

# Linear Model: Almost correct 
fit.ols <- lm(Y~X+M)
summary(fit.ols)


# Two-Stage Least-Squares regression: Piecemeal
# First: regress M onto X, get predicted M values
ols_first <- lm(M ~ X)
M_hat     <- fitted(ols_first)

summary(ols_first)

# Second: regress Y onto Predicted M values
ols_second <- lm(Y ~ M_hat)
summary(ols_second)
coef(ols_second)


# TSLS: Using ivreg() function from AER package
# In this case: Get's same results
iv_res <- ivreg(Y ~ M | X)
summary(iv_res)


# Using sem() function from lavaan packages
# Instrumental Variables Approach
ivMod <- "
Y ~ intY*1 + b*M   
M ~ intM*1 + a*X

#variances and residuals
Y ~~ start(.9)*Y
M ~~ start(1.25)*M
Y ~~ M

#Indirect effect
ab := a*b

"

fit <- sem(ivMod, data=ds,fixed.x = FALSE)
summary(fit)
