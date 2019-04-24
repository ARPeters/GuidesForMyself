rm(list = ls())


# ---- load-packages ------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(AER)
library(lavaan)
library(lme4)


# ---- declare-globals ------------------------------------------------------------------------------------------------------------------------
mySeed <- 7866
simN   <- 5000


# ---- simulate-data ------------------------------------------------------------------------------------------------------------------------
set.seed(mySeed)

# Creating Sample Data
# X (binary) and Z (continuous), predictors of mediator M
X         <- rbinom(n=simN,size=1,prob=.5)
Z         <- rnorm(n=simN)

linpred_M <- -15 + 15*X + .5*Z*X
M         <- rbinom(n=simN,size=1,plogis(linpred_M))
#M         <- 0 + .5*X + .5*Z + rnorm(n=simN)*sqrt(1)

# Outcome variable Y, regressed onto X, Y, and Z
Y         <- 0 + 0*X + .5*M + .5*Z + rnorm(n=simN)*sqrt(1)

ds        <- data.frame(list(Y=Y,M=M,X=X,Z=Z))

# Creating fake longitudinal variable with id and frailty term
id            <- rep(c(1:1000), 5)
id            <- id[order(id)]

frailzy       <- round(rep(rnorm(n = (simN/5)), 5), 3)
frailzy       <- frailzy[order(frailzy)]

L_X           <- rbinom(n=simN,size=1,prob=.5)

L_linpred_M   <- -15 + 15*L_X + .5*frailzy*L_X
L_M           <- rbinom(n=simN,size=1,plogis(L_linpred_M))

L_Y           <- 0 + 0*L_X + 0.5*L_M + 0.5*frailzy + rnorm(n=simN)*sqrt(1)

ds_L          <- data.frame(list(ID = id, Y = L_Y, M=L_M, X=L_X, Z = frailzy))


# ---- analyze-data ------------------------------------------------------------------------------------------------------------------------
# Baseline analysese
summary(glm(M~X          , "binomial" , data = ds_L))  # Modeling M: Almost correctly specified
summary(glm(M~X + Z      , "binomial" , data = ds_L))  # Modeling M: Almost correctly specified
summary(glm(M~X + Z + X*Z, "binomial" , data = ds_L))  # Modeling M: Correctly specified

summary(lm(Y~M    , data = ds_L)) # Modeling Y: Almost Correctly Specified
summary(lm(Y~X+M+Z, data = ds_L)) # Modeling Y: Correctly Specified

# cor(ds[,c(4:1)])
# ggplot(ds,aes(x=M)) + geom_histogram() + facet_grid(~X)

# Linear Model: Almost correct 
fit.ols <- lm(Y~X+M, data = ds_L) 
summary(fit.ols)

# Two-Stage Least-Squares regression: Piecemeal
# First: regress M onto X, get predicted M values
ols_first <- lm(M ~ X, data = ds_L)
M_hat     <- fitted(ols_first)

summary(ols_first)

# Second: regress Y onto Predicted M values
ols_second <- lm(Y ~ M_hat, data = ds_L)
summary(ols_second)
# coef(ols_second)

# TSLS: Using ivreg() function from AER package
# In this case: Get's same results
iv_res <- ivreg(Y ~ M | X, data = ds_L)
summary(iv_res)

# Using sem() function from lavaan packages
# Instrumental Variables Approach
ivMod <- 
"
Y ~ intY*1 + b*M   
M ~ intM*1 + a*X

#variances and residuals
Y ~~ start(.9)*Y
M ~~ start(1.25)*M
Y ~~ M

#Indirect effect
ab := a*b

"

fit <- sem(ivMod, fixed.x = FALSE, data=ds_L)
summary(fit)


# TSLS: with lmer (which can accomodate id values)
ols_first_mixed <- lmer(M ~ X + (1|ID), data = ds_L)
M_hat_mixed     <- fitted(ols_first   , data = ds_L)

summary(ols_first_mixed)

# Second: regress Y onto Predicted M values
ols_second_mixed <- lmer(Y ~ M_hat_mixed + (1|ID), data = ds_L)
summary(ols_second_mixed)

# Compare correct (lm) to correctish (lmer)
ols_first_correct_lm  <- lm(M ~ X + X*Z                 , data = ds)
m_hat_correct_lm      <- fitted(ols_first_correct_lm)
ols_second_correct_lm <- lm(Y ~ X + m_hat_correct_lm + Z, data = ds)

ols_first_correct_lmer  <- lmer(M ~ X + (1|ID)                     , data = ds_L)
m_hat_correct_lmer      <- fitted(ols_first_correct_lmer)
ols_second_correct_lmer <- lmer(Y ~ X + m_hat_correct_lmer + (1|ID), data = ds_L)

summary(ols_first_correct_lm)
summary(ols_first_correct_lmer)

summary(ols_second_correct_lm)
summary(ols_second_correct_lmer)

# sem approach: Latent Growth Curve Modeling
dsw <- 
  ds_L %>%
  dplyr::filter(t == 1) %>%
  dplyr::rename(
    Y1 = Y,
    M1 = M,
    X1 = X
  ) %>%
  dplyr::left_join(
    ds_L %>% dplyr::filter(t == 2) %>%
      dplyr::select(- "Z" ) %>%
      dplyr::rename(
        Y2 = Y,
        M2 = M,
        X2 = X
      ), by = "ID"
  ) %>%
  dplyr::left_join(
    ds_L %>% dplyr::filter(t == 3) %>%
      dplyr::select(- "Z" ) %>%
      dplyr::rename(
        Y3 = Y,
        M3 = M,
        X3 = X
      ), by = "ID"
  ) %>%
  dplyr::left_join(
    ds_L %>% dplyr::filter(t == 4) %>%
      dplyr::select(- "Z" ) %>%
      dplyr::rename(
        Y4 = Y,
        M4 = M,
        X4 = X
      ), by = "ID"
  ) %>%
  dplyr::left_join(
    ds_L %>% dplyr::filter(t == 5) %>%
      dplyr::select(- "Z" ) %>%
      dplyr::rename(
        Y5 = Y,
        M5 = M,
        X5 = X
      ), by = "ID"
  ) %>%
  dplyr::select(
    ID, Z, Y1, Y2, Y3, Y4, Y5, X1, X2, X3, X4, X5, M1, M2, M3, M4, M5
  )

head(dsw)

model <- '
# Intercept and slope with fixed coefficients
  i =~ Y1 + Y2 + Y3 + Y4 + Y5
  s =~ Y1 + Y2 + Y3 + Y4 + Y5

# 
  M1 ~ Z + X1
  M2 ~ Z + X2
  M3 ~ Z + X3
  M4 ~ Z + X4
  M5 ~ Z + X5

  Y1 ~ Z + M1
  Y2 ~ Z + M2
  Y3 ~ Z + M3
  Y4 ~ Z + M4
  Y5 ~ Z + M5
'

fit <- growth(model, data = dsw)
summary(fit)
