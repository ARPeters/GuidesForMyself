rm(list = ls())


# ---- load-packages ------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(AER)
library(lavaan)
library(lme4)
library(wfe)
library(plm)
library(systemfit)


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


# ----

# Fiddling around with simulated data
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
# summary(fit.ols)


# ---- TSLS ------------------------------------------------------------------------------------------------------------------------
# Two-Stage Least-Squares regression: Piecemeal
# First: regress M onto X, get predicted M values
ols_first <- lm(M ~ X, data = ds_L)
M_hat     <- fitted(ols_first)
summary(ols_first)

# Second: regress Y onto Predicted M values
ols_second <- lm(Y ~ M_hat, data = ds_L)
summary(ols_second)

# coef(ols_second)


# ---- ivreg ------------------------------------------------------------------------------------------------------------------------
# TSLS: Using ivreg() function from AER package
# In this case: Gets same results
iv_res <- ivreg(Y ~ M | X, data = ds_L)
summary(iv_res)


# ---- systemfit ------------------------------------------------------------------------------------------------------------------------
# TSLS: Using systemfit package
# In this case: Gets same results

# Recreating in systemfit
eqFirst   <- M ~ X 
eqSecond  <- Y ~ M

system <- list(First = eqFirst, Second = eqSecond)

inst <- ~ X
tsls_test <- systemfit(system, "2SLS", inst = inst, data = ds_L)

summary(tsls_test)


# ---- sem ------------------------------------------------------------------------------------------------------------------------

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


# ---- lmer-guess ------------------------------------------------------------------------------------------------------------------------
# TSLS: with lmer (which can accomodate id values)
ols_first_mixed <- lmer(M ~ X + (1|ID), data = ds_L)
M_hat_mixed     <- round(fitted(ols_first   , data = ds_L), 3)
summary(ols_first_mixed)

ds_ranef <-
  ranef(ols_first_mixed) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    grouping_var  = as.integer(as.character(grp)),
    id_effect_var = condval,
  ) %>%
  dplyr::select(grouping_var, id_effect_var)


ds_L2 <-
  ds_L %>% 
  dplyr::left_join(
    ds_ranef,
    by = c("ID" = "grouping_var")
  ) %>% 
  dplyr::mutate(
    M_hat_mixed_without_s1id = M_hat_mixed,
    M_hat_mixed_with_s1id = M_hat_mixed_without_s1id + id_effect_var
  )

# Second: regress Y onto Predicted M values
ols_second_mixed <- lmer(Y ~ M_hat_mixed_without_s1id + (1|ID), data = ds_L2)
summary(ols_second_mixed)

# ----


ds_test <-
  ds_L %>% 
  dplyr::mutate(
    time = rep(c(1:5), 1000)
  ) %>% 
  dplyr::select(ID, time, dplyr::everything())
View(head(ds_test, 40))

test <- plm(formula = Y ~ M + X | + X | X, data = ds_test, model = "random")
test <- plm(formula = Y ~ M + X | + X | X, data = ds_test, model = "random", random.method = "ht")



# Second step 2: regress Y onto predicted M values plus the random individual effect
ols_second_mixed_b <- lmer(Y ~ M_hat_mixed_with_s1id + (1|ID), data = ds_L2)
summary(ols_second_mixed_b)


# If we just added the mixed effects into second stage
ols_second_mixed_c <- lmer(Y ~ M_hat + (1|ID), data = ds_L2)
summary(ols_second_mixed_b)


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


# wfe package

### NOTE: this example illustrates the use of wfe function with randomly
### generated panel data with arbitrary number of units and time.
## generate panel data with number of units = N, number of time = Time
N <- 10 # number of distinct units
Time <- 15 # number of distinct time
## treatment effect
beta <- 1
## generate treatment variable
treat <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
## make sure at least one observation is treated for each unit
while ((sum(apply(treat, 2, mean) == 0) > 0) | (sum(apply(treat, 2, mean) == 1) > 0) |
       (sum(apply(treat, 1, mean) == 0) > 0) | (sum(apply(treat, 1, mean) == 1) > 0)) {
  treat <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
}
treat.vec <- c(treat)
## unit fixed effects
alphai <- rnorm(N, mean = apply(treat, 2, mean))
## geneate two random covariates
x1 <- matrix(rnorm(N*Time, 0.5,1), ncol=N)
x2 <- matrix(rbeta(N*Time, 5,1), ncol=N)
x1.vec <- c(x1)
x2.vec <- c(x2)
## generate outcome variable
y <- matrix(NA, ncol = N, nrow = Time)
for (i in 1:N) {
  y[, i] <- alphai[i] + treat[, i] + x1[,i] + x2[,i] + rnorm(Time)
}
y.vec <- c(y)
## generate unit and time index
unit.index <- rep(1:N, each = Time)
time.index <- rep(1:Time, N)
Data.str <- as.data.frame(cbind(y.vec, treat.vec, unit.index, x1.vec, x2.vec))
colnames(Data.str) <- c("y", "tr", "strata.id", "x1", "x2")
Data.obs <- as.data.frame(cbind(y.vec, treat.vec, unit.index, time.index, x1.vec, x2.vec))
colnames(Data.obs) <- c("y", "tr", "unit", "time", "x1", "x2")

### Average Treatment Effect for the Treated
mod.att <- wfe(y~ tr+x1+x2, data = Data.str, treat = "tr",
               unit.index = "strata.id", method = "unit",
               qoi = "att", hetero.se=TRUE, auto.se=TRUE)
## summarize the results
summary(mod.att)

# Trying it with ds_L
mod.att_L <- wfe(Y ~ M + X + Z, data = ds_L, treat = "M", unit.index = "ID", method = "unit", qoi = "att")
summary(mod.att_L)
# sem approach: Latent Growth Curve Modeling
# dsw <- 
#   ds_L %>%
#   dplyr::filter(t == 1) %>%
#   dplyr::rename(
#     Y1 = Y,
#     M1 = M,
#     X1 = X
#   ) %>%
#   dplyr::left_join(
#     ds_L %>% dplyr::filter(t == 2) %>%
#       dplyr::select(- "Z" ) %>%
#       dplyr::rename(
#         Y2 = Y,
#         M2 = M,
#         X2 = X
#       ), by = "ID"
#   ) %>%
#   dplyr::left_join(
#     ds_L %>% dplyr::filter(t == 3) %>%
#       dplyr::select(- "Z" ) %>%
#       dplyr::rename(
#         Y3 = Y,
#         M3 = M,
#         X3 = X
#       ), by = "ID"
#   ) %>%
#   dplyr::left_join(
#     ds_L %>% dplyr::filter(t == 4) %>%
#       dplyr::select(- "Z" ) %>%
#       dplyr::rename(
#         Y4 = Y,
#         M4 = M,
#         X4 = X
#       ), by = "ID"
#   ) %>%
#   dplyr::left_join(
#     ds_L %>% dplyr::filter(t == 5) %>%
#       dplyr::select(- "Z" ) %>%
#       dplyr::rename(
#         Y5 = Y,
#         M5 = M,
#         X5 = X
#       ), by = "ID"
#   ) %>%
#   dplyr::select(
#     ID, Z, Y1, Y2, Y3, Y4, Y5, X1, X2, X3, X4, X5, M1, M2, M3, M4, M5
#   )
# 
# head(dsw)
# 
# model <- '
# # Intercept and slope with fixed coefficients
#   i =~ Y1 + Y2 + Y3 + Y4 + Y5
#   s =~ Y1 + Y2 + Y3 + Y4 + Y5
# 
# # 
#   M1 ~ Z + X1
#   M2 ~ Z + X2
#   M3 ~ Z + X3
#   M4 ~ Z + X4
#   M5 ~ Z + X5
# 
#   Y1 ~ Z + M1
#   Y2 ~ Z + M2
#   Y3 ~ Z + M3
#   Y4 ~ Z + M4
#   Y5 ~ Z + M5
# '
# 
# fit <- growth(model, data = dsw)
# summary(fit)


# ---- zeligverse-approach ------------------------------------------------------------------------------------------------------------------------
# Getting sample data from here: http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf
library(quantreg)
library(lme4)
library(zeligverse)
library(dplyr) # load %>% pipe operator
library(Zelig)

# Getting Zelig multilevel optios imporrted
# devtools::install_github("IQSS/ZeligMultilevel")
# install.packages("ZeligMultilevel")
require(ZeligMultilevel)


# Zelig example:
# load data and estimate model
data(sanction)

# mil == treatment variable

zqi.out <- zelig(num ~ target + coop + mil, 
                 model = "poisson", data = sanction, cite = FALSE)

summary(zqi.out)

# find the ATT where the treatement is mil = 1
z.att <- zqi.out %>%
  ATT(treatment = "mil", treat = 1) %>% 
  get_qi(qi = "ATT", xvalue = "TE")

# summarize the results
hist(z.att, 
     main = NULL,
     xlab ="Averege Treatment Effect of mil on the Treated")



# Fiddling around:
# dsq <- 
#   sanction %>% 
#   dplyr::mutate(
#     
#   )
# head(dsq)
# 
# meq <- zelig(num ~ target + coop + mil + (1 | num),
#              model = "ls.mixed",
#              data = sanction, cite = FALSE)
# args(zelig)
# summary(meq)
# 
# 
# data(voteincome)
# 
# z5 <- zlogitmixed$new()
# z5
# z5$zelig(vote ~ education + age + female + (1 | state),
#          data = voteincome)
# z5


# Mixed Effects example from Bodo Winter
# Getting sample data from here: http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf


politeness=read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

dsq <- 
  politeness %>% 
  dplyr::mutate(
    gender = dplyr::if_else(gender == "M", 1L, 0L),
    attitude_2 = as.factor(dplyr::if_else(attitude == "pol", 1L, 0L))
  )

# politeness.model = lmer(frequency ~ attitude_2 + gender + (1|subject) + (1|scenario), data=dsq)
politeness.model = lmer(frequency ~ attitude_2 + gender + (1|subject), data=dsq)
summary(politeness.model)



iv_res <- ivreg(Y ~ M | X, data = ds_L)
summary(iv_res)



eqFirst  <- M ~ X
eqSecond <- Y ~ M

system    <- list(First = eqFirst, Second = eqSecond)
inst      <- ~ X
test_2sls <- systemfit(system, "2SLS", inst = inst, data = ds_L)
test_2sls

eqFirst  <- M ~ X
eqSecond <- Y ~ M + (1 | ID)

system    <- list(First = eqFirst, Second = eqSecond)
inst      <- ~ X
test_2sls <- systemfit(system, "2SLS", inst = inst, data = ds_L)
test_2sls



data("KleinI")
View(head(KleinI, 40))
data( "Kmenta" )
View(head(Kmenta, 40))
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list( demand = eqDemand, supply = eqSupply )

## 2SLS estimation
inst <- ~ income + farmPrice + trend
fit2sls <- systemfit( system, "2SLS", inst = inst, data = Kmenta )
print( fit2sls )







# Importing Bodo Winter example into Zelig to get ATT ( I hope, eventually)

# replicating (hopefully) in Zelig
zme <- zelig(frequency ~ attitude_2 + (1|subject) + (1|scenario), data=dsq, model = "ls.mixed")
summary(zme)


# find the ATT where the treatement is att (pretend treatment variable) = 1
z.att <- zme %>%
  ATT(treatment = "attitude_2", treat = 1)


ATT(zme, treatment = "gender", treat = 1)


# https://stats.stackexchange.com/questions/159997/2sls-for-panel-data-in-r


## replicates Baltagi (2005, 2013), table 7.4
## preferred way with plm()
data("Wages", package = "plm")

ht <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) +
            bluecol + ind + union + sex + black + ed |
            bluecol + south + smsa + ind + sex + black |
            wks + married + union + exp + I(exp ^ 2),
          data = Wages, index = 595,
          random.method = "ht", model = "random", inst.method = "baltagi")
summary(ht)
View(head(Wages, 40))
# deprecated way with pht() for HT
ht <- pht(lwage ~ wks + south + smsa + married + exp + I(exp^2) +
 bluecol + ind + union + sex + black + ed |
 sex + black + bluecol + south + smsa + ind,
 data = Wages, model = "ht", index = 595)
summary(ht)

# systemfit
library(systemfit)


data( "Kmenta" )
View(head(Kmenta, 40))
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list( demand = eqDemand, supply = eqSupply )

## 2SLS estimation
inst <- ~ income + farmPrice + trend
fit2sls <- systemfit( system, "2SLS", inst = inst, data = Kmenta )
print( fit2sls )


data("Grunfeld", package = "plm")
View(head(Grunfeld, 40))


eqFirst  <- value ~ firm
eqSecond <- inv ~ value + (1 | year)
system   <- list(First = eqFirst, Second = eqSecond)
inst     <- ~ firm 
fit_test <- systemfit(system, "2SLS", inst = inst, data = Grunfeld)
fit_test

iv_res <- ivreg(inv ~ value | firm, data = Grunfeld)
summary(iv_res)
