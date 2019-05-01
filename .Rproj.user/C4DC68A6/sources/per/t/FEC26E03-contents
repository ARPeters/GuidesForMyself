#Following example in:
# https://www.youtube.com/watch?v=VtWsyUCGfhg&t=16s

# ---- load-packages
library(ltm)
library(psych)
library(mirt)
library(mvtnorm)
library(msm)

# ---- load-data
ds <- LSAT


twopl_model <- ltm(LSAT ~ z1, IRT.param = TRUE)
summary(irtmodel)
coef(irtmodel)
#Intepretation:
# Difficulty (b or theta in the 2pl model) is given in terms of z scores. So item 1 and 5 look extremely easy.
# Discrimination (a) values; we'd like to see them at or above 1. 

plot(twopl_model, type = "ICC") # Plots item characteristic curves; all items at once. 
plot(twopl_model, type = "ICC", items = 3)
plot(twopl_model, type = "ICC", items = c(2,3))
plot(twopl_model, type = "IIC", items = 0) # Plots TEST information curve. This one is best at discriminating between those with a theta of -2 ish. 
plot(twopl_model, type = "IIC")            # Note: scale for information is often not easy to explain/understand.

#This one isn't working. Not sure why. 
factor.scores(twopl_model)

person.fit(twopl_model)

item.fit(twopl_model)


# ---- 3pl

threepl_model <- tpm(data = ds, type = "latent.trait", IRT.param = TRUE)
summary(threepl_model)
coef(threepl_model)
plot(threepl_model, type = "ICC")
plot(threepl_model, type = "IIC", items = 0)

factor.scores(threepl_model)

person.fit(threepl_model)
item.fit(threepl_model)

#These look to be the same model; anova doesn't come close to significance
anova(irtmodel, threepl_model)

# ---- polytomous
# And here she is using her own data that I can't access. 
