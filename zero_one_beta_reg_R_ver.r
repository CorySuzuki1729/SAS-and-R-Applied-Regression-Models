library(gamlss)
library(readr)
library(rcompanion)

assignment_data = read.csv("./Example7.4Data.csv", header=T, sep=",")

#computing new variable and making it part of original dataset
assignment_data$female = ifelse(assignment_data$gender=="F", 1, 0)

#fitting zero-one beta regression model
fitted_model = gamlss(propassign~female, mu.link="logit",
nu.formula=~mathscore, nu.link="log",
tau.formula=~grade, tau.link="log", data=assignment_data,
family=BEINF)
summary(fitted_model)

#checking model fit
intercept_only_model = gamlss(propassign~1, mu.link="logit",
nu.formula=~1, nu.link="log", tau.formula=~1, tau.link="log",
data=assignment_data, family=BEINF)
deviance = -2*(logLik(intercept_only_model)-logLik(fitted_model))
print(deviance)
p_value = pchisq(deviance, df=3, lower.tail=F)
print(p_value)

#using fitted model for prediction
param_pred = predictAll(fitted_model, newdata=data.frame(grade=8,
mathscore=101, female=0), type="response")
print((param_pred$tau+param_pred$mu)/(1+param_pred$nu+param_pred$tau))
