library(gamlss)
library(readr)
library(rcompanion)

medadherence_data = read.csv("./Example7.3Data.csv", header=T,
sep=",")

#computing new variable and making it part of the original dataset
medadherence_data$female = ifelse(medadherence_data$gender=="F", 1, 0)

#fitting one inflated beta regression model
fitted_model = gamlss(pdc~age+female+depression, mu.link="logit",
nu.formula=~diabetes+nmeds, nu.link="logit",
data=medadherence_data, family=BEOI)
summary(fitted_model)

#checking model fit
intercept_only_model = gamlss(pdc~1, mu.link="logit",
nu.formula=~1, nu.link="logit", data=medadherence_data,
family=BEOI)
deviance = -2*(logLik(intercept_only_model)-logLik(fitted_model))
print(deviance)
pvalue = pchisq(deviance, df=5, lower.tail=F)
print(pvalue)

#using fitted model for prediction
param_pred = predictAll(fitted_model, newdata=data.frame(age=77,
female=1, depression=0, diabetes=1, nmeds=3), type="response")
print(param_pred$nu+(1-param_pred$nu)*param_pred$mu)