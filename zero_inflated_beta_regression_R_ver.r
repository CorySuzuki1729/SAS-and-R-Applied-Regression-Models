install.packages("gamlss")
library(gamlss)
library(readr)
library(rcompanion)

transport_data = read.csv("./Example7.2Data.csv", header=T,
sep=",")

#computing new variables and making them part of original dataset
transport_data$propbiked = transport_data$nbiked/transport_data$ntrips
transport_data$faculty = ifelse(transport_data$status=="faculty", 1, 0)
transport_data$staff = ifelse(transport_data$status=="staff", 1, 0)
transport_data$male = ifelse(transport_data$gender=="M", 1, 0)

#fitting zero inflated beta regression model
fitted_model = gamlss(propbiked~faculty+staff+parking,
mu.link="logit", nu.formula=~male+distance,
nu.link="logit", data=transport_data, family=BEZI)
summary(fitted_model)

#checking model fit
intercept_only_mod = gamlss(propbiked~1, mu.link="logit",
nu.formula~1, nu.link="logit", data=transport_data,
family=BEZI)
deviance = -2*(logLik(intercept_only_mod)-logLik(fitted_model))
print(deviance)
p_value = pchisq(deviance, df=5, lower.tail=F)
print(p_value)

#using fitted model for prediction
param_pred = predictAll(fitted_model, newdata=data.frame(parking=6,
distance=3, faculty=0, staff=0, male=0), type="response")
print((1-param_pred$nu)*param_pred$mu)