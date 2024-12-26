library(readr)
library(rcompanion)
library(pscl)

weather_data = read.csv("./Example6.3Data.csv", header=T, sep=",")

#fitting zero inflated negative binomial model
fitted_model = zeroinfl(snowyears~mintemp|elevation,
data=weather_data, dist="negbin")
summary(fitted_model)

#checking model fit
intercept_only_mod = zeroinfl(snowyears~1, data=weather_data,
dist="negbin")
deviance = -2*(logLik(intercept_only_mod)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=2, lower.tail=F)
print(p_value)

#using fitted model for prediction
zero_inf_negbin_pred = predict(fitted_model, data.frame(elevation=1165.9,
mintemp=63.1))
print(zero_inf_negbin_pred)