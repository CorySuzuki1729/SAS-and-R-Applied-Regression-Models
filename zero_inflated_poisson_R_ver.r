install.packages("pscl")
library(readr)
library(rcompanion)
library(pscl)

smoking_data = read.csv("./Example5.3Data.csv", header=T,
sep=",")
smoking_data$health = as.factor(smoking_data$health)

#specifying reference category
health_rel = relevel(smoking_data$health, ref="good")

#fitting zero-inflated Poisson model
fitted_model = zeroinfl(cigarettes~gender+age|health_rel,
data=smoking_data)
summary(fitted_model)

#checking model fit
intercept_only = zeroinfl(cigarettes~1, data=smoking_data)
deviance = -2*(logLik(intercept_only)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=3, lower.tail=F)
print(p_value)

#using fitted model for prediction
pred_zero_poi_inf = predict(fitted_model, data.frame(gender="M",
health_rel="good", age=50))
print(pred_zero_poi_inf)