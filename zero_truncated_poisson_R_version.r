install.packages("VGAM")
library(readr)
library(rcompanion)
library(VGAM)

hospital_stay_data = read.csv("./Example5.1Data.csv", header=T, sep=",")

#eliminating zeros from the original dataset
hospital_day_data = hospital_stay_data[which(hospital_stay_data$days!=0),]

#fitting zero-truncated Poisson model
fitted_model = vglm(days~gender+age+illness,
data=hospital_day_data, family=pospoisson())
summary(fitted_model)

#checking model fit
intercept_only_mod = vglm(days~1, data=hospital_day_data,
family=pospoisson())
deviance = -2*(logLik(intercept_only_mod)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=3, lower.tail=F)
print(p_value)

#using fitted model for prediction
zero_pois_pred = predict(fitted_model, data.frame(gender="M",
age=55, illness="no"), type="response")
print(zero_pois_pred)