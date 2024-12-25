library(readr)
library(rcompanion)

hospital_data = read.csv("./Example5.1Data.csv", header=T, sep=",")

#fitting Poisson regression model
fitted_model = glm(days~gender+age+illness,
data=hospital_data, family=poisson(link=log))
summary(fitted_model)

#checking model fit
intercept_only_model = glm(days~1, data=hospital_data,
family=poisson(link=log))
deviance = -2*(logLik(intercept_only_model)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=3, lower.tail=F)
print(p_value)

#using fitted model for prediction
poisson_predict = predict(fitted_model, data.frame(gender="M", age=55,
illness="no"), type="response")
print(poisson_predict)