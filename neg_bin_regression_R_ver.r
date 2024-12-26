library(readr)
library(rcompanion)
library(MASS)

swim_data = read.csv("./Example6.1Data.csv", header=T, sep=",")

#specifying reference category
swim_data$firsttime = as.factor(swim_data$firsttime)
firsttime_rel = relevel(swim_data$firsttime, ref="yes")

#fitting negative binomial model
fitted_model = glm.nb(sets~gender+age+firsttime_rel, data=swim_data)
summary(fitted_model)

#checking model fit
intercept_only_model = glm.nb(sets~1, data=swim_data)
deviance = -2*(logLik(intercept_only_model)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=3, lower.tail=F)
print(p_value)

#using fitted model for prediction
swim_predict = predict(fitted_model, data.frame(gender="F",
firsttime_rel="yes"), type="response")
print(swim_predict)

