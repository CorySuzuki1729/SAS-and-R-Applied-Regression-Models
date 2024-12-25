library(readr)
library(rcompanion)
library("ordinal")

health_survey = read.csv("./Example4.1Data.csv", header=T, sep=",")

#specifying reference categories

health_survey$gender = as.factor(health_survey$gender)
health_survey$marital = as.factor(health_survey$marital)
health_survey$educ = as.factor(health_survey$educ)
health_survey$health = as.factor(health_survey$health)
gender_rel = relevel(health_survey$gender, ref="M")
marital_rel = relevel(health_survey$marital, ref="yes")
educ_rel = relevel(health_survey$educ, ref="HSgrad+")

#fitting the cumulative probit model
fitted_model = clm(health~gender_rel+age+marital_rel+educ_rel,
data=health_survey, link="probit")
summary(fitted_model)

#extracting AICC and BIC for fitted model
p=8
n=32
AICC = -2*logLik(fitted_model)+2*p*n/(n-p-1)
print(AICC)
BIC(fitted_model)

#checking model fit
intercept_only_model = clm(health~1, data=health_survey,
link="probit")
deviance = -2*(logLik(intercept_only_model)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=5, lower.tail=F)
print(p_value)

#using fitted model for prediction
pred_health = predict(fitted_model, type="prob",
data.frame(gender_rel="M", age=52, marital_rel="yes",
educ_rel="HSgrad"))
print(pred_health)