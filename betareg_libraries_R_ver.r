install.packages("betareg")
library(betareg)
library(readr)
library(rcompanion)

libraries_data = read.csv("./Example7.1Data.csv", header=T,
sep=",")

#specifying reference category
libraries_data$location = as.factor(libraries_data$location)
location_rel = relevel(libraries_data$location, ref="rural")

#fitting beta regression model
fitted_model = betareg(propontime~nbooks+ncardholders+location_rel,
data=libraries_data, link="logit")
summary(fitted_model)

#checking model fit
intercept_only_mod = betareg(propontime~1,
data=libraries_data, link="logit")
deviance = -2*(logLik(intercept_only_mod)-logLik(fitted_model))
print(deviance)

pvalue = pchisq(deviance, df=3, lower.tail=F)
print(pvalue)

#using fitted model for prediction
betareg_pred = predict(fitted_model, data.frame(nbooks=15,
ncardholders=2.5, location_rel="rural"))
print(betareg_pred)