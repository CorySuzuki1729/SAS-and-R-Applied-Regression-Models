library(readr)
library(rcompanion)
library(pscl)

bookstore_data = read.csv("./Example5.4Data.csv", header=T,
sep=",")

#fitting hurdle Poisson model
fitted_model = hurdle(ntextbooks~aid|renting,
data=bookstore_data, dist="poisson", zero.dist-"binomial",
link="logit")
summary(fitted_model)

#checking model fit
intercept_only_model = hurdle(ntextbooks~1, data=bookstore_data,
dist="poisson", zero.dist="binomial", link="logit")
deviance = -2*(logLik(intercept_only_model)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=2, lower.tail=F)
print(p_value)

#using fitted model for prediction
hurdle_pred = predict(fitted_model, data.frame(renting=0, aid="no"))
print(hurdle_pred)