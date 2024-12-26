library(readr)
library(rcompanion)
library(VGAM)

hotel_data = read.csv("./Example6.2Data.csv", header=T, sep=",")

#fitting truncated negative binomial model
fitted_model = vglm(ncomplaints~floor+member+days,
data=hotel_data, family=posnegbinomial())
summary(fitted_model)

#checking model fit
intercept_only_mod = vglm(ncomplaints~1, data=hotel_data,
family=posnegbinomial())
deviance = -2*(logLik(intercept_only_mod)-logLik(fitted_model))
print(deviance)

p_value = pchisq(deviance, df=3, lower.tail=F)
print(p_value)

#using fitted model for prediction

trunc_negbin_pred = predict(fitted_model, data.frame(floor=4,
member="yes", days=2), type="response")
print(trunc_negbin_pred)