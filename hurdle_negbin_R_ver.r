library(readr)
library(rcompanion)
library(pscl)

credit_cards = read.csv("./Example6.4Data.csv", header=T, sep=",")

#specifying reference categories
credit_cards$gender = as.factor(credit_cards$gender)
credit_cards$income = as.factor(credit_cards$income)
gender_rel = relevel(credit_cards$gender, ref="M")
income_rel = relevel(credit_cards$income, ref="Low")

#fitting hurdle negative binomial model
fitted_model = hurdle(ndelinqaccounts~gender_rel+income_rel
+nunemplyears|age, data=credit_cards,
dist="negbin", zero.dist="binomial", link="logit")
summary(fitted_model)

#checking model fit
intercept_only_model = hurdle(ndelinqaccounts~1,
data=credit_cards, dist="negbin", zero.dist="binomial",
link="logit")
deviance = -2*(logLik(intercept_only_model)-logLik(fitted_model))
print(deviance)

pvalue = pchisq(deviance, df=4, lower.tail=F)
print(pvalue)

#using fitted model for prediction
hurdle_negbin_pred = predict(fitted_model, data.frame(age=45,
gender_rel="M", income_rel="High", nunemplyears=0))
print(hurdle_negbin_pred)